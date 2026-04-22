{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | A Tasty 'Ingredient' that skips tests whose source has not changed since
-- the last passing run, using GHC HIE files for fine-grained dependency
-- tracking.
--
-- Caching is __opt-in__: only tests wrapped with 'cacheable' are ever skipped.
-- Unwrapped tests run unconditionally on every invocation.
--
-- = How it works
--
-- Each 'cacheable' test is assigned a fingerprint:
--
-- @
-- fingerprint = hash(body_hash, dep_hash, cabal_hash)
-- body_hash   = hash of the testCase expression's source bytes
-- dep_hash    = hash of the source bytes of every declaration transitively
--               reachable from the test body via the HIE identifier graph
-- cabal_hash  = hash of all .cabal files in the project root
-- @
--
-- On each run fingerprints are compared against a cache
-- (@.cache\/hie-tasty-cache@).  Tests with matching fingerprints are
-- replaced with an instant-pass placeholder; only stale tests execute.
-- The cache is updated per-test as each passing test completes.
--
-- = Requirements
--
-- Both the library and test-suite stanzas must emit HIE files:
--
-- @
-- ghc-options: -fwrite-ide-info -hiedir .hie
-- @
--
-- Requires GHC >= 9.8.
--
-- = Usage
--
-- @
-- import Test.Tasty.HieCache (defaultMainWithHieCache, cacheable)
--
-- main :: IO ()
-- main = defaultMainWithHieCache tests
--
-- tests :: TestTree
-- tests = testGroup "all"
--   [ cacheable $ testGroup "pure" [...]   -- skipped when unchanged
--   , testGroup "integration" [...]        -- always runs
--   ]
-- @
module Test.Tasty.HieCache
  ( -- * Main entry point
    defaultMainWithHieCache
    -- * Ingredient
  , hieCacheIngredient
    -- * Opt-in
  , cacheable
  ) where

import           Control.Exception         (SomeException, catch, evaluate, try)
import           Control.Monad             (when)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.ByteString.Unsafe    (unsafeUseAsCStringLen)
import           Data.IORef                (IORef, modifyIORef', newIORef,
                                            readIORef)
import           Data.List                 (intercalate, sort)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (listToMaybe)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word64)
import           Foreign.Ptr               (castPtr)
import           GHC.Fingerprint           (Fingerprint (..), fingerprintData,
                                            fingerprintFingerprints)
import           System.Directory          (createDirectoryIfMissing,
                                            doesDirectoryExist, doesFileExist,
                                            listDirectory)
import           System.FilePath           (takeExtension, (</>))
import           System.IO                 (hPutStrLn, stderr)
import           System.IO.Unsafe          (unsafePerformIO)

import           Test.Tasty
import           Test.Tasty.Options        (IsOption (..), OptionSet,
                                            lookupOption, safeRead)
import           Test.Tasty.Providers      (IsTest (..), testPassed)
import           Test.Tasty.Runners

import qualified GHC.Iface.Ext.Binary      as HIE
import qualified GHC.Iface.Ext.Types       as HIE
import           GHC.Types.Name            (nameOccName)
import           GHC.Types.Name.Cache      (NameCache, initNameCache)
import           GHC.Types.Name.Occurrence (occNameString)
import           GHC.Types.SrcLoc          (RealSrcSpan, srcSpanEndLine,
                                            srcSpanStartLine)

-- ---------------------------------------------------------------------------
-- Public API

-- | Drop-in replacement for 'defaultMain'.
-- Prepends 'hieCacheIngredient' to 'defaultIngredients' so that unchanged
-- tests are skipped automatically.
defaultMainWithHieCache :: TestTree -> IO ()
defaultMainWithHieCache =
  defaultMainWithIngredients
    (hieCacheIngredient defaultIngredients : defaultIngredients)

-- | Mark a 'TestTree' as eligible for HIE-based caching.
-- Tests not wrapped with 'cacheable' always run unconditionally.
--
-- @
-- tests = testGroup "all"
--   [ cacheable $ testGroup "pure" [...]   -- skipped when unchanged
--   , testGroup "integration" [...]        -- always runs
--   ]
-- @
cacheable :: TestTree -> TestTree
cacheable = localOption (HieCacheEnabled True)

-- Internal option: set by 'cacheable', checked per-test in the ingredient.
newtype HieCacheEnabled = HieCacheEnabled Bool deriving (Eq, Ord, Typeable)

instance IsOption HieCacheEnabled where
  defaultValue   = HieCacheEnabled False
  parseValue s   = HieCacheEnabled <$> safeRead s
  optionName     = return "hie-cache"
  optionHelp     = return "Enable HIE-based caching for this test subtree"

-- ---------------------------------------------------------------------------
-- Ingredient

-- | The caching ingredient.  Wraps a list of sub-ingredients (typically
-- 'defaultIngredients') and intercepts test execution to serve cached results.
-- Use this directly if you need to compose it with other custom ingredients.
hieCacheIngredient :: [Ingredient] -> Ingredient
hieCacheIngredient subIngredients = TestManager [] $ \opts tree -> Just $ do
  let hieDir    = ".hie"
      cachePath = ".cache" </> "hie-tasty-cache"

  hieExists <- doesDirectoryExist hieDir
  if not hieExists
    then do
      hPutStrLn stderr "HieCache: no .hie directory, running all tests"
      runAll subIngredients opts tree
    else do
      let paths = collectPaths tree

      fps <- computeFingerprints hieDir paths
               `catch` \(e :: SomeException) -> do
                 hPutStrLn stderr $ "HieCache: fingerprint error: " ++ show e
                 return Map.empty

      cache <- loadCache cachePath

      let stale = Set.fromList
            [ p | p <- paths
            , Map.lookup (pathKey p) fps /= Map.lookup (pathKey p) cache ]

      let freshCount = length paths - Set.size stale
      when (freshCount > 0) $
        hPutStrLn stderr $
          "HieCache: skipping " ++ show freshCount ++ " cached test(s)"

      cacheRef <- newIORef cache
      let wrapped = wrapAndFilterTree stale [] mempty cacheRef fps tree

      result <- runAll subIngredients opts wrapped

      finalCache <- readIORef cacheRef
      createDirectoryIfMissing True ".cache"
      saveCache cachePath finalCache
      return result
  where
    runAll ingredients opts t =
      case tryIngredients ingredients opts t of
        Nothing     -> return True
        Just action -> action

-- ---------------------------------------------------------------------------
-- Test-tree transformation

type TestPath = [TestName]

-- | Collect only the paths of tests marked with 'cacheable'.
collectPaths :: TestTree -> [TestPath]
collectPaths = go [] mempty
  where
    go prefix opts = \case
      SingleTest name _ ->
        let HieCacheEnabled enabled = lookupOption opts
        in [ reverse (name : prefix) | enabled ]
      TestGroup  name cs  -> concatMap (go (name : prefix) opts) cs
      PlusTestOptions f t -> go prefix (f opts) t
      WithResource _ f    -> go prefix opts (f (return (error "HieCache: WithResource")))
      AskOptions  f       -> go prefix opts (f opts)
      After _ _ t         -> go prefix opts t

wrapAndFilterTree
  :: Set TestPath
  -> TestPath
  -> OptionSet
  -> IORef (Map String Fingerprint)
  -> Map String Fingerprint
  -> TestTree
  -> TestTree
wrapAndFilterTree stale prefix opts cacheRef fps = \case
  SingleTest name t ->
    let fullPath = reverse (name : prefix)
        key      = pathKey fullPath
        HieCacheEnabled enabled = lookupOption opts
    in if not enabled
       then SingleTest name t
       else if Set.member fullPath stale
            then case Map.lookup key fps of
                   Just fp -> SingleTest name (CachingTest key fp cacheRef t)
                   Nothing -> SingleTest name t
            else SingleTest name CachedTest
  TestGroup name cs ->
    TestGroup name (map (wrapAndFilterTree stale (name : prefix) opts cacheRef fps) cs)
  PlusTestOptions f t ->
    PlusTestOptions f (wrapAndFilterTree stale prefix (f opts) cacheRef fps t)
  WithResource spec f ->
    WithResource spec (wrapAndFilterTree stale prefix opts cacheRef fps . f)
  AskOptions f ->
    AskOptions (\o -> wrapAndFilterTree stale prefix (opts <> o) cacheRef fps (f o))
  After dt expr t ->
    After dt expr (wrapAndFilterTree stale prefix opts cacheRef fps t)

-- | Wraps a real test: records its fingerprint in @cacheRef@ on pass.
data CachingTest t = CachingTest String Fingerprint (IORef (Map String Fingerprint)) t

instance (Typeable t, IsTest t) => IsTest (CachingTest t) where
  run opts (CachingTest key fp cacheRef t) progress = do
    result <- run opts t progress
    case resultOutcome result of
      Success   -> modifyIORef' cacheRef (Map.insert key fp)
      Failure _ -> return ()
    return result
  testOptions = return []

-- | Always passes instantly — replaces fresh (cached) tests.
data CachedTest = CachedTest
instance IsTest CachedTest where
  run _ CachedTest _ = return (testPassed "") { resultShortDescription = "OK (cached)" }
  testOptions = return []

-- ---------------------------------------------------------------------------
-- HIE data

data HieData = HieData
  { hdSrc :: BS.ByteString
  , hdAst :: HIE.HieAST HIE.TypeIndex
  }

readHieData :: NameCache -> FilePath -> IO (Maybe HieData)
readHieData nc path = do
  r <- try (HIE.readHieFile nc path) :: IO (Either SomeException HIE.HieFileResult)
  case r of
    Left  _  -> return Nothing
    Right hr -> do
      let hf  = HIE.hie_file_result hr
          src = HIE.hie_hs_src hf
      src' <- evaluate src
      case Map.elems (HIE.getAsts (HIE.hie_asts hf)) of
        []      -> return Nothing
        (ast:_) -> return (Just (HieData src' ast))

-- ---------------------------------------------------------------------------
-- Fingerprinting

-- | Per-declaration info: the source bytes for each clause/equation, plus the
-- set of Haskell names that appear as 'Use' within the declaration's body.
-- Used during the transitive-dep BFS.
data DeclData = DeclData
  { ddChunks    :: [[BS.ByteString]]  -- source bytes, one inner list per clause
  , ddUsedNames :: Set String          -- names used (not bound) in the body
  }

computeFingerprints
  :: FilePath
  -> [TestPath]
  -> IO (Map String Fingerprint)
computeFingerprints hieDir paths = do
  hieFiles <- sort . filter ((== ".hie") . takeExtension)
                <$> listDirectory hieDir

  nc <- initNameCache 'z' []
  rawData <- mapM (\f -> (f,) <$> readHieData nc (hieDir </> f)) hieFiles
  let allData = [ (f, d) | (f, Just d) <- rawData ]

  let declMaps = Map.fromList [ (f, buildDeclMap d) | (f, d) <- allData ]
  -- Raw source per file — needed for CPP whole-file hashing
  let fileSrcs  = Map.fromList [ (f, hdSrc d) | (f, d) <- allData ]

  -- Hash all .cabal files so that default-extensions changes invalidate the cache
  cabalFiles <- sort . filter ((== ".cabal") . takeExtension) <$> listDirectory "."
  cabalHash  <- hashBytesList =<< mapM BS.readFile cabalFiles

  let leafMap = Map.fromListWith const
        [ (last p, p) | p <- paths, not (null p) ]

  return $ Map.fromList
    [ (pathKey path, fp)
    | (leafName, path) <- Map.toList leafMap
    , Just fp          <- [computeTestFP allData declMaps fileSrcs cabalHash leafName]
    ]

computeTestFP
  :: [(FilePath, HieData)]
  -> Map FilePath (Map String DeclData)
  -> Map FilePath BS.ByteString   -- raw source per file (for CPP handling)
  -> Fingerprint                   -- hash of all .cabal files
  -> String
  -> Maybe Fingerprint
computeTestFP allData declMaps fileSrcs cabalHash leafName = do
  let quoted = BSC.pack ('"' : leafName ++ "\"")

  -- Find which HIE file contains the test name string
  (testFile, testData, offset) <- listToMaybe
    [ (f, d, off)
    | (f, d) <- allData
    , Just off <- [findSubstring quoted (hdSrc d)]
    ]

  -- Source bytes of the testCase expression
  let src       = hdSrc testData
      lineStart = findLineStart src offset
      baseInd   = countIndent src lineStart
      exprEnd   = findExprEnd src lineStart baseInd
      bodyBytes = BS.take (exprEnd - lineStart) (BS.drop lineStart src)
      bodyHash  = unsafeHashBytes bodyBytes

  -- Line range of the test body (for AST walking)
  let startLine = 1 + BSC.count '\n' (BS.take lineStart src)
      endLine   = startLine + max 0 (BSC.count '\n' bodyBytes - 1)

  -- Names used within the test body (from HIE identifier graph)
  let usedNames = getUsedNames (hdAst testData) startLine endLine

  -- Transitively collect dependency source bytes via BFS over the HIE graph
  let depChunks = transitiveDeps declMaps fileSrcs testFile usedNames

  let depHash = unsafePerformIO $ hashBytesList (sort depChunks)

  return (fingerprintFingerprints [bodyHash, depHash, cabalHash])

-- | BFS over the HIE identifier graph, collecting source bytes for every
-- declaration reachable from @startNames@ in files other than @testFile@.
--
-- For files that use CPP the /entire/ file source is included (as one chunk)
-- instead of individual declaration spans, because CPP macro names are
-- invisible to the HIE graph.
transitiveDeps
  :: Map FilePath (Map String DeclData)
  -> Map FilePath BS.ByteString   -- raw file source
  -> FilePath                      -- test file to exclude
  -> Set String                    -- names used directly in the test body
  -> [BS.ByteString]
transitiveDeps declMaps fileSrcs testFile startNames =
    declChunks ++ cppChunks ++ pragmaChunks
  where
    allPairs = bfs Set.empty Set.empty (Set.toList startNames)

    -- Declaration-specific source bytes for non-CPP files
    declChunks =
      [ chunk
      | (f, name) <- Set.toList allPairs
      , Just dm   <- [Map.lookup f declMaps]
      , Just dd   <- [Map.lookup name dm]
      , chunk     <- concat (ddChunks dd)
      ]

    -- Whole-file source for any CPP file we touched (deduplicated per file)
    cppChunks =
      [ src
      | f   <- Set.toList (Set.map fst allPairs)
      , Just src <- [Map.lookup f fileSrcs]
      , usesCPP src
      ]

    -- Pragma lines from each non-CPP visited file (language extensions etc.)
    -- Changing {-# LANGUAGE X #-} in a dep file must invalidate its tests.
    pragmaChunks =
      [ BSC.unlines pragmaLines
      | f   <- Set.toList (Set.map fst allPairs)
      , Just src <- [Map.lookup f fileSrcs]
      , not (usesCPP src)   -- CPP files already hash the whole source
      , let pragmaLines = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      , not (null pragmaLines)
      ]

    usesCPP src = BSC.pack "#define " `BSC.isInfixOf` src

    -- BFS: visitedNames prevents re-processing; allPairs accumulates results
    bfs pairs _ [] = pairs
    bfs pairs visitedNames (name : queue)
      | Set.member name visitedNames = bfs pairs visitedNames queue
      | otherwise =
          let newPairs = Set.fromList
                [ (f, name)
                | (f, dm) <- Map.toList declMaps
                , f /= testFile
                , Map.member name dm
                ]
              visitedNames' = Set.insert name visitedNames
              newNames =
                [ n
                | (f, _) <- Set.toList newPairs
                , Just dm <- [Map.lookup f declMaps]
                , Just dd <- [Map.lookup name dm]
                , n       <- Set.toList (ddUsedNames dd)
                , not (Set.member n visitedNames')
                ]
          in bfs (Set.union pairs newPairs) visitedNames'
               (queue ++ filter (`Set.notMember` visitedNames') newNames)

-- ---------------------------------------------------------------------------
-- HIE AST analysis

-- | Build a map from occName to 'DeclData' for every top-level binding in
-- the AST.  Each equation/clause contributes one entry; entries for the same
-- name are merged (source chunks concatenated, used-name sets unioned).
buildDeclMap :: HieData -> Map String DeclData
buildDeclMap (HieData src ast) =
  Map.fromListWith mergeDeclData (concatMap nodeEntries (flatNodes ast))
  where
    flatNodes n = n : concatMap flatNodes (HIE.nodeChildren n)

    nodeEntries node =
      [ (occNameString (nameOccName name),
         DeclData [[extractSpanLines src nodeSpan]] (usedInBody nodeSpan))
      | (Right name, details) <- concatMap (Map.toList . HIE.nodeIdentifiers)
                                            (Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo node)))
      , any isDeclCtx (Set.toList (HIE.identInfo details))
      , let nodeSpan = HIE.nodeSpan node
      ]

    -- Collect names *used* (not bound) within the equation starting at this span.
    -- We extend the span to the end of the equation using the same indentation
    -- heuristic used for test bodies.
    usedInBody nodeSpan =
      let startLine = srcSpanStartLine nodeSpan
          offset    = lineStartOffset src startLine
          baseInd   = countIndent src offset
          exprEnd   = findExprEnd src offset baseInd
          bodyBytes = BS.take (exprEnd - offset) (BS.drop offset src)
          endLine   = startLine + max 0 (BSC.count '\n' bodyBytes - 1)
      in getUsedNames ast startLine endLine


    mergeDeclData (DeclData c1 u1) (DeclData c2 u2) =
      DeclData (c1 ++ c2) (Set.union u1 u2)

    isDeclCtx :: HIE.ContextInfo -> Bool
    isDeclCtx = \case
      HIE.ValBind {} -> True
      HIE.MatchBind  -> True
      HIE.Decl {}    -> True
      _              -> False

-- | Collect the occurrence-name strings of all identifiers /used/ (not bound)
-- within source lines @startLine..endLine@ in the AST.
getUsedNames :: HIE.HieAST HIE.TypeIndex -> Int -> Int -> Set String
getUsedNames root startLine endLine = go root
  where
    go node
      | srcSpanEndLine   (HIE.nodeSpan node) < startLine = Set.empty
      | srcSpanStartLine (HIE.nodeSpan node) > endLine   = Set.empty
      | otherwise =
          let mine  = foldMap nodeUses
                        (Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo node)))
              below = foldMap go (HIE.nodeChildren node)
          in Set.union mine below

    nodeUses ni = Set.fromList
      [ occNameString (nameOccName name)
      | (Right name, details) <- Map.toList (HIE.nodeIdentifiers ni)
      , HIE.Use `Set.member` HIE.identInfo details
      ]

-- | Extract the source bytes covering lines @startLine..endLine@ (1-indexed).
extractSpanLines :: BS.ByteString -> RealSrcSpan -> BS.ByteString
extractSpanLines src nodeSpan =
  extractLineRange src (srcSpanStartLine nodeSpan) (srcSpanEndLine nodeSpan)

extractLineRange :: BS.ByteString -> Int -> Int -> BS.ByteString
extractLineRange src startLine endLine =
  let ls        = BSC.split '\n' src
      relevant  = take (endLine - startLine + 1) (drop (startLine - 1) ls)
  in  BSC.unlines relevant

-- ---------------------------------------------------------------------------
-- Source navigation

-- | Byte offset of the start of line @n@ (1-indexed).
lineStartOffset :: BS.ByteString -> Int -> Int
lineStartOffset src n = go 0 1
  where
    nl = fromIntegral (fromEnum '\n')
    go pos line
      | line >= n         = pos
      | pos >= BS.length src = pos
      | BS.index src pos == nl = go (pos + 1) (line + 1)
      | otherwise              = go (pos + 1) line

findLineStart :: BS.ByteString -> Int -> Int
findLineStart src offset = go (offset - 1)
  where
    nl = fromIntegral (fromEnum '\n')
    go i
      | i < 0                = 0
      | BS.index src i == nl = i + 1
      | otherwise            = go (i - 1)

findExprEnd :: BS.ByteString -> Int -> Int -> Int
findExprEnd src lineStart baseIndent = go (skipLine src lineStart)
  where
    go pos
      | pos >= BS.length src               = BS.length src
      | countIndent src pos > baseIndent   = go (skipLine src pos)
      | otherwise                          = pos

countIndent :: BS.ByteString -> Int -> Int
countIndent src pos = go pos 0
  where
    sp = fromIntegral (fromEnum ' ')
    tb = fromIntegral (fromEnum '\t')
    nl = fromIntegral (fromEnum '\n')
    go i n
      | i >= BS.length src           = n
      | BS.index src i == sp         = go (i + 1) (n + 1)
      | BS.index src i == tb         = go (i + 1) (n + 8)
      | BS.index src i == nl         = 0
      | otherwise                    = n

skipLine :: BS.ByteString -> Int -> Int
skipLine src = go
  where
    nl = fromIntegral (fromEnum '\n')
    go i
      | i >= BS.length src       = BS.length src
      | BS.index src i == nl     = i + 1
      | otherwise                = go (i + 1)

findSubstring :: BS.ByteString -> BS.ByteString -> Maybe Int
findSubstring needle haystack
  | BS.null needle = Just 0
  | otherwise      = go 0
  where
    nlen = BS.length needle
    hlen = BS.length haystack
    go i
      | i + nlen > hlen                             = Nothing
      | BS.take nlen (BS.drop i haystack) == needle = Just i
      | otherwise                                   = go (i + 1)

-- ---------------------------------------------------------------------------
-- Hashing

unsafeHashBytes :: BS.ByteString -> Fingerprint
unsafeHashBytes bs
  | BS.null bs = Fingerprint 0 0
  | otherwise  = unsafePerformIO $
      unsafeUseAsCStringLen bs $ \(ptr, len) ->
        fingerprintData (castPtr ptr) len

hashBytesList :: [BS.ByteString] -> IO Fingerprint
hashBytesList [] = return (Fingerprint 0 0)
hashBytesList bs = do
  fps <- mapM (\b -> unsafeUseAsCStringLen b $ \(p, l) -> fingerprintData (castPtr p) l) bs
  return (fingerprintFingerprints fps)

-- ---------------------------------------------------------------------------
-- Cache persistence

pathKey :: TestPath -> String
pathKey = intercalate " > "

type CacheMap = Map String (Word64, Word64)

fpToTuple :: Fingerprint -> (Word64, Word64)
fpToTuple (Fingerprint a b) = (a, b)

tupleToFp :: (Word64, Word64) -> Fingerprint
tupleToFp (a, b) = Fingerprint a b

loadCache :: FilePath -> IO (Map String Fingerprint)
loadCache path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else do
      bs <- BS.readFile path
      let content = BSC.unpack bs
      case reads content :: [(CacheMap, String)] of
        [(m, _)] -> return (Map.map tupleToFp m)
        _        -> return Map.empty

saveCache :: FilePath -> Map String Fingerprint -> IO ()
saveCache path m = writeFile path (show (Map.map fpToTuple m))

