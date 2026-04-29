{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-|
Module      : Test.Tasty.HieCache
Description : Tasty ingredient that skips unchanged tests using GHC HIE files
Stability   : experimental
Portability : non-portable (requires GHC >= 9.4 and HIE files)

A Tasty 'Ingredient' that skips tests whose source has not changed since
the last passing run, using GHC HIE files for fine-grained dependency
tracking.

Caching is __opt-in__: only tests wrapped with 'cacheable' are ever skipped.
Unwrapped tests run unconditionally on every invocation.

= How it works

Each 'cacheable' test is assigned a fingerprint:

@
fingerprint = hash(body_hash, dep_hash, cabal_hash)
body_hash   = hash of the testCase expression's source bytes
dep_hash    = hash of the source bytes of every declaration transitively
              reachable from the test body via the HIE identifier graph
cabal_hash  = hash of all .cabal files in the project root
@

On each run fingerprints are compared against a cache
(@.cache\/hie-tasty-cache@).  Tests with matching fingerprints are
replaced with an instant-pass placeholder; only stale tests execute.
The cache is updated per-test as each passing test completes.

= Requirements

Both the library and test-suite stanzas must emit HIE files:

@
ghc-options: -fwrite-ide-info -hiedir .hie
@

Requires GHC >= 9.4 (tested on 9.4, 9.6, 9.8, 9.10, 9.12, 9.14).

= Usage

@
import Test.Tasty.HieCache (defaultMainWithHieCache, cacheable)

main :: IO ()
main = defaultMainWithHieCache tests

tests :: TestTree
tests = testGroup \"all\"
  [ cacheable $ testGroup \"pure\" [...]   -- skipped when unchanged
  , testGroup \"integration\" [...]        -- always runs
  ]
@
-}
module Test.Tasty.HieCache
  ( -- * Main entry point
    defaultMainWithHieCache
    -- * Ingredient
  , hieCacheIngredient
    -- * Opt-in
  , cacheable
    -- * Internal (not part of the stable API; exposed for testing)
  , internalComputeFingerprint
  ) where

import           Control.Exception            (SomeException, catch, evaluate,
                                               try)
import           Control.Monad                (when)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC
import           Data.ByteString.Unsafe       (unsafeUseAsCStringLen)
import           Data.IORef                   (IORef, modifyIORef', newIORef,
                                               readIORef)
import           Data.List                    (sort)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (listToMaybe)
import           Data.Proxy                   (Proxy (..))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Typeable                (Typeable)
import           Data.Word                    (Word64)
import           Foreign.Ptr                  (castPtr)
import           GHC.Fingerprint              (Fingerprint (..),
                                               fingerprintData,
                                               fingerprintFingerprints)
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               doesFileExist, listDirectory)
import           System.FilePath              (takeExtension, (</>))
import           System.IO                    (hPutStrLn, stderr)
import           System.IO.Unsafe             (unsafePerformIO)

import           Test.Tasty
import           Test.Tasty.HieCache.Internal (countIndent, findExprEnd,
                                               findLineStart, findSubstring,
                                               lineStartOffset, pathKey)
import           Test.Tasty.Options           (IsOption (..),
                                               OptionDescription (..),
                                               OptionSet, lookupOption,
                                               mkFlagCLParser, safeRead)
import           Test.Tasty.Providers         (IsTest (..), testPassed)
import           Test.Tasty.Runners

import qualified GHC.Iface.Ext.Binary         as HIE
import qualified GHC.Iface.Ext.Types          as HIE
import           GHC.Types.Name               (Name, nameOccName, nameUnique)
import           GHC.Types.Name.Cache         (NameCache, initNameCache)
import           GHC.Types.Name.Occurrence    (occNameString)
import           GHC.Types.SrcLoc             (RealSrcSpan, srcSpanEndLine,
                                               srcSpanStartLine)
import           GHC.Types.Unique             (getKey)

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

-- | CLI flag: pass @--disable-tasty-cache@ to disable all caching.
newtype HieCacheDisable = HieCacheDisable Bool deriving (Eq, Ord, Typeable)

instance IsOption HieCacheDisable where
  defaultValue   = HieCacheDisable False
  parseValue s   = HieCacheDisable <$> safeRead s
  optionName     = return "disable-tasty-cache"
  optionHelp     = return "Disable HIE-based caching entirely (run every test regardless of cacheable)"
  optionCLParser = mkFlagCLParser mempty (HieCacheDisable True)

-- ---------------------------------------------------------------------------
-- Ingredient

-- | The caching ingredient.  Wraps a list of sub-ingredients (typically
-- 'defaultIngredients') and intercepts test execution to serve cached results.
-- Use this directly if you need to compose it with other custom ingredients.
hieCacheIngredient :: [Ingredient] -> Ingredient
hieCacheIngredient subIngredients = TestManager [Option (Proxy :: Proxy HieCacheDisable)] $ \opts tree -> Just $ do
  let HieCacheDisable disabled = lookupOption opts
  let hieDir    = ".hie"
      cachePath = ".cache" </> "hie-tasty-cache"

  if disabled
    then do
      hPutStrLn stderr "HieCache: caching disabled, running all tests"
      runAll subIngredients opts tree
    else do
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
                , let key = pathKey p
                , case Map.lookup key fps of
                    Nothing -> True                            -- can't fingerprint → must run
                    Just fp -> Map.lookup key cache /= Just fp -- fingerprint changed → stale
                ]

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

-- Per-declaration info: the source bytes for each clause/equation, the set
-- of Haskell names that appear as @Use@ within the declaration's body, and
-- the set of type-class names whose evidence is used in the body (which
-- triggers the class-edge BFS — see 'transitiveDeps').
data DeclData = DeclData
  { ddChunks      :: [[BS.ByteString]] -- source bytes, one inner list per clause
  , ddUsedNames   :: Set String        -- names used (not bound) in the body
  , ddUsedClasses :: Set String        -- classes used via evidence in the body
  }

-- | Per-file index from class name to the set of occurrence names of
-- bindings declared inside instances of that class in that file.  Built
-- from @Decl InstDec _@ bindings + @EvidenceVarBind (EvInstBind ...)@
-- containers.
type ClassIndex = Map String (Set String)

-- | Description of a single evidence-variable binding.  We need to
-- distinguish two flavours because what GHC writes into HIE files at
-- call sites is /not/ a direct reference to a top-level instance dict
-- (@$fNumFoo@).  Instead, GHC introduces a fresh local alias
-- (@$dNum@) via @EvLetBind@ whose @EvBindDeps@ point to the actual
-- @$fNumFoo@ — sometimes through several hops of further @EvLetBind@s.
--
-- Resolving an @EvidenceVarUse@ to a class therefore means following
-- the @EvLet@ chain until we reach an @EvInst@.
data EvBind
  = EvInst !String     -- ^ @EvInstBind { cls = c }@: the dictionary
                       --   provides class @c@ directly.
  | EvLet  ![Word64]   -- ^ @EvLetBind deps@: aliasing binding, may
                       --   chain to other 'EvBind' entries via @deps@.
  deriving (Eq, Show)

-- | Global index keyed by 'Unique' (extracted as 'Word64', the underlying
-- representation since GHC 9.10).  Populated from every
-- @EvidenceVarBind@ across every HIE file.
type EvBindIndex = Map Word64 EvBind

uniqWord :: Name -> Word64
uniqWord = getKey . nameUnique

-- | Walk the 'EvBind' chain starting at @uniq@ to recover the class
-- whose dictionary the bound evidence variable ultimately provides.
-- Cycles are guarded against with @visited@.
resolveEvClass :: EvBindIndex -> Word64 -> Maybe String
resolveEvClass idx = go Set.empty
  where
    go visited u
      | Set.member u visited = Nothing
      | otherwise =
          case Map.lookup u idx of
            Just (EvInst c)  -> Just c
            Just (EvLet ds) ->
              listToMaybe
                [ c
                | d <- ds
                , Just c <- [go (Set.insert u visited) d]
                ]
            Nothing -> Nothing

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

      -- Phase 1: collect every @EvidenceVarBind (EvInstBind cls)@ across
      -- all files into a global Name → class map.  Done first so that
      -- per-file decl building (Phase 2) can recognise cross-module
      -- evidence uses.
      globalEvBinds = Map.unions [ collectEvBinds d | (_, d) <- allData ]

      -- Phase 2: per-file decl map + class index, using the global
      -- ev-bind map to compute 'ddUsedClasses'.
      indices = [ (f, buildFileIndex globalEvBinds d) | (f, d) <- allData ]
      declMaps     = Map.fromList [ (f, dm) | (f, (dm, _)) <- indices ]
      classIndices = Map.fromList [ (f, ci) | (f, (_, ci)) <- indices ]
      fileSrcs     = Map.fromList [ (f, hdSrc d) | (f, d) <- allData ]

  -- Hash all .cabal files so that default-extensions changes invalidate the cache
  cabalFiles <- sort . filter ((== ".cabal") . takeExtension) <$> listDirectory "."
  cabalHash  <- hashBytesList =<< mapM BS.readFile cabalFiles

  let leafMap = Map.fromListWith const
        [ (last p, p) | p <- paths, not (null p) ]

  return $ Map.fromList
    [ (pathKey path, fp)
    | (leafName, path) <- Map.toList leafMap
    , Just fp          <- [computeTestFP allData globalEvBinds declMaps
                                          classIndices fileSrcs cabalHash
                                          leafName]
    ]

computeTestFP
  :: [(FilePath, HieData)]
  -> EvBindIndex
  -> Map FilePath (Map String DeclData)
  -> Map FilePath ClassIndex
  -> Map FilePath BS.ByteString   -- raw source per file (for CPP handling)
  -> Fingerprint                   -- hash of all .cabal files
  -> String
  -> Maybe Fingerprint
computeTestFP allData globalEvBinds declMaps classIndices fileSrcs cabalHash leafName = do
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

  -- Names and classes used within the test body
  let usedNames   = getUsedNames   (hdAst testData) startLine endLine
      usedClasses = getUsedClasses (hdAst testData) globalEvBinds startLine endLine

  -- Transitively collect dependency source bytes via BFS over the HIE graph
  let depChunks = transitiveDeps declMaps classIndices fileSrcs testFile
                                  usedNames usedClasses
      depHash   = unsafePerformIO $ hashBytesList (sort depChunks)

  return (fingerprintFingerprints [bodyHash, depHash, cabalHash])

-- | BFS over the HIE identifier graph, collecting source bytes for every
-- declaration reachable from @startNames@ in files other than @testFile@.
--
-- The BFS has two kinds of edges:
--
-- * /Name edges./ Visiting a binding @n@ pulls in @n@'s source bytes and
--   adds the names referenced inside @n@'s body ('ddUsedNames') to the
--   queue.
--
-- * /Class edges./ When a binding (or the test body) uses overloaded
--   identifiers, its 'ddUsedClasses' / @startClasses@ records the
--   classes whose dictionaries are resolved at the call sites.  Visiting
--   a class @C@ enqueues the occurrence names of every binding declared
--   inside any @instance C T@ in any file ('ClassIndex').  Those then
--   flow through the regular name BFS, which is how instance-method
--   bodies enter the dep hash even when the body never textually
--   mentions the method name.
--
-- This is an over-approximation: visiting class @Num@ pulls in the
-- methods of /every/ @Num@ instance in the project, not just the one
-- specialised to the relevant type.  That is acceptable — false
-- positives (over-invalidation) are documented; what we are fixing is
-- the false negative.
--
-- For files that use CPP the /entire/ file source is included (as one
-- chunk) instead of individual declaration spans, because CPP macro
-- names are invisible to the HIE graph.
transitiveDeps
  :: Map FilePath (Map String DeclData)
  -> Map FilePath ClassIndex      -- per-file class → method index
  -> Map FilePath BS.ByteString   -- raw file source
  -> FilePath                      -- test file to exclude
  -> Set String                    -- names used directly in the test body
  -> Set String                    -- classes used directly in the test body
  -> [BS.ByteString]
transitiveDeps declMaps classIndices fileSrcs testFile startNames startClasses =
    declChunks ++ cppChunks ++ pragmaChunks
  where
    -- Method names for the initial set of classes.
    initialClassMethodNames = Set.unions
      [ ms
      | c          <- Set.toList startClasses
      , (_, ci)    <- Map.toList classIndices
      , Just ms    <- [Map.lookup c ci]
      ]

    initialNames = Set.union startNames initialClassMethodNames

    allPairs = bfs Set.empty Set.empty startClasses (Set.toList initialNames)

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
      | f        <- Set.toList (Set.map fst allPairs)
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

    -- BFS over the name queue.  When a binding's body has 'ddUsedClasses'
    -- we also enqueue every method name from any instance of those
    -- classes (across all files), tracked via 'visitedClasses' to avoid
    -- redoing the expansion.
    bfs pairs _visitedNames _visitedClasses [] = pairs
    bfs pairs visitedNames visitedClasses (name : queue)
      | Set.member name visitedNames =
          bfs pairs visitedNames visitedClasses queue
      | otherwise =
          let visitedNames' = Set.insert name visitedNames
              newPairs = Set.fromList
                [ (f, name)
                | (f, dm) <- Map.toList declMaps
                , f /= testFile
                , Map.member name dm
                ]
              -- Names referenced by the bodies of visited bindings
              referencedNames =
                [ n
                | (f, _)  <- Set.toList newPairs
                , Just dm <- [Map.lookup f declMaps]
                , Just dd <- [Map.lookup name dm]
                , n       <- Set.toList (ddUsedNames dd)
                ]
              -- Newly seen classes to expand into method names
              newClasses =
                [ c
                | (f, _)  <- Set.toList newPairs
                , Just dm <- [Map.lookup f declMaps]
                , Just dd <- [Map.lookup name dm]
                , c       <- Set.toList (ddUsedClasses dd)
                , c `Set.notMember` visitedClasses
                ]
              visitedClasses' = Set.union visitedClasses (Set.fromList newClasses)
              classExpansion =
                [ m
                | c       <- newClasses
                , (_, ci) <- Map.toList classIndices
                , Just ms <- [Map.lookup c ci]
                , m       <- Set.toList ms
                ]
              moreNames = filter (`Set.notMember` visitedNames')
                            (referencedNames ++ classExpansion)
          in bfs (Set.union pairs newPairs) visitedNames' visitedClasses'
               (queue ++ moreNames)

-- ---------------------------------------------------------------------------
-- HIE AST analysis

-- | Walk every node and collect each @EvidenceVarBind@ into the
-- 'EvBindIndex'.  Both @EvInstBind@ (the actual top-level instance
-- dictionaries) and @EvLetBind@ (the per-call-site aliases GHC inserts
-- when applying a constraint) are recorded; resolution to a class is
-- via 'resolveEvClass', which follows the @EvLet@ chain to an
-- @EvInst@.
--
-- When the same 'Unique' is bound by both flavours (rare), 'EvInst'
-- wins so we never lose a direct class.
collectEvBinds :: HieData -> EvBindIndex
collectEvBinds (HieData _ ast) =
  Map.fromListWith preferInst
    [ (uniqWord name, b)
    | node <- flatNodes ast
    , ni <- Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo node))
    , (Right name, details) <- Map.toList (HIE.nodeIdentifiers ni)
    , Just b <- map ctxEvBind (Set.toList (HIE.identInfo details))
    ]
  where
    flatNodes n = n : concatMap flatNodes (HIE.nodeChildren n)

    preferInst a@(EvInst _) _ = a
    preferInst _ b@(EvInst _) = b
    preferInst a            _ = a

-- | Extract the 'EvBind' described by a 'ContextInfo', if any.
ctxEvBind :: HIE.ContextInfo -> Maybe EvBind
ctxEvBind (HIE.EvidenceVarBind (HIE.EvInstBind _ clsName) _ _) =
  Just (EvInst (occNameString (nameOccName clsName)))
ctxEvBind (HIE.EvidenceVarBind (HIE.EvLetBind deps) _ _) =
  Just (EvLet (map uniqWord (HIE.getEvBindDeps deps)))
ctxEvBind _ = Nothing

-- | Return the class 'Name' from an @EvidenceVarBind (EvInstBind …)@
-- context, or 'Nothing' for any other 'ContextInfo'.  Used by the
-- per-file class index to identify instance-node containers.
ctxInstClass :: HIE.ContextInfo -> Maybe Name
ctxInstClass (HIE.EvidenceVarBind (HIE.EvInstBind _ clsName) _ _) =
  Just clsName
ctxInstClass _ = Nothing

-- | Build a map from occName to @DeclData@ for every top-level binding in
-- the AST plus a per-file @ClassIndex@ mapping each class to the
-- occurrence names of its instance methods within this file.  The global
-- 'EvBindIndex' lets us recognise which 'Name's used inside a body's span
-- are evidence variables for which class.
buildFileIndex :: EvBindIndex -> HieData -> (Map String DeclData, ClassIndex)
buildFileIndex globalEvBinds (HieData src ast) =
    (Map.fromListWith mergeDeclData (concatMap nodeEntries nodes), classIdx)
  where
    nodes = flatNodes ast
    flatNodes n = n : concatMap flatNodes (HIE.nodeChildren n)

    nodeEntries node =
      [ (occNameString (nameOccName name),
         DeclData
           [[extractSpanLines src nodeSpan]]
           (getUsedNames ast startLine endLine)
           (getUsedClasses ast globalEvBinds startLine endLine))
      | (Right name, details) <- concatMap (Map.toList . HIE.nodeIdentifiers)
                                            (Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo node)))
      , any isDeclCtx (Set.toList (HIE.identInfo details))
      , let nodeSpan          = HIE.nodeSpan node
            (startLine, endLine) = bodyLineRange src nodeSpan
      ]

    -- A node is treated as an /instance node/ if any of its immediate
    -- children carries an @EvidenceVarBind (EvInstBind _ cls)@
    -- identifier — that is, the synthetic dictionary binding generated
    -- by @instance C T where …@.  When we hit one, every @MatchBind@ or
    -- @ValBind@ binder anywhere in that node's subtree is registered as a
    -- method of @cls@.
    --
    -- We don't recurse further inside an instance node looking for /more/
    -- instance nodes (Haskell forbids nested @instance@ declarations,
    -- and method bodies' local bindings should already be reached via
    -- the regular name-edge BFS).
    classIdx :: ClassIndex
    classIdx = Map.fromListWith Set.union (concatMap goClass [ast])
      where
        goClass node =
          case nodeImmediateChildClasses node of
            cls : _ ->
              -- Treat this node as an instance node with class `cls`.
              -- Collect every binding name in its subtree.
              [ (cls, Set.singleton name)
              | sub <- subtree node
              , ni  <- Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo sub))
              , (Right n, details) <- Map.toList (HIE.nodeIdentifiers ni)
              , any isBindCtx (Set.toList (HIE.identInfo details))
              , let name = occNameString (nameOccName n)
              ]
            [] ->
              concatMap goClass (HIE.nodeChildren node)

        subtree n = n : concatMap subtree (HIE.nodeChildren n)

        nodeImmediateChildClasses node =
          [ occNameString (nameOccName clsName)
          | child <- HIE.nodeChildren node
          , ni <- Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo child))
          , (Right _, details) <- Map.toList (HIE.nodeIdentifiers ni)
          , Just clsName <- map ctxInstClass (Set.toList (HIE.identInfo details))
          ]

        isBindCtx HIE.MatchBind  = True
        isBindCtx (HIE.ValBind {}) = True
        isBindCtx _              = False

    mergeDeclData (DeclData c1 u1 cl1) (DeclData c2 u2 cl2) =
      DeclData (c1 ++ c2) (Set.union u1 u2) (Set.union cl1 cl2)

    isDeclCtx :: HIE.ContextInfo -> Bool
    isDeclCtx = \case
      HIE.ValBind {} -> True
      HIE.MatchBind  -> True
      HIE.Decl {}    -> True
      -- 'TyDecl' tags identifiers appearing in their own type signature
      -- (and in associated-type/class-method signature contexts).
      -- Including it folds the signature line(s) into each binding's
      -- chunk, so changes that touch only the signature — e.g.
      -- swapping the order of variables in an explicit @forall@,
      -- which is observable under @TypeApplications@ — invalidate
      -- the cache.
      HIE.TyDecl     -> True
      _              -> False

-- | Compute the line-range of an equation body starting at this AST
-- node's span, using the same indentation heuristic as 'computeTestFP'.
bodyLineRange :: BS.ByteString -> RealSrcSpan -> (Int, Int)
bodyLineRange src nodeSpan =
  let startLine = srcSpanStartLine nodeSpan
      offset    = lineStartOffset src startLine
      baseInd   = countIndent src offset
      exprEnd   = findExprEnd src offset baseInd
      bodyBytes = BS.take (exprEnd - offset) (BS.drop offset src)
      endLine   = startLine + max 0 (BSC.count '\n' bodyBytes - 1)
  in  (startLine, endLine)

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

-- | Collect the class names whose evidence is used within source lines
-- @startLine..endLine@.  An evidence-variable use carries an
-- 'HIE.EvidenceVarUse' context info; we look up the bound 'Name' (the
-- dictionary) in the global 'EvBindIndex' to recover the class.
getUsedClasses
  :: HIE.HieAST HIE.TypeIndex
  -> EvBindIndex
  -> Int -> Int -> Set String
getUsedClasses root globalEvBinds startLine endLine = go root
  where
    go node
      | srcSpanEndLine   (HIE.nodeSpan node) < startLine = Set.empty
      | srcSpanStartLine (HIE.nodeSpan node) > endLine   = Set.empty
      | otherwise =
          let mine  = foldMap nodeClassUses
                        (Map.elems (HIE.getSourcedNodeInfo (HIE.sourcedNodeInfo node)))
              below = foldMap go (HIE.nodeChildren node)
          in Set.union mine below

    nodeClassUses ni = Set.fromList
      [ cls
      | (Right name, details) <- Map.toList (HIE.nodeIdentifiers ni)
      , HIE.EvidenceVarUse `Set.member` HIE.identInfo details
      , Just cls <- [resolveEvClass globalEvBinds (uniqWord name)]
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

-- Source navigation utilities live in Test.Tasty.HieCache.Internal

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

-- ---------------------------------------------------------------------------
-- Internal: testing helpers
--
-- These are exported for unit tests that need to drive the fingerprinting
-- machinery against in-memory mutations of HIE-recorded source bytes (i.e.
-- to simulate the effect of a code edit without recompiling).  They are not
-- part of the stable API.

-- | Load HIE files from @hieDir@, optionally substitute the source bytes
-- of selected files (keyed by HIE filename, e.g. @"Instances.hie"@), and
-- compute the fingerprint of the test whose leaf name matches
-- @leafName@.
internalComputeFingerprint
  :: FilePath
  -> Map FilePath BS.ByteString  -- ^ source overrides, keyed by HIE filename
  -> Fingerprint                  -- ^ cabal hash to mix into the fingerprint
  -> String                       -- ^ test leaf name
  -> IO (Maybe Fingerprint)
internalComputeFingerprint hieDir overrides cabalHash leafName = do
  hieFiles <- sort . filter ((== ".hie") . takeExtension)
                <$> listDirectory hieDir
  nc <- initNameCache 'z' []
  rawData <- mapM (\f -> (f,) <$> readHieData nc (hieDir </> f)) hieFiles
  let original = [ (f, d) | (f, Just d) <- rawData ]
      allData =
        [ (f, case Map.lookup f overrides of
                Just newSrc -> d { hdSrc = newSrc }
                Nothing     -> d)
        | (f, d) <- original
        ]
      globalEvBinds = Map.unions [ collectEvBinds d | (_, d) <- allData ]
      indices = [ (f, buildFileIndex globalEvBinds d) | (f, d) <- allData ]
      declMaps     = Map.fromList [ (f, dm) | (f, (dm, _)) <- indices ]
      classIndices = Map.fromList [ (f, ci) | (f, (_, ci)) <- indices ]
      fileSrcs     = Map.fromList [ (f, hdSrc d) | (f, d) <- allData ]
  return (computeTestFP allData globalEvBinds declMaps classIndices
                         fileSrcs cabalHash leafName)

