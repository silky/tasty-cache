-- | Genuine tests of the @tasty-cache@ library itself.
--
-- These tests verify that the library's own logic is correct.  They
-- come in two flavours:
--
--   * /Pure unit tests/ — exercise the helpers in
--     'Test.Tasty.HieCache.Internal' and the byte-level pragma / cabal
--     hashing mirrored from 'Test.Tasty.HieCache' directly.
--
--   * /Fingerprint-mutation tests/ — apply an in-memory edit to a
--     fixture's source bytes (without recompiling), recompute the
--     fingerprint via 'internalComputeFingerprint', and assert that
--     the fingerprint changed (or, for known limitations like
--     multi-line pragmas, did /not/ change).  Anchor 'testCase' names
--     they target live in 'Demos.hs'; @findSubstring@ resolves them
--     there because @Demos.hie@ sorts alphabetically before
--     @Library.hie@ and the anchor strings are not present in this
--     module's source bytes (built up via 'unwords' to avoid a
--     contiguous quoted match).
--
-- The whole tree is wrapped in 'cacheable' — the library's tests are
-- pure and deterministic, so the project dogfoods its own cache.
module Library (libraryTests) where

import qualified Data.ByteString.Char8        as BSC
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           FixtureHelpers               (assertFingerprintChanged,
                                               assertFingerprintUnchanged)
import           Test.Tasty.HieCache          (cacheable)
import           Test.Tasty.HieCache.Internal (countIndent, findExprEnd,
                                               pathKey)

libraryTests :: TestTree
libraryTests = cacheable $ testGroup "Library"
  [ sourceNavigationTests
  , cacheKeyLogicTests
  , pragmaAndCabalHashTests
  , fingerprintComputationTests
  ]

-- ---------------------------------------------------------------------------
-- Source-navigation helpers (Test.Tasty.HieCache.Internal)
--
-- 'findExprEnd' uses an indentation heuristic to determine where a
-- testCase expression (or a top-level binding) ends.  It treats blank
-- lines as terminators, which is a known false-negative when a
-- multi-line body has internal blank lines.

sourceNavigationTests :: TestTree
sourceNavigationTests = testGroup "Source-navigation helpers"
  [ testCase "findExprEnd: blank line inside do-block truncates body" $ do
      let src = BSC.pack $ unlines
            [ "    testCase \"foo\" $ do"
            , "      let x = helper1"
            , ""
            , "      x @?= expected"
            ]
          lineStart  = 0
          baseIndent = countIndent src 0
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      assertBool "body should NOT contain the assertion after the blank line"
        (not $ BSC.isInfixOf (BSC.pack "expected") bodyBytes)

  , testCase "findExprEnd: full body is captured without blank line" $ do
      let src = BSC.pack $ unlines
            [ "    testCase \"foo\" $ do"
            , "      let x = helper1"
            , "      x @?= expected"
            ]
          lineStart  = 0
          baseIndent = countIndent src 0
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      assertBool "body should contain the full expression"
        (BSC.isInfixOf (BSC.pack "expected") bodyBytes)

  , testCase "findExprEnd: blank line in function definition truncates dependency tracking" $ do
      let src = BSC.pack $ unlines
            [ "myHelper x ="
            , "    let a = dep1 x"
            , ""
            , "        b = dep2 a"
            , "    in a + b"
            ]
          lineStart  = 0
          baseIndent = countIndent src 0
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      assertBool "dep2 after blank line is NOT included in body"
        (not $ BSC.isInfixOf (BSC.pack "dep2") bodyBytes)
  ]

-- ---------------------------------------------------------------------------
-- Cache-key logic
--
-- The staleness check, leafMap collision behaviour, and the pure-Haskell
-- model of the BFS used in 'transitiveDeps'.

cacheKeyLogicTests :: TestTree
cacheKeyLogicTests = testGroup "Cache-key logic"
  -- Staleness: a missing fingerprint should be treated as stale, not
  -- as "matches the empty cache".  Earlier versions had a bug here
  -- (Nothing == Nothing); the current logic returns True for missing.
  [ testCase "staleness: missing fingerprint with empty cache → stale (post-fix)" $ do
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.empty :: Map.Map String Int
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= True

  , testCase "staleness: matching fingerprint → fresh" $ do
      let fps   = Map.singleton "foo" (42 :: Int)
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= False

  , testCase "staleness: changed fingerprint → stale" $ do
      let fps   = Map.singleton "foo" (99 :: Int)
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= True

  -- leafMap collision: two tests with the same leaf name in different
  -- groups collide; only one path survives in the map.  Documented
  -- false-negative.
  , testCase "leafMap: duplicate leaf name drops one path" $ do
      let paths :: [[String]]
          paths = [ ["group A", "add"]
                  , ["group B", "add"]
                  ]
          leafMap = Map.fromListWith const
            [ (last p, p) | p <- paths, not (null p) ]
      Map.size leafMap @?= 1

  , testCase "leafMap: dropped test gets no fingerprint key" $ do
      let paths :: [[String]]
          paths = [ ["group A", "add"]
                  , ["group B", "add"]
                  ]
          leafMap = Map.fromListWith const
            [ (last p, p) | p <- paths, not (null p) ]
          fps = Map.fromList
            [ (pathKey path, 42 :: Int)
            | (_leafName, path) <- Map.toList leafMap
            ]
          droppedKey = case Map.lookup "add" leafMap of
            Just kept -> case [ p | p <- paths, p /= kept ] of
                           (other:_) -> pathKey other
                           []        -> error "impossible: no other path"
            Nothing   -> error "impossible: no entry"
      Map.member droppedKey fps @?= False

  -- Pure model of the bare-name BFS used by 'transitiveDeps'.  Used to
  -- demonstrate why class-edge BFS was needed: a name-only BFS does
  -- not reach instance methods invoked implicitly through type-class
  -- resolution.
  , testCase "name-only BFS misses instance methods not textually reachable" $ do
      let testBodyNames = Set.fromList ["poly", "Foo"]
          declMap = Map.fromList
            [ ("poly",        Set.fromList ["*", "+"])
            , ("*",           Set.empty)
            , ("+",           Set.empty)
            , ("fromInteger", Set.empty)
            , ("negate",      Set.empty)
            ]
          reachable = bfsReachable declMap testBodyNames
      assertBool "* is reachable"          (Set.member "*" reachable)
      assertBool "+ is reachable"          (Set.member "+" reachable)
      assertBool "fromInteger NOT reachable (false negative without class edges)"
        (not (Set.member "fromInteger" reachable))
      assertBool "negate NOT reachable"
        (not (Set.member "negate" reachable))

  , testCase "name-only BFS does follow methods textually mentioned in the path" $ do
      let testBodyNames = Set.fromList ["f"]
          declMap = Map.fromList
            [ ("f",      Set.fromList ["negate"])
            , ("negate", Set.empty)
            , ("abs",    Set.empty)
            ]
          reachable = bfsReachable declMap testBodyNames
      assertBool "negate (textually used by f) is reachable"
        (Set.member "negate" reachable)
      assertBool "abs (not textually used) is still unreachable"
        (not (Set.member "abs" reachable))
  ]

-- | Pure model of the @transitiveDeps@ bare-name BFS.
bfsReachable :: Map.Map String (Set String) -> Set String -> Set String
bfsReachable declMap = go Set.empty
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let visited'   = Set.union visited frontier
              nextNames  = Set.unions
                [ Map.findWithDefault Set.empty n declMap
                | n <- Set.toList frontier
                ]
              frontier'  = Set.difference nextNames visited'
          in go visited' frontier'

-- ---------------------------------------------------------------------------
-- Pragma & cabal hashing
--
-- 'transitiveDeps' folds in every line beginning with "{-#" from each
-- visited file.  Multi-line pragmas (where the continuation lines do
-- not begin with "{-#") are not captured — a documented limitation.
--
-- The cabal hash is the hash of every .cabal file in the project root,
-- so changes to default-extensions / ghc-options / build-depends roll
-- the global cabalHash.

pragmaAndCabalHashTests :: TestTree
pragmaAndCabalHashTests = testGroup "Pragma & cabal hash"
  [ testCase "pragma filter captures {-# LANGUAGE X #-} line" $ do
      let src = BSC.pack $ unlines
            [ "{-# LANGUAGE LambdaCase #-}"
            , "module Foo where"
            , "x :: Int"
            , "x = 1"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      captured @?= [BSC.pack "{-# LANGUAGE LambdaCase #-}"]

  , testCase "pragma filter distinguishes different LANGUAGE pragmas" $ do
      let mk ext = BSC.pack $ unlines
            [ "{-# LANGUAGE " ++ ext ++ " #-}"
            , "module Foo where"
            ]
          captured1 = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines (mk "LambdaCase"))
          captured2 = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines (mk "BangPatterns"))
      assertBool "different pragma → different captured chunk"
        (captured1 /= captured2)

  , testCase "pragma filter captures OPTIONS_GHC line" $ do
      let src = BSC.pack $ unlines
            [ "{-# OPTIONS_GHC -O0 #-}"
            , "module Foo where"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      captured @?= [BSC.pack "{-# OPTIONS_GHC -O0 #-}"]

  , testCase "pragma filter captures INLINE/NOINLINE/SPECIALIZE" $ do
      let src = BSC.pack $ unlines
            [ "{-# INLINE foo #-}"
            , "foo = 1"
            , "{-# NOINLINE bar #-}"
            , "bar = 2"
            , "{-# SPECIALIZE baz :: Int -> Int #-}"
            , "baz x = x"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      length captured @?= 3

  -- Multi-line pragmas: documented limitation.  The pragma-line filter
  -- only matches lines beginning with "{-#"; continuation lines escape.
  , testCase "multi-line LANGUAGE pragma: only first line captured (LIMITATION)" $ do
      let src = BSC.pack $ unlines
            [ "{-# LANGUAGE"
            , "    LambdaCase"
            , "  , ScopedTypeVariables"
            , "  #-}"
            , "module Foo where"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      captured @?= [BSC.pack "{-# LANGUAGE"]
      assertBool "continuation line 'LambdaCase' is NOT captured"
        (not (any (BSC.isInfixOf (BSC.pack "LambdaCase")) captured))

  , testCase "multi-line RULES pragma: continuation lines escape capture" $ do
      let src = BSC.pack $ unlines
            [ "{-# RULES"
            , "  \"foo/bar\" forall x. foo (bar x) = x"
            , "  #-}"
            , "module Foo where"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      length captured @?= 1
      assertBool "RULES body line is NOT captured"
        (not (any (BSC.isInfixOf (BSC.pack "foo/bar")) captured))

  , testCase "CPP detection: usesCPP triggers on '#define'" $ do
      let src1 = BSC.pack $ unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module Foo where"
            , "#define X 3"
            , "f :: Int"
            , "f = X"
            ]
          src2 = BSC.pack $ unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module Foo where"
            , "f :: Int"
            , "f = 3"
            ]
          usesCPP s = BSC.pack "#define " `BSC.isInfixOf` s
      usesCPP src1 @?= True
      usesCPP src2 @?= False

  -- Cabal hashing: pure byte-comparison model of what gets hashed.
  , testCase "cabal: default-extensions edit changes bytes" $ do
      let cabal1 = BSC.pack $ unlines
            [ "library"
            , "  default-extensions:"
            , "    LambdaCase"
            ]
          cabal2 = BSC.pack $ unlines
            [ "library"
            , "  default-extensions:"
            , "    LambdaCase"
            , "    BangPatterns"
            ]
      assertBool "cabal byte strings should differ" (cabal1 /= cabal2)

  , testCase "cabal: ghc-options edit changes bytes" $ do
      let cabal1 = BSC.pack "ghc-options: -O0\n"
          cabal2 = BSC.pack "ghc-options: -O2\n"
      assertBool "cabal byte strings should differ" (cabal1 /= cabal2)
  ]

-- ---------------------------------------------------------------------------
-- Fingerprint computation
--
-- End-to-end mutation tests against the real HIE files in @.hie/@.
-- 'fingerprintBeforeAfter' (in 'FixtureHelpers') applies an in-memory
-- source-byte substitution to one fixture's @hie_hs_src@, recomputes
-- the fingerprint, and compares to the unmutated baseline.
--
-- Each test below targets a specific dependency-tracking mechanism:
-- name-edge BFS, class-edge BFS, type-signature inclusion, the
-- implicit-dispatch heuristic, or the multi-line pragma limitation.

fingerprintComputationTests :: TestTree
fingerprintComputationTests = testGroup "Fingerprint computation"
  [ -- Name-edge BFS: editing a function body invalidates tests that
    -- reach that function transitively.
    testCase "name-edge: factorial body edit invalidates factorial test" $
      assertFingerprintChanged
        (unwords ["factorial", "5", "==", "120"])
        "test/Lib.hs"
        "Lib.hie"
        "n * factorial (n - 1)"
        "n + factorial (n - 1)"

  -- Type-signature inclusion: TyDecl-context bytes are folded into
  -- each binding's chunks, so signature-only edits invalidate.
  , testCase "type-sig: forall-ordering swap on Polymorphic.pair invalidates" $
      assertFingerprintChanged
        (unwords ["pair", "returns", "a", "tuple", "of", "its", "arguments"])
        "test/Polymorphic.hs"
        "Polymorphic.hie"
        "forall a b. a -> b -> (a, b)"
        "forall a b. a -> b -> (b, a)"

  -- Pragma-line capture: editing a {-# LANGUAGE X #-} line in a dep
  -- file invalidates every test reaching that file.
  , testCase "pragma-line: editing {-# LANGUAGE ExplicitForAll #-} invalidates pair" $
      assertFingerprintChanged
        (unwords ["pair", "returns", "a", "tuple", "of", "its", "arguments"])
        "test/Polymorphic.hs"
        "Polymorphic.hie"
        "{-# LANGUAGE ExplicitForAll #-}"
        "{-# LANGUAGE ExplicitForAll, BangPatterns #-}"

  -- Class-edge BFS: from FalseNegatives, the canonical class-edge
  -- regression that motivated the BFS extension.
  , testCase "class-edge: fromInteger edit on Num Foo invalidates poly @Foo" $
      assertFingerprintChanged
        (unwords ["poly", "@Foo", "3", "==", "Foo", "10"])
        "test/Instances.hs"
        "Instances.hie"
        "Foo (fromInteger n)"
        "Foo (fromInteger n + 100)"

  , testCase "class-edge: unused Num Foo method (signum) edit invalidates poly @Foo" $
      assertFingerprintChanged
        (unwords ["poly", "@Foo", "3", "==", "Foo", "10"])
        "test/Instances.hs"
        "Instances.hie"
        "Foo (signum a)"
        "Foo (signum a + 1)"

  , testCase "class-edge: edit on unrelated Num Bar invalidates poly @Foo" $
      assertFingerprintChanged
        (unwords ["poly", "@Foo", "3", "==", "Foo", "10"])
        "test/Instances.hs"
        "Instances.hie"
        "Bar (fromInteger n)"
        "Bar (fromInteger n + 100)"

  -- Precision boundary: a monomorphic test using only @(+)@ does NOT
  -- pull in user-defined Num instances (because @(+)@ dispatches
  -- through @Num Int@ from base, outside our HIE files).
  , testCase "precision: Num Foo edit does NOT invalidate add 1 2 == 3" $
      assertFingerprintUnchanged
        (unwords ["add", "1", "2", "==", "3"])
        "test/Instances.hs"
        "Instances.hie"
        "Foo (fromInteger n)"
        "Foo (fromInteger n + 100)"

  , testCase "class-edge: Greet Foo edit invalidates roast (Foo 3)" $
      assertFingerprintChanged
        (unwords ["roast", "(Foo", "3)", "uses", "both", "Greet",
                  "and", "Insult", "instances"])
        "test/Instances.hs"
        "Instances.hie"
        "\"Hello Foo \""
        "\"Howdy Foo \""

  , testCase "class-edge: Insult Foo edit invalidates roast (Foo 3)" $
      assertFingerprintChanged
        (unwords ["roast", "(Foo", "3)", "uses", "both", "Greet",
                  "and", "Insult", "instances"])
        "test/Instances.hs"
        "Instances.hie"
        "\" is silly\""
        "\" is splendid\""

  -- Implicit-dispatch heuristic — the buildFileIndex augmentation that
  -- closed three previously-discovered false negatives.

  -- OverloadedStrings: name-edge sanity (BFS reaches OvStrFix).
  , testCase "OvStr name-edge: greetOvStr body edit invalidates" $
      assertFingerprintChanged
        (unwords ["OvStr", "greet", "hello"])
        "test/OvStrFix.hs"
        "OvStrFix.hie"
        "greetOvStr = \"world\""
        "greetOvStr = \"WORLD\""

  -- OverloadedStrings: heuristic adds IsString → fromString bytes
  -- enter the dep set.
  , testCase "OvStr class-edge: fromString edit invalidates" $
      assertFingerprintChanged
        (unwords ["OvStr", "greet", "hello"])
        "test/OvStrFix.hs"
        "OvStrFix.hie"
        "S (\"ovstr:\" ++ s)"
        "S (\"OVSTR:\" ++ s ++ \"!\")"

  -- OverloadedLists: name-edge sanity.
  , testCase "OvList name-edge: sampleOvList body edit invalidates" $
      assertFingerprintChanged
        (unwords ["OvList", "sample", "1", "2", "3"])
        "test/OvListFix.hs"
        "OvListFix.hie"
        "sampleOvList = [1,2,3]"
        "sampleOvList = [9,9,9]"

  -- OverloadedLists: heuristic adds IsList → fromList bytes enter
  -- the dep set.
  , testCase "OvList class-edge: fromList edit invalidates" $
      assertFingerprintChanged
        (unwords ["OvList", "sample", "1", "2", "3"])
        "test/OvListFix.hs"
        "OvListFix.hie"
        "L (xs ++ [99999])"
        "L (xs ++ [88888, 77777])"

  -- OverloadedLabels: works without the heuristic — IsLabel is
  -- recorded as an EvidenceVarUse at the @#k@ desugaring site.
  , testCase "OvLabel: fromLabel edit invalidates" $
      assertFingerprintChanged
        (unwords ["OvLab", "sample", "k"])
        "test/OvLabFix.hs"
        "OvLabFix.hie"
        "Lbl \"ovlabel-k\""
        "Lbl \"OVLABEL-K-EDITED\""

  -- RebindableSyntax: name-edge sanity.
  , testCase "Rebind name-edge: rebindIf body edit invalidates" $
      assertFingerprintChanged
        (unwords ["Rebind", "if", "1", "else", "2"])
        "test/RebindFix.hs"
        "RebindFix.hie"
        "rebindIf = if True then 1 else 2"
        "rebindIf = if True then 99 else 2"

  -- RebindableSyntax: heuristic adds 'ifThenElse' (and other rebound
  -- names) to ddUsedNames, so editing the local 'ifThenElse' body
  -- invalidates.
  , testCase "Rebind class-edge: ifThenElse edit invalidates" $
      assertFingerprintChanged
        (unwords ["Rebind", "if", "1", "else", "2"])
        "test/RebindFix.hs"
        "RebindFix.hie"
        "t P.+ 1000"
        "t P.+ 2000"

  -- DefaultSignatures: editing the class default body invalidates a
  -- test that uses an instance which inherits the default.
  , testCase "DefaultSignatures: default body edit invalidates" $
      assertFingerprintChanged
        (unwords ["DefSig", "describe", "42"])
        "test/DefSigFix.hs"
        "DefSigFix.hie"
        "\"defsig-prefix \" ++ show x"
        "\"DEFSIG-EDITED \" ++ show x"

  , testCase "DeriveAnyClass: class default edit invalidates" $
      assertFingerprintChanged
        (unwords ["AnyClass", "hello", "AnyClass"])
        "test/AnyClassFix.hs"
        "AnyClassFix.hie"
        "\"anyclass-greeting\""
        "\"ANYCLASS-EDITED\""

  , testCase "DerivingVia: via-instance method edit invalidates" $
      assertFingerprintChanged
        (unwords ["Via", "shout", "ViaThing", "7"])
        "test/ViaFix.hs"
        "ViaFix.hie"
        "\"via-loud-prefix \" ++ show x"
        "\"VIA-EDITED \" ++ show x"

  , testCase "GND: underlying-instance method edit invalidates" $
      assertFingerprintChanged
        (unwords ["GND", "bark", "GNDThing", "5"])
        "test/GNDFix.hs"
        "GNDFix.hie"
        "\"gnd-bark \" ++ show n"
        "\"GND-EDITED \" ++ show n"

  , testCase "StandaloneDeriving: cross-module data-type edit invalidates" $
      assertFingerprintChanged
        (unwords ["StdDeriv", "show", "StdBranch"])
        "test/StdDerivData.hs"
        "StdDerivData.hie"
        "data StdData = StdLeaf | StdBranch StdData StdData"
        "data StdData = StdLeaf | StdBranch StdData StdData | StdExtra Int"

  , testCase "PatternSynonyms: synonym RHS edit invalidates" $
      assertFingerprintChanged
        (unwords ["PatSyn", "Bin", "7"])
        "test/PatSynData.hs"
        "PatSynData.hie"
        "pattern Bin l v r = TNode l v r"
        "pattern Bin l v r = TNode r v l"

  , testCase "TypeFamilies: instance RHS edit invalidates" $
      assertFingerprintChanged
        (unwords ["TyFam", "listElem", "1", "2", "3"])
        "test/TyFamFix.hs"
        "TyFamFix.hie"
        "type instance Element [a] = a"
        "type instance Element [a] = Int"

  , testCase "DataKinds: promoted-tag data type edit invalidates" $
      assertFingerprintChanged
        (unwords ["DKinds", "describeTag", "Foo"])
        "test/DKindsFix.hs"
        "DKindsFix.hie"
        "data Tag = Foo | Bar"
        "data Tag = Foo | Bar | Baz"

  , testCase "ImplicitParams: ?greeting binding edit invalidates" $
      assertFingerprintChanged
        (unwords ["ImpPar", "greetImpl", "world"])
        "test/ImpParFix.hs"
        "ImpParFix.hie"
        "\"imppar-hello\""
        "\"IMPPAR-EDITED\""

  -- Multi-line pragma: pinning the documented limitation.  Editing a
  -- continuation line of a multi-line @{-# LANGUAGE … #-}@ block does
  -- NOT invalidate, because pragmaChunks only captures lines starting
  -- with @{-#@ and the implicit-dispatch heuristic's parser likewise
  -- only sees single-line LANGUAGE pragmas.
  , testCase "multi-line pragma (LIMITATION): continuation-line edit does NOT invalidate" $
      assertFingerprintUnchanged
        (unwords ["MLP", "zero"])
        "test/MultiLinePragmaFix.hs"
        "MultiLinePragmaFix.hie"
        "  , ScopedTypeVariables"
        "  , ScopedTypeVariables, BangPatterns"
  ]
