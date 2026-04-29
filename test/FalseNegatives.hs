-- | Tests demonstrating false-negative scenarios in the caching logic.
--
-- A false negative means a test is served from cache (instant pass) when it
-- should have been re-run.  Each test group below isolates one mechanism by
-- which this can happen.
module FalseNegatives (falseNegativeTests) where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           GHC.Fingerprint              (Fingerprint (..))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Tasty.HieCache          (internalComputeFingerprint)
import           Test.Tasty.HieCache.Internal (countIndent, findExprEnd,
                                               pathKey)

falseNegativeTests :: TestTree
falseNegativeTests = testGroup "False-negative scenarios"
  [ stalenessTests
  , leafMapTests
  , findExprEndTests
  , instanceResolutionTests
  ]

-- ---------------------------------------------------------------------------
-- 1. The Nothing == Nothing staleness bug
--
-- The original staleness check was:
--
--   Map.lookup key fps /= Map.lookup key cache
--
-- When a test has no fingerprint (fps lookup = Nothing) and the cache is
-- empty (cache lookup = Nothing), this compares Nothing /= Nothing = False,
-- so the test is treated as fresh and replaced with an instant-pass
-- CachedTest — without ever running.

stalenessTests :: TestTree
stalenessTests = testGroup "Staleness logic"
  [ testCase "old logic: missing fingerprint + empty cache → wrongly fresh" $ do
      -- Simulate: test "foo" has no fingerprint, cache is empty
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.empty :: Map.Map String Int
          key   = "foo"
          -- Old logic: Nothing /= Nothing = False → NOT stale (bug!)
          oldIsStale = Map.lookup key fps /= Map.lookup key cache
      oldIsStale @?= False  -- demonstrates the bug

  , testCase "new logic: missing fingerprint + empty cache → correctly stale" $ do
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.empty :: Map.Map String Int
          key   = "foo"
          -- New logic: no fingerprint → always stale
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= True

  , testCase "old logic: missing fingerprint + stale cache → correctly stale" $ do
      -- If a previous fingerprint exists in cache but current is missing,
      -- the old logic happens to get this right: Nothing /= Just x = True
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          oldIsStale = Map.lookup key fps /= Map.lookup key cache
      oldIsStale @?= True

  , testCase "matching fingerprint → correctly fresh" $ do
      let fps   = Map.singleton "foo" (42 :: Int)
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= False

  , testCase "changed fingerprint → correctly stale" $ do
      let fps   = Map.singleton "foo" (99 :: Int)
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= True
  ]

-- ---------------------------------------------------------------------------
-- 2. leafMap collision: duplicate leaf names
--
-- leafMap = Map.fromListWith const [ (last p, p) | ... ]
--
-- Two tests with the same leaf name in different groups collide.  Only one
-- path is kept; the other gets no fingerprint → falls into bug #1.

leafMapTests :: TestTree
leafMapTests = testGroup "leafMap collision"
  [ testCase "duplicate leaf name drops one path" $ do
      let paths :: [[String]]
          paths = [ ["group A", "add"]   -- leaf name = "add"
                  , ["group B", "add"]   -- same leaf name!
                  ]
          leafMap = Map.fromListWith const
            [ (last p, p) | p <- paths, not (null p) ]
      -- Only one entry survives
      Map.size leafMap @?= 1

  , testCase "dropped test gets no fingerprint key" $ do
      let paths :: [[String]]
          paths = [ ["group A", "add"]
                  , ["group B", "add"]
                  ]
          leafMap = Map.fromListWith const
            [ (last p, p) | p <- paths, not (null p) ]
          -- Simulate fingerprinting: only the surviving path gets a key
          fps = Map.fromList
            [ (pathKey path, 42 :: Int)
            | (_leafName, path) <- Map.toList leafMap
            ]
      -- The dropped path has no fingerprint
      let droppedKey = case Map.lookup "add" leafMap of
            Just kept -> case [ p | p <- paths, p /= kept ] of
                           (other:_) -> pathKey other
                           []        -> error "impossible: no other path"
            Nothing   -> error "impossible: no entry"
      Map.member droppedKey fps @?= False
  ]

-- ---------------------------------------------------------------------------
-- 3. findExprEnd stops at blank lines
--
-- countIndent returns 0 for a blank line.  findExprEnd stops when
-- countIndent pos <= baseIndent.  So a blank line inside a multi-line
-- expression truncates the body hash — edits after the blank line are
-- invisible to the cache.

findExprEndTests :: TestTree
findExprEndTests = testGroup "findExprEnd blank-line truncation"
  [ testCase "blank line inside do-block truncates body" $ do
      -- Simulates a test body like:
      --     testCase "foo" $ do
      --       let x = helper1
      --                          ← blank line
      --       x @?= expected
      let src = BSC.pack $ unlines
            [ "    testCase \"foo\" $ do"       -- line 0 (offset 0), indent 4
            , "      let x = helper1"           -- line 1, indent 6
            , ""                                -- line 2, blank
            , "      x @?= expected"            -- line 3, indent 6
            ]
          lineStart  = 0
          baseIndent = countIndent src 0  -- 4
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      -- The blank line causes findExprEnd to stop before "x @?= expected"
      assertBool "body should NOT contain the assertion after the blank line"
        (not $ BSC.isInfixOf (BSC.pack "expected") bodyBytes)

  , testCase "without blank line, full body is captured" $ do
      let src = BSC.pack $ unlines
            [ "    testCase \"foo\" $ do"
            , "      let x = helper1"
            , "      x @?= expected"
            ]
          lineStart  = 0
          baseIndent = countIndent src 0  -- 4
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      assertBool "body should contain the full expression"
        (BSC.isInfixOf (BSC.pack "expected") bodyBytes)

  , testCase "blank line in function definition truncates dependency tracking" $ do
      -- A library function with a blank line:
      --   myHelper x =
      --       let a = dep1 x
      --
      --           b = dep2 a
      --       in a + b
      --
      -- usedInBody uses findExprEnd from the start of the declaration,
      -- so it would stop at the blank line and miss dep2.
      let src = BSC.pack $ unlines
            [ "myHelper x ="                -- line 0, indent 0
            , "    let a = dep1 x"          -- line 1, indent 4
            , ""                            -- line 2, blank
            , "        b = dep2 a"          -- line 3, indent 8
            , "    in a + b"                -- line 4, indent 4
            ]
          lineStart  = 0
          baseIndent = countIndent src 0  -- 0
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      -- dep2 is after the blank line — invisible to the cache
      assertBool "dep2 after blank line is NOT included in body"
        (not $ BSC.isInfixOf (BSC.pack "dep2") bodyBytes)
  ]

-- ---------------------------------------------------------------------------
-- 4. Instance resolution / overloaded identifiers
--
-- These tests motivate and verify the class-edge BFS extension that
-- 'transitiveDeps' applies on top of the plain name-edge BFS.
--
-- The first two unit tests model only the *name-edge* BFS in pure
-- Haskell to demonstrate the false negative that necessitates the class
-- edges: an instance method invoked implicitly through type-class
-- resolution (@fromInteger@ at a numeric literal, for example) is not
-- reached by the name-edge BFS because no name on the BFS path
-- textually mentions it.
--
-- The third test ('fromInteger edit changes poly fingerprint') is an
-- end-to-end check against real HIE files: it loads the project's HIE
-- files, applies an in-memory mutation to @Instances.hs@'s
-- @fromInteger@ clause (without recompiling), and asserts that the
-- fingerprint of @poly @Foo 3 == Foo 10@ changes.  This passes only
-- when the class-edge BFS chases the evidence-var application from the
-- test body's @poly@ call into the @Num Foo@ instance and folds its
-- method bodies into the dep hash.

instanceResolutionTests :: TestTree
instanceResolutionTests = testGroup "Instance resolution"
  [ testCase "name-only BFS misses instance methods not textually reachable" $ do
      -- Names directly used in the test body `poly (Foo 3) @?= Foo 10`:
      let testBodyNames = Set.fromList ["poly", "Foo"]

      -- Decl map: name → set of names used in body (modelling ddUsedNames).
      -- `poly`'s body is `x * x + 1` — uses `*` and `+`; the `1` is a
      -- literal with no textual name.
      let declMap = Map.fromList
            [ ("poly",        Set.fromList ["*", "+"])
            -- The instance methods on `Num Foo` are each keyed by their
            -- occurrence name in `buildDeclMap`:
            , ("*",           Set.empty)
            , ("+",           Set.empty)
            , ("fromInteger", Set.empty)
            , ("negate",      Set.empty)
            ]

      let reachable = bfsReachable declMap testBodyNames

      -- `*` and `+` are reached (textually used by `poly`):
      assertBool "* is reachable"
        (Set.member "*" reachable)
      assertBool "+ is reachable"
        (Set.member "+" reachable)
      -- But `fromInteger` is NOT reached, even though it is invoked at
      -- every numeric-literal occurrence in `poly`'s body when specialised
      -- at `Foo`.  Changes to `instance Num Foo` { `fromInteger` } go
      -- undetected — false negative.
      assertBool "fromInteger is NOT reachable (false negative)"
        (not (Set.member "fromInteger" reachable))
      -- Same for any instance method whose name does not textually appear
      -- on a BFS path — `negate`, `abs`, `signum`, ...
      assertBool "negate is NOT reachable (false negative)"
        (not (Set.member "negate" reachable))

  , testCase "Polymorphic test body that names a method does follow it" $ do
      -- Sanity check: when the polymorphic helper *does* mention a method
      -- name, the BFS does follow it (this is why most cases happen to
      -- work by accident).  A helper `f x = negate x` keyed at "f", with
      -- an instance method "negate", is reachable from a test body that
      -- names "f".
      let testBodyNames = Set.fromList ["f"]
          declMap = Map.fromList
            [ ("f",      Set.fromList ["negate"])
            , ("negate", Set.empty)
            , ("abs",    Set.empty)
            ]
          reachable = bfsReachable declMap testBodyNames
      assertBool "negate (textually used by f) is reachable"
        (Set.member "negate" reachable)
      -- abs is still not reached: not textually mentioned anywhere.
      assertBool "abs (not textually used) is still unreachable"
        (not (Set.member "abs" reachable))

    -- End-to-end check against real HIE files: mutate the source bytes
    -- of `Instances.hs`'s `fromInteger` clause in memory and require
    -- that the fingerprint of `poly @Foo 3 == Foo 10` changes.  The
    -- mutation does not touch any name on the bare-name BFS path, so
    -- prior to the evidence-following BFS this test fails.
    -- Built via 'unwords' so the source bytes of this test module do
    -- not contain the contiguous string literals — otherwise
    -- 'internalComputeFingerprint''s 'findSubstring' would locate them
    -- here (sorted before "Main.hie") and pick this file as the test's
    -- home.
  , testCase "fromInteger edit on Num Foo invalidates poly @Foo" $
      assertFingerprintChanged
        (unwords ["poly", "@Foo", "3", "==", "Foo", "10"])
        "test/Instances.hs"
        "Instances.hie"
        "Foo (fromInteger n)"
        "Foo (fromInteger n + 100)"

    -- Over-approximation: visiting class @Num@ enqueues every method of
    -- every @Num@ instance in every file.  So even editing an
    -- *unused* method (@signum@) of the relevant instance changes the
    -- dep hash for tests that use that class.  False positive,
    -- intentional.
  , testCase "unused Num Foo method (signum) edit invalidates poly @Foo" $
      assertFingerprintChanged
        (unwords ["poly", "@Foo", "3", "==", "Foo", "10"])
        "test/Instances.hs"
        "Instances.hie"
        "Foo (signum a)"
        "Foo (signum a + 1)"

    -- Over-approximation across *types* of the same class: editing
    -- @instance Num Bar@ — which @poly @Foo@ never uses — still
    -- invalidates the @poly @Foo@ test, because the class-edge BFS is
    -- class-only (not class+type).  The conservative direction.
  , testCase "edit on unrelated Num Bar invalidates poly @Foo" $
      assertFingerprintChanged
        (unwords ["poly", "@Foo", "3", "==", "Foo", "10"])
        "test/Instances.hs"
        "Instances.hie"
        "Bar (fromInteger n)"
        "Bar (fromInteger n + 100)"

    -- Precision boundary: a monomorphic Int test that uses *only*
    -- bare-name operators whose own body has no class-edge propagation
    -- is NOT invalidated by edits to user-defined instances of that
    -- class.  @add 1 2 == 3@ goes through @add@ → @(+)@; the @(+)@
    -- bindings inside @instance Num Foo@ and @instance Num Bar@ have
    -- empty 'ddUsedClasses' (their bodies dispatch on @Int@'s @Num@,
    -- whose evidence chain ends in @base@ — outside our globalEvBinds
    -- — and so resolves to nothing).  No class edge is taken; the
    -- @fromInteger@ bytes never enter the dep set; mutation does not
    -- change the fingerprint.
    --
    -- Caveat: this precision does not extend to tests that touch
    -- @(-)@.  GHC generates @(-)@ as a default-method body
    -- (@x - y = x + negate y@) whose @+@ and @negate@ are dispatched
    -- through the /user-defined/ instance dictionary; that dictionary
    -- /is/ in 'globalEvBinds', so resolution succeeds and @(-)@'s
    -- 'ddUsedClasses' contains @Num@ — pulling every @Num@ method
    -- into the dep set even from monomorphic tests like
    -- @factorial 5 == 120@ that go via @(-)@.  Documented as a known
    -- over-approximation.
  , testCase "Num Foo fromInteger edit does NOT invalidate add 1 2 == 3" $
      assertFingerprintUnchanged
        (unwords ["add", "1", "2", "==", "3"])
        "test/Instances.hs"
        "Instances.hie"
        "Foo (fromInteger n)"
        "Foo (fromInteger n + 100)"

    -- Multi-class case A: editing the @Greet Foo@ method invalidates a
    -- test that uses both @Greet@ and @Insult@ via @roast@.  The
    -- class-edge BFS pulls in /every/ class on the test body's
    -- evidence frontier, not only the first.
  , testCase "Greet Foo edit invalidates roast (Foo 3)" $
      assertFingerprintChanged
        (unwords ["roast", "(Foo", "3)", "uses", "both", "Greet",
                  "and", "Insult", "instances"])
        "test/Instances.hs"
        "Instances.hie"
        "\"Hello Foo \""
        "\"Howdy Foo \""

    -- Multi-class case B: same test body, edit the *other* class's
    -- instance.  Same outcome.  Together with case A this proves
    -- multi-class evidence is not collapsed to a single class.
  , testCase "Insult Foo edit invalidates roast (Foo 3)" $
      assertFingerprintChanged
        (unwords ["roast", "(Foo", "3)", "uses", "both", "Greet",
                  "and", "Insult", "instances"])
        "test/Instances.hs"
        "Instances.hie"
        "\" is silly\""
        "\" is splendid\""

    -- Explicit @forall@ ordering matters semantically under
    -- @TypeApplications@ — @pair @Int @String@ binds different
    -- variables depending on whether the signature is
    -- @forall a b@ or @forall b a@ — so a swap of forall order has
    -- to invalidate the cache.  This relies on the type-signature
    -- bytes being part of the dep hash.
  , testCase "swapping forall a b -> forall b a invalidates pair test" $
      assertFingerprintChanged
        (unwords ["pair", "returns", "a", "tuple", "of", "its", "arguments"])
        "test/Polymorphic.hs"
        "Polymorphic.hie"
        "forall a b. a -> b -> (a, b)"
        "forall b a. a -> b -> (a, b)"
  ]

-- | Compute fingerprints for @leafName@ before and after applying the
-- given source-byte substitution to @hieFile@'s in-memory copy of
-- @hsFile@.
fingerprintBeforeAfter
  :: String       -- leaf name of the test
  -> FilePath     -- on-disk source file (read for the original bytes)
  -> FilePath     -- HIE filename to override (key into the overrides map)
  -> String       -- needle: substring to replace
  -> String       -- replacement
  -> IO (Maybe Fingerprint, Maybe Fingerprint)
fingerprintBeforeAfter leafName hsFile hieFile needle replacem = do
  let cabalHash = Fingerprint 0 0
      hieDir    = ".hie"
      needleBs  = BSC.pack needle
      replBs    = BSC.pack replacem
  origBytes <- BS.readFile hsFile
  let mutated = case BSC.breakSubstring needleBs origBytes of
        (before, rest)
          | BS.null rest -> error
              ("fixture broken: needle '" ++ needle ++
               "' not in " ++ hsFile)
          | otherwise ->
              before <> replBs <> BS.drop (BS.length needleBs) rest
      overrides = Map.singleton hieFile mutated
  fp1 <- internalComputeFingerprint hieDir Map.empty cabalHash leafName
  fp2 <- internalComputeFingerprint hieDir overrides cabalHash leafName
  return (fp1, fp2)

-- | Assert that mutating the given source needle changes the
-- fingerprint of the named test.
assertFingerprintChanged
  :: String -> FilePath -> FilePath -> String -> String -> Assertion
assertFingerprintChanged leafName hsFile hieFile needle replacem = do
  (fp1, fp2) <- fingerprintBeforeAfter leafName hsFile hieFile needle replacem
  case (fp1, fp2) of
    (Just a, Just b) ->
      assertBool
        ("fingerprint of \"" ++ leafName ++
         "\" should change after editing '" ++ needle ++
         "' but got " ++ show a ++ " == " ++ show b)
        (a /= b)
    _ ->
      assertFailure
        ("could not compute fingerprint for \"" ++ leafName ++
         "\": fp1=" ++ show fp1 ++ ", fp2=" ++ show fp2)

-- | Assert that mutating the given source needle does /not/ change
-- the fingerprint of the named test.
assertFingerprintUnchanged
  :: String -> FilePath -> FilePath -> String -> String -> Assertion
assertFingerprintUnchanged leafName hsFile hieFile needle replacem = do
  (fp1, fp2) <- fingerprintBeforeAfter leafName hsFile hieFile needle replacem
  case (fp1, fp2) of
    (Just a, Just b) ->
      assertBool
        ("fingerprint of \"" ++ leafName ++
         "\" should NOT change after editing '" ++ needle ++
         "' but got " ++ show a ++ " /= " ++ show b)
        (a == b)
    _ ->
      assertFailure
        ("could not compute fingerprint for \"" ++ leafName ++
         "\": fp1=" ++ show fp1 ++ ", fp2=" ++ show fp2)

-- A pure model of the @transitiveDeps@ BFS: starting from @start@, visit
-- every name reachable through @declMap@ (name → names used in its body).
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
