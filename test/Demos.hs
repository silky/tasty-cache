-- | Demonstration tests: a tour of what the cache supports and where
-- it breaks.
--
-- Two flavours:
--
--   * /Cacheable demos/ — wrapped in 'cacheable'.  On the second
--     'cabal test' run these print @OK (cached)@; editing a
--     dependency invalidates them.  Useful as visual confirmation
--     that the cache is engaged for a particular dependency pattern
--     (direct, transitive, GADT, class-edge, per-extension).
--
--   * /Always-run demos/ — not wrapped.  Either intentionally exempt
--     from caching (mutually recursive bindings, Template Haskell,
--     CPP, multi-line pragmas) or they are anchor 'testCase's whose
--     names have to live in this module's HIE source so the
--     fingerprint-mutation tests in 'Library' can find them via
--     @findSubstring@.
--
-- This module also serves as the source of truth for /which/ leaf
-- names the @Library@ mutation tests target — every 'testCase' label
-- here is a potential 'findSubstring' anchor.
module Demos (demoTests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           AnyClassFix         (greetAnyClass)
import           Arithmetic          (add10, add5, timesBy3)
import           CPPDemo             (cppVersion, scaled)
import           DefSigFix           (defSigResult)
import           Diamond             (base, combined, partA, partB)
import           DKindsFix           (dKindsResult)
import           Expr                (Expr (..), eval, pretty)
import           GNDFix              (gndResult)
import           ImpParFix           (impParResult)
import           Instances           (Foo (..))
import           Lib                 (add, factorial)
import           MultiLinePragmaFix  (mlpResult)
import           OvLabFix            (Lbl (..), sampleOvLabel)
import           OvListFix           (L (..), sampleOvList)
import           OvStrFix            (S (..), greetOvStr)
import           Parity              (collatz, isEven, isOdd)
import           PatSynFix           (patSynResult)
import           Polymorphic         (pair, poly, roast)
import           RebindFix           (rebindIf)
import           StdDerivFix         (stdDerivResult)
import           Test.Tasty.HieCache (cacheable)
import           TyFamFix            (tyFamResult)
import           ViaFix              (viaResult)

demoTests :: TestTree
demoTests = testGroup "Demos"
  [ -- ---------------------------------------------------------------
    -- Things the cache supports (cacheable demos)
    -- ---------------------------------------------------------------

    -- Direct dependency: editing 'factorial' re-runs the factorial
    -- test; 'add' tests stay cached.
    cacheable $ testGroup "Direct dependency (Lib)"
      [ testCase "add 1 2 == 3"       $ add 1 2     @?= 3
      , testCase "add 0 0 == 0"       $ add 0 0     @?= 0
      , testCase "factorial 5 == 120" $ factorial 5 @?= 120
      ]

    -- GADT independence: 'eval' and 'pretty' occupy distinct source
    -- spans; editing one does not invalidate tests of the other.
  , cacheable $ testGroup "GADT independence (Expr)"
      [ testCase "eval Lit"          $ eval (Lit 42)                                    @?= 42
      , testCase "eval Add"          $ eval (Add (Lit 3) (Lit 4))                       @?= 7
      , testCase "eval If true"      $ eval (If (BoolLit True)  (Lit 1) (Lit 2))        @?= 1
      , testCase "eval If false"     $ eval (If (BoolLit False) (Lit 1) (Lit 2))        @?= 2
      , testCase "eval Eq true"      $ eval (Eq (Lit 3) (Lit 3))                        @?= True
      , testCase "eval Eq false"     $ eval (Eq (Lit 3) (Lit 4))                        @?= False
      , testCase "eval nested If/Eq" $ eval (If (Eq (Lit 2) (Lit 2)) (Lit 42) (Lit 0))  @?= 42
      , testCase "pretty Lit"        $ pretty (Lit 5)                                    @?= "5"
      , testCase "pretty Add"        $ pretty (Add (Lit 1) (Lit 2))                      @?= "(1 + 2)"
      , testCase "pretty nested Add" $ pretty (Add (Lit 1) (Add (Lit 2) (Lit 3)))        @?= "(1 + (2 + 3))"
      , testCase "pretty Eq"         $ pretty (Eq (Lit 1) (Lit 2))                       @?= "(1 == 2)"
      ]

    -- Class-edge BFS: 'poly' uses 'Num' implicitly; the BFS chases the
    -- evidence chain into 'instance Num Foo' (in Instances.hs), so
    -- editing that instance correctly invalidates this group.
  , cacheable $ testGroup "Class-edge BFS (Polymorphic)"
      [ testCase "poly @Foo 3 == Foo 10" $ poly (Foo 3) @?= Foo 10
      , testCase "roast (Foo 3) uses both Greet and Insult instances" $
          roast (Foo 3) @?= "Hello Foo 3 and Foo 3 is silly"
      , testCase "pair returns a tuple of its arguments" $
          pair (1 :: Int) ("x" :: String) @?= (1, "x")
      ]

    -- Diamond: combined → partA/partB → base.  Editing 'base'
    -- invalidates all four tests via the transitive BFS.
  , cacheable $ testGroup "Diamond dependencies (Diamond)"
      [ testCase "base 5 == 6"      $ base 5     @?= 6
      , testCase "base 0 == 1"      $ base 0     @?= 1
      , testCase "partA 3 == 5"     $ partA 3    @?= 5
      , testCase "partA 0 == 2"     $ partA 0    @?= 2
      , testCase "partB 3 == 8"     $ partB 3    @?= 8
      , testCase "combined 3 == 13" $ combined 3 @?= 13
      ]

    -- Per-extension demos.  Each fixture exercises a specific
    -- implicit-dispatch or synthesised-binding path; the corresponding
    -- mutation tests in 'Library' use these testCase names as
    -- 'findSubstring' anchors.
  , cacheable $ testGroup "Per-extension implicit dispatch"
      [ testCase "OvStr greet hello"            $ greetOvStr     @?= S "ovstr:world"
      , testCase "OvList sample 1 2 3"          $ sampleOvList   @?= L [1,2,3,99999]
      , testCase "OvLab sample k"               $ sampleOvLabel  @?= Lbl "ovlabel-k"
      , testCase "Rebind if 1 else 2"           $ rebindIf       @?= 1 + 1000
      , testCase "DefSig describe 42"           $ defSigResult   @?= "defsig-prefix DefSigUser 42"
      , testCase "AnyClass hello AnyClass"      $ greetAnyClass  @?= "anyclass-greeting"
      , testCase "Via shout ViaThing 7"         $ viaResult      @?= "via-loud-prefix 7"
      , testCase "GND bark GNDThing 5"          $ gndResult      @?= "gnd-bark 5"
      , testCase "StdDeriv show StdBranch"      $ stdDerivResult @?= "stdderiv-prefix StdBranch StdLeaf StdLeaf"
      , testCase "PatSyn Bin 7"                 $ patSynResult   @?= 7 + 5000
      , testCase "TyFam listElem 1 2 3"         $ tyFamResult    @?= 1 + 7000
      , testCase "DKinds describeTag Foo"       $ dKindsResult   @?= "dkinds-foo-prefix 5"
      , testCase "ImpPar greetImpl world"       $ impParResult   @?= "imppar-hello world"
      ]

    -- ---------------------------------------------------------------
    -- Things the cache deliberately does not catch (always-run demos)
    -- ---------------------------------------------------------------

    -- Mutual recursion: 'isEven' calls 'isOdd' calls 'isEven'.  Either
    -- function reaching the BFS pulls in the other, so the dep set is
    -- the same regardless of which function the test names — and
    -- editing either invalidates both.  Left unwrapped here as a demo
    -- of a pattern where caching adds little.
  , testGroup "Mutual recursion (always runs)"
      [ testCase "isEven 0"          $ isEven 0   @?= True
      , testCase "isEven 4"          $ isEven 4   @?= True
      , testCase "isEven 7 is False" $ isEven 7   @?= False
      , testCase "isOdd 1"           $ isOdd 1    @?= True
      , testCase "isOdd 7"           $ isOdd 7    @?= True
      , testCase "isOdd 4 is False"  $ isOdd 4    @?= False
      , testCase "collatz 1 == 0"    $ collatz 1  @?= 0
      , testCase "collatz 6 == 8"    $ collatz 6  @?= 8
      , testCase "collatz 27 == 111" $ collatz 27 @?= 111
      ]

    -- Template Haskell: splice-generated bindings are intentionally
    -- excluded from caching because their dependency on the template
    -- itself isn't recorded as a regular HIE Use edge.
  , testGroup "Template Haskell (always runs)"
      [ testCase "add5 3 == 8"      $ add5 3     @?= 8
      , testCase "add10 7 == 17"    $ add10 7    @?= 17
      , testCase "timesBy3 4 == 12" $ timesBy3 4 @?= 12
      ]

    -- CPP: HIE source is pre-CPP but spans are post-CPP, so the
    -- declaration spans for 'scaled' / 'cppVersion' don't cover the
    -- @#define@ lines.  The whole-file CPP detection in
    -- 'transitiveDeps' hashes the full source for any @#define@-using
    -- file, but this cohort is left unwrapped to make the limitation
    -- visible.
  , testGroup "CPP (always runs)"
      [ testCase "scaled 4 == 12" $ scaled 4   @?= 12
      , testCase "scaled 0 == 0"  $ scaled 0   @?= 0
      , testCase "cppVersion"     $ cppVersion @?= "v3"
      ]

    -- Multi-line pragmas: the pragma-line filter and the
    -- implicit-dispatch heuristic both operate on lines beginning with
    -- @{-#@.  A multi-line @{-# LANGUAGE … #-}@ block escapes both,
    -- so editing a continuation line does not invalidate.  The
    -- 'Library' suite has the assertFingerprintUnchanged test that
    -- pins this behaviour; here we just demonstrate that the fixture
    -- itself works.
  , testGroup "Multi-line pragma (limit demonstration, always runs)"
      [ testCase "MLP zero" $ mlpResult 0 @?= "mlp-zero"
      ]
  ]
