module Main (main) where

import           Test.Tasty
import           Test.Tasty.HieCache (cacheable, defaultMainWithHieCache)
import           Test.Tasty.HUnit

import           AnyClassFix         (greetAnyClass)
import           Arithmetic          (add10, add5, timesBy3)
import           CPPDemo             (cppVersion, scaled)
import           DefSigFix           (defSigResult)
import           Diamond             (base, combined, partA, partB)
import           DKindsFix           (dKindsResult)
import           Expr                (Expr (..), eval, pretty)
import           ExtensionCoverage   (extensionCoverageTests)
import           FalseNegatives      (falseNegativeTests)
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
import           TyFamFix            (tyFamResult)
import           ViaFix              (viaResult)

main :: IO ()
main = defaultMainWithHieCache tests

tests :: TestTree
tests = testGroup "scenarios"

  -- Wrapped with cacheable: skipped when source is unchanged.
  [ cacheable $ testGroup "Lib (basic direct dependency)"
    [ testCase "add 1 2 == 3"       $ add 1 2     @?= 3
    , testCase "add 0 0 == 0"       $ add 0 0     @?= 0
    , testCase "factorial 5 == 120" $ factorial 5 @?= 120
    ]

  -- Not wrapped: always runs, regardless of source changes.
  , testGroup "Parity (mutual recursion — always runs)"
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

  -- Wrapped: eval and pretty are independent; editing one does not
  -- invalidate the other.
  , cacheable $ testGroup "Expr (GADT)"
    [ testCase "eval Lit"          $ eval (Lit 42)                                    @?= 42
    , testCase "eval Add"          $ eval (Add (Lit 3) (Lit 4))                       @?= 7
    , testCase "eval If true"      $ eval (If (BoolLit True)  (Lit 1) (Lit 2))        @?= 1
    , testCase "eval If false"     $ eval (If (BoolLit False) (Lit 1) (Lit 2))        @?= 2
    , testCase "eval Eq true"      $ eval (Eq (Lit 3) (Lit 3))                        @?= True
    , testCase "eval Eq false"     $ eval (Eq (Lit 3) (Lit 4))                        @?= False
    , testCase "eval nested If/Eq" $ eval (If (Eq (Lit 2) (Lit 2)) (Lit 42) (Lit 0)) @?= 42
    , testCase "pretty Lit"        $ pretty (Lit 5)                                    @?= "5"
    , testCase "pretty Add"        $ pretty (Add (Lit 1) (Lit 2))                      @?= "(1 + 2)"
    , testCase "pretty nested Add" $ pretty (Add (Lit 1) (Add (Lit 2) (Lit 3)))        @?= "(1 + (2 + 3))"
    , testCase "pretty Eq"         $ pretty (Eq (Lit 1) (Lit 2))                       @?= "(1 == 2)"
    ]

  -- Wrapped: exercises the class-edge BFS for type-class instance
  -- resolution.  The test body names `poly` and `Foo`, but the `Num Foo`
  -- instance lives in `Instances.hs`, and `fromInteger` is invoked only
  -- via type-class resolution at the call site.  The class-edge BFS
  -- chases the evidence-var application back to the instance and folds
  -- its method bodies into the dep hash, so editing the body of
  -- `instance Num Foo` (e.g. `fromInteger`) correctly invalidates this
  -- test.
  , cacheable $ testGroup "Polymorphic (instance resolution via class-edge BFS)"
    [ testCase "poly @Foo 3 == Foo 10" $ poly (Foo 3) @?= Foo 10
    , testCase "roast (Foo 3) uses both Greet and Insult instances" $
        roast (Foo 3) @?= "Hello Foo 3 and Foo 3 is silly"
    , testCase "pair returns a tuple of its arguments" $
        pair (1 :: Int) ("x" :: String) @?= (1, "x")
    ]

  -- Wrapped: transitive BFS means editing 'base' invalidates all four.
  , cacheable $ testGroup "Diamond (transitive deps)"
    [ testCase "base 5 == 6"      $ base 5     @?= 6
    , testCase "base 0 == 1"      $ base 0     @?= 1
    , testCase "partA 3 == 5"     $ partA 3    @?= 5
    , testCase "partA 0 == 2"     $ partA 0    @?= 2
    , testCase "partB 3 == 8"     $ partB 3    @?= 8
    , testCase "combined 3 == 13" $ combined 3 @?= 13
    ]

  -- Not wrapped: always runs.
  , testGroup "Arithmetic (Template Haskell — always runs)"
    [ testCase "add5 3 == 8"      $ add5 3     @?= 8
    , testCase "add10 7 == 17"    $ add10 7    @?= 17
    , testCase "timesBy3 4 == 12" $ timesBy3 4 @?= 12
    ]

  -- Not wrapped: always runs.
  , testGroup "CPPDemo (C preprocessor — always runs)"
    [ testCase "scaled 4 == 12" $ scaled 4   @?= 12
    , testCase "scaled 0 == 0"  $ scaled 0   @?= 0
    , testCase "cppVersion"     $ cppVersion @?= "v3"
    ]

  -- False-negative demonstration tests (always runs).
  , falseNegativeTests

  -- Extension-coverage fixtures: each testCase here is the leaf-name
  -- target for a mutation test in 'extensionCoverageTests'.  Not wrapped
  -- in 'cacheable'; behaviour is verified end-to-end on every run, and
  -- the testCase string must be present in Main.hie for findSubstring.
  , testGroup "Extension fixtures"
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
    , testCase "MLP zero"                     $ mlpResult 0    @?= "mlp-zero"
    ]

  -- Extension-coverage mutation/sanity tests.
  , extensionCoverageTests
  ]
