module Main (main) where

import           Test.Tasty
import           Test.Tasty.HieCache (cacheable, defaultMainWithHieCache)
import           Test.Tasty.HUnit

import           Arithmetic          (add10, add5, timesBy3)
import           CPPDemo             (cppVersion, scaled)
import           Diamond             (base, combined, partA, partB)
import           Expr                (Expr (..), eval, pretty)
import           Lib                 (add, factorial)
import           Parity              (collatz, isEven, isOdd)

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
  ]
