module Main (main) where

import           Test.Tasty
import           Test.Tasty.HieCache (defaultMainWithHieCache)

import           Demos               (demoTests)
import           Library             (libraryTests)

main :: IO ()
main = defaultMainWithHieCache (testGroup "tasty-cache" [libraryTests, demoTests])
