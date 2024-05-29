module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Properties

main :: IO ()
main = do
  let allTests = [Properties.tests]
  defaultMain (testGroup "tests" allTests)
