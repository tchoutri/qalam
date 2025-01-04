module Main where

import Test.Tasty

main :: IO ()
main = defaultMain . testGroup "Qalam Tests" $ specs

specs :: [TestTree]
specs = []
