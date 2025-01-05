{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Orphans where

import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck

import Qalam.Model.Document.Types

instance Arbitrary DocumentData where
  arbitrary = DocumentData <$> arbitraryLowerCaseText 3 20

arbitraryLowerCaseText :: Int -> Int -> Gen Text
arbitraryLowerCaseText lowerBound higherBound = do
  l <- choose (lowerBound, higherBound)
  fmap Text.pack <$> vectorOf l $ elements ['a' .. 'z']
