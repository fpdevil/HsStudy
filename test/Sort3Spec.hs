{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module         : Sort3Spec
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT (change this as needed)
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    :
--
-----------------------------------------------------------------------------
module Sort3Spec (main, spec) where

import qualified Sort3
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "head" $ do
    it "test removing first list element" $ do
      head [1,2,3,4] `shouldBe` 1
      head ["the", "dog", "ran"] `shouldBe` "dog" -- should fail
  -- describe "Sort3" $ do
  --   prop "sort3 sorts correctly" $ do
  --     \(triple :: (Int, Int, Int)) ->
  --       let (a0', a1', a2') = Sort3.sort3 triple
  --       in a0' <= a1' && a1' <= a2'
