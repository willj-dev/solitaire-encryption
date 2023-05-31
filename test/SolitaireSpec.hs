{-# OPTIONS_GHC -Wno-unused-imports #-}

module SolitaireSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Solitaire

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "encode" $ do
        it "encodes a string" $ do
            encode "Do Not Use PC!" `shouldBe` [4, 15, 14, 15, 20, 21, 19, 5, 16, 3]

    describe "ltr" $ do
        it "does the thing" $ do
            ltr <$> [1, 5, 13, 26] `shouldBe` "AEMZ"

    describe "addLetters" $ do
        it "adds A + E = F" $ do
            pos 'A' +* pos 'E' `shouldBe` pos 'F'
        it "adds T + Q = K" $ do
            pos 'T' +* pos 'Q' `shouldBe` pos 'K'

    describe "subLetters" $ do
        it "subtracts F - A = E" $ do
            pos 'F' -* pos 'A' `shouldBe` pos 'E'
        it "subtracts K - Q = T" $ do
            pos 'K' -* pos 'Q' `shouldBe` pos 'T'

