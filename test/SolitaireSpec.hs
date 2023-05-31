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

    describe "addLetters" $ do
        it "adds A + E = F" $ do
            addLetters (letterPosition 'A') (letterPosition 'E') `shouldBe` letterPosition 'F'
        it "adds T + Q = K" $ do
            addLetters (letterPosition 'T') (letterPosition 'Q') `shouldBe` letterPosition 'K'