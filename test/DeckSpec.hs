{-# OPTIONS_GHC -Wno-unused-imports #-}

module DeckSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Deck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cardValue" $ do
    it "correctly values the ordered deck" $ do
      cardValue <$> orderedDeck `shouldBe` [1 .. 53] ++ [53]

  describe "moveCard" $ do
    it "correctly moves a card down" $ moveCard (spade Two) 2 spadesOneThroughFive
      `shouldBe` spades [Ace, Three, Four, Two, Five]
    it "correctly wraps a card around" $ moveCard (spade Four) 2 spadesOneThroughFive
      `shouldBe` spades [Ace, Four, Two, Three, Five]

  describe "jokersTripleCut" $ do
    it "should do a triple cut" $ jokersTripleCut (insertJokers (spades [Ace, Two]) (spades [Three, Four]) (spades [Five, Six]))
      `shouldBe` insertJokers (spades [Five, Six]) (spades [Three, Four]) (spades [Ace, Two])
    it "should not depend on joker order" $ jokersTripleCut (insertJokers' (spades [Ace, Two]) (spades [Three, Four]) (spades [Five, Six]))
      `shouldBe` insertJokers' (spades [Five, Six]) (spades [Three, Four]) (spades [Ace, Two])
    it "should handle empty left" $ jokersTripleCut (insertJokers [] (spades [Three, Four]) (spades [Five, Six]))
      `shouldBe` insertJokers (spades [Five, Six]) (spades [Three, Four]) []
    it "should handle empty right" $ jokersTripleCut (insertJokers (spades [Ace, Two]) (spades [Three, Four]) [])
      `shouldBe` insertJokers [] (spades [Three, Four]) (spades [Ace, Two])
    it "should handle empty middle" $ jokersTripleCut (insertJokers (spades [Ace, Two]) [] (spades [Five, Six]))
      `shouldBe` insertJokers (spades [Five, Six]) [] (spades [Ace, Two])
    it "should handle empty ends" $ jokersTripleCut (insertJokers [] (spades [Three, Four]) [])
      `shouldBe` insertJokers [] (spades [Three, Four]) []

  describe "countCut" $ do
    it "should do the thing" $ do
      countCut (clubs [Four, Five, Ace, Two, Three]) `shouldBe` clubs [Two, Four, Five, Ace, Three]

  describe "countCard" $ do
    it "should work" $ do
      countCard (clubs [Four, Five, Ace, Two, Three]) `shouldBe` Card Club Two

spade :: Rank -> Card
spade = Card Spade

spades :: [Rank] -> Deck
spades = map (Card Spade)

clubs :: [Rank] -> Deck
clubs = map (Card Club)

spadesOneThroughFive :: Deck
spadesOneThroughFive = Card Spade <$> [Ace, Two, Three, Four, Five]

insertJokers :: Deck -> Deck -> Deck -> Deck
insertJokers l c r = l ++ JokerA : c ++ JokerB : r

insertJokers' :: Deck -> Deck -> Deck -> Deck
insertJokers' l c r = l ++ JokerB : c ++ JokerA : r