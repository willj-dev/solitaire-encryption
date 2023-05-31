module Main (main) where

import Deck
import Shuffle
import System.Random (initStdGen)

main :: IO ()
main = do
  gen <- initStdGen
  print orderedDeck
  print $ cardValue <$> orderedDeck
  print $ shuffledDeck gen
