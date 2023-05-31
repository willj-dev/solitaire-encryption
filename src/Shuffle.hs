{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Shuffle (shuffledDeck) where

import Deck
import System.Random (RandomGen, randomR)

shuffledDeck :: RandomGen g => g -> ([Card], g)
shuffledDeck g = shuffleList g orderedDeck

-- Shuffles an entire list (must be finite) with the given RandomGen, and gives back a new generator
shuffleList :: RandomGen g => g -> [a] -> ([a], g)
shuffleList g xs = shuffleList' g (length xs) xs

-- Shuffles the first n elements of a list with the given RandomGen, and gives back a new generator.
-- The list need not be finite, but it does need to have at least length n.
-- Note also that if the generator isn't actually used, the same one is returned
-- This algorithm is probably pretty inefficient, relying on many list concats
-- ...but since the list isn't really that big, probably not a big deal
shuffleList' :: RandomGen g => g -> Int -> [a] -> ([a], g)
shuffleList' _ n _ | n < 0 = error "shuffleList' not defined for negative n"
shuffleList' g 0 xs = (xs, g)
shuffleList' g 1 xs = (xs, g)
shuffleList' g n xs = (x : shuffledRest, g'') where
    (i, g') = randomR (0, n - 1) g
    (xsL, x : xsR) = splitAt i xs
    (shuffledRest, g'') = shuffleList' g' (n - 1) (xsL ++ xsR)
