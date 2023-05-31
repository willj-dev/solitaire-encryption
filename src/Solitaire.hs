module Solitaire (encrypt, decrypt, encode, keystream, letterPosition, addLetters) where

import Deck
import Data.Char (isUpper, isLower, toUpper, ord)

encrypt :: String -> Deck -> (String, Deck)
encrypt s d = undefined

decrypt :: String -> Deck -> (String, Deck)
decrypt s d = undefined

-- generates n keystream values from the given deck. the deck should be a full 54-card deck, including
-- both jokers; this will break if either joker is missing, and it will generate nonsense if
-- any of the non-Joker cards are missing or duplicated.
keystream :: Int -> Deck -> ([Int], Deck)
keystream n d = undefined

-- converts a string into a list of ints, by throwing away non-alphabetic characters and
-- converting letters to their positional values (A = 1, B = 2, ..., Z = 26), ignoring case
encode :: String -> [Int]
encode "" = []
encode (c:cs)
    | isUpper c = letterPosition c : encode cs
    | isLower c = letterPosition (toUpper c) : encode cs
    | otherwise = encode cs -- throw away this character

-- converts an uppercase letter to its positional value: A = 1, B = 2, ..., Z = 26
-- produces nonsense for characters that aren't uppercase letters from the English alphabet (ascii 65 thru 90)
letterPosition :: Char -> Int
letterPosition c = 1 + (ord c - ord 'A')

-- adds two numbers that are assumed to correspond to letters (i.e. be between 1 and 26 inclusive),
-- wrapping around so that e.g. 'A + C = D' and 'Z + C = C'
addLetters :: Int -> Int -> Int
addLetters a b
    | a + b > 26 = a + b - 26
    | otherwise  = a + b