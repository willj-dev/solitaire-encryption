module Solitaire (encrypt, decrypt, encode, keystream, pos, ltr, (|>), (+*), (-*)) where

import Deck
import Data.Char (toUpper, ord, chr, isAsciiUpper, isAsciiLower)

encrypt :: String -> Deck -> (String, Deck)
encrypt s d = let
    clear = encode s
    (ks, d') = keystream (length clear) d
    cipher = ltr <$> zipWith (+*) clear ks
    in (cipher, d')

decrypt :: String -> Deck -> (String, Deck)
decrypt s d = let
    cipher = pos <$> s
    (ks, d') = keystream (length cipher) d
    clear = ltr <$> zipWith (-*) cipher ks
    in (clear, d')

-- reverse composition for easier reading left to right
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f

-- generates n keystream values from the given deck. the deck should be a full 54-card deck, including
-- both jokers; this will break if either joker is missing, and it will generate nonsense if
-- any of the non-Joker cards are missing or duplicated.
keystream :: Int -> Deck -> ([Int], Deck)
keystream 0 d = ([], d)
keystream n d = let
    steps = moveCard JokerA 1 |> moveCard JokerB 2 |> jokersTripleCut |> countCut
    d' = steps d
    outCard = countCard d'
    in if isJoker outCard
        then keystream n d' -- go again without generating output for this step
        else let
            out = cardValue' outCard
            (outTail, d'') = keystream (n - 1) d'
            in (out : outTail, d'')


-- converts a string into a list of ints, by throwing away non-alphabetic characters and
-- converting letters to their positional values (A = 1, B = 2, ..., Z = 26), ignoring case
encode :: String -> [Int]
encode "" = []
encode (c:cs)
    | isAsciiUpper c = pos c : encode cs
    | isAsciiLower c = pos (toUpper c) : encode cs
    | otherwise = encode cs -- throw away this character

-- converts an uppercase letter to its positional value: A = 1, B = 2, ..., Z = 26
-- produces nonsense for characters that aren't uppercase letters from the English alphabet (ascii 65 thru 90)
pos :: Char -> Int
pos c = 1 + (ord c - ord 'A')

ltr :: Int -> Char
ltr i = chr (i + ord 'A' - 1)

-- adds two numbers that are assumed to correspond to letters (i.e. be between 1 and 26 inclusive),
-- wrapping around so that e.g. 'A + C = D' and 'Z + C = C'
(+*) :: Int -> Int -> Int
a +* b
    | a + b > 26 = a + b - 26
    | otherwise  = a + b

-- same thing but in reverse
(-*) :: Int -> Int -> Int
a -* b
    | a > b = a - b
    | otherwise = 26 + a - b
