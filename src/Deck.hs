{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Deck (
    Suit(..), Rank(..), Card(..), Deck, orderedDeck, cardValue,
    moveCard, insertCardAt, jokersTripleCut, countCut, countCard
) where

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Ord)

instance Show Suit where
    show Club       = "♣"
    show Diamond    = "♦"
    show Heart      = "♥"
    show Spade      = "♠"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord)

instance Show Rank where
    show Ace    = "A"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"
    show Six    = "6"
    show Seven  = "7"
    show Eight  = "8"
    show Nine   = "9"
    show Ten    = "10"
    show Jack   = "J"
    show Queen  = "Q"
    show King   = "K"

data Card = Card Suit Rank | JokerA | JokerB deriving (Eq, Ord)

isJoker :: Card -> Bool
isJoker (Card _ _) = False
isJoker _ = True

type Deck = [Card]

instance Show Card where
    show (Card s r) = show r ++ show s
    show JokerA     = "JokerA"
    show JokerB     = "JokerB"

orderedDeck :: Deck
orderedDeck = foldr addSuit [JokerA, JokerB] suits
    where
        addSuit suit deck = suitCards suit ++ deck
        suitCards suit = Card suit <$> ranks

suits :: [Suit]
suits = [Club, Diamond, Heart, Spade]

ranks :: [Rank]
ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

cardValue :: Card -> Int
cardValue (Card s r)    = suitValue s + rankValue r
cardValue JokerA        = 53
cardValue JokerB        = 53 -- both jokers have the same value

suitValue :: Suit -> Int
suitValue Club      = 0
suitValue Diamond   = 13
suitValue Heart     = 26
suitValue Spade     = 39

rankValue :: Rank -> Int
rankValue Ace   = 1
rankValue Two   = 2
rankValue Three = 3
rankValue Four  = 4
rankValue Five  = 5
rankValue Six   = 6
rankValue Seven = 7
rankValue Eight = 8
rankValue Nine  = 9
rankValue Ten   = 10
rankValue Jack  = 11
rankValue Queen = 12
rankValue King  = 13

-- Deck operations relevant to the Solitaire algorithm

-- moves the (first instance of the) given card down the deck the given number of spaces, wrapping around to the top of the deck
-- if the bottom is reached. The given card is assumed to exist in the deck, and the number of spaces to move the card must be
-- less than the size of the deck
moveCard :: Card -> Int -> Deck -> Deck
moveCard c n d = let
    (l, r) = break (== c) d
    r' = tail r
    in if n <= length r'
        then l ++ insertCardAt c n r' -- insert n cards down towards the end
        else insertCardAt c (n - length r') l ++ r' -- wrap around from the beginning

-- inserts a card at the given position in the deck; `insertCardAt 0 2 [1, 2, 3, 4] = [1, 2, 0, 3, 4]`
insertCardAt :: Card -> Int -> Deck -> Deck
insertCardAt c n d = let (l, r) = splitAt n d in l ++ c : r

-- a deck like (cards 1) Joker (cards 2) Joker (cards 3) becomes (cards 3) Joker (cards 2) Joker (cards 1)
-- the set of cards at the beginning or end can be empty; if both are empty, the deck is unchanged.
-- Two jokers must be in the deck.
jokersTripleCut :: Deck -> Deck
jokersTripleCut d = let
    (l, jcjr) = break isJoker d -- Left and Joker1|Center|Joker2|Right
    j1 : cjr = jcjr
    (c, jr) = break isJoker cjr
    j2 : r = jr

    in r ++ (j1 : c) ++ (j2 : l)

-- cuts the deck at the location indicated by the last card's value (1--53, see `cardValue` above),
-- leaving the last card in place. So if the last card's value is 3, a deck in order (1 2 3 ... 52 53 54)
-- becomes (4 5 6 ... 52 53 1 2 3 54).
-- Deck must have all 54 cards - or, more precisely, it must have at least as many cards as the maximum cardValue present.
countCut :: Deck -> Deck
countCut d = let
    i = cardValue (last d)
    (l, r) = splitAt i d
    in init r ++ l ++ [last r]

-- for a given deck, gets the value x of the top card, then gets the value of the (x + 1)th card down from the top.
countCard :: Deck -> Card
countCard d = let
    i = cardValue (head d)
    in d !! (i - 1)
