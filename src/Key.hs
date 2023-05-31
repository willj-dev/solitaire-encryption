module Key (passphrase, passphrase') where

import Deck
import Solitaire ((|>), encode)

-- TODO: various functions for generating a Deck from a given key, such as a passphrase
passphrase :: String -> Deck
passphrase = passphrase' orderedDeck

-- Keys a passphrase into an existing deck
passphrase' :: Deck -> String -> Deck
passphrase' d = foldr addPassphraseChar d . encode

addPassphraseChar :: Int -> Deck -> Deck
addPassphraseChar i = moveCard JokerA 1 |> moveCard JokerB 2 |> jokersTripleCut |> countCut |> countCut' i
