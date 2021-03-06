-- | Data representing Blackjack game cards

module Cards where

import Control.Monad
import Data.List
import Data.Char

data Suit = Club | Diamond | Heart | Spade

instance Show Suit where
  show Club = [toEnum 9827]
  show Diamond = [toEnum 9830]
  show Heart = [toEnum 9829]
  show Spade = [toEnum 9824]

data Rank = Numeral Int | Jack | Queen | King | Ace

instance Show Rank where
  show (Numeral v) = show v
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card = Card Rank Suit

instance Show Card where
  show (Card r s) = show r ++ show s

frenchDeck :: [Card]
frenchDeck = Card <$> allRanks <*> allSuits
  where
    allRanks = (map Numeral [2..10]) ++ [Jack, Queen, King, Ace]
    allSuits = [Club, Diamond, Heart, Spade]

cardValue :: Card -> [Int]
cardValue (Card (Numeral v) _) = [v]
cardValue (Card Ace _) = [1, 11]
cardValue _ = [10]

score :: [Card] -> [Int]
score = sort . nub . foldr1 (liftM2 (+)) . map cardValue

bestScore :: [Card] -> Int
bestScore h = if null valid
              then minimum busted
              else maximum valid
  where
    (valid, busted) = span (<=21) $ score h
