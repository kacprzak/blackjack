-- | Data representing Blackjack game cards

module Cards where

import Control.Monad
import Data.List
import Data.Char
import Data.Functor.Classes

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
frenchDeck = Card <$> allRanks <*>  allSuits
  where
    allRanks = (Numeral <$> [2..10]) ++ [Jack, Queen, King, Ace]
    allSuits = [Club, Diamond, Heart, Spade]

cardValue :: Card -> [Int]
cardValue (Card (Numeral v) _) = [v]
cardValue (Card Ace _) = [1, 11]
cardValue _ = [10]

scores :: [Card] -> [Int]
scores = sort . nub . foldr1 (liftM2 (+)) . map cardValue

score :: [Card] -> Either Int Int
score h = if null valid
          then Left $ minimum busted
          else Right $ maximum valid
  where
    (valid, busted) = span (<=21) $ scores h

compareScore :: Either Int Int -> Either Int Int -> Ordering
compareScore (Left _) (Left _) = EQ
compareScore x y = compare2 x y
