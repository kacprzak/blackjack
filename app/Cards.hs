-- | Data representing Blackjack game cards
module Cards where

import Control.Monad
import Data.Char
import Data.Functor.Classes
import Data.List
import Data.Monoid

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
    allRanks = (Numeral <$> [2 .. 10]) ++ [Jack, Queen, King, Ace]
    allSuits = [Club, Diamond, Heart, Spade]

cardValue :: Card -> [Int]
cardValue (Card (Numeral v) _) = [v]
cardValue (Card Ace _) = [1, 11]
cardValue _ = [10]

-- Returns possible scores from hand. Maybe more than one because of Ace.
scores :: [Card] -> [Int]
scores [] = [0]
scores cs = sort . nub . foldl1' (liftM2 (+)) . map cardValue $ cs

newtype Score = Score {getScore :: Either Int Int} deriving (Eq)

instance Ord Score where
  compare (Score (Left s1)) (Score (Left s2)) = EQ
  compare (Score s1) (Score s2) = compare2 s1 s2

instance Show Score where
  show (Score (Left s)) = show s
  show (Score (Right s)) = show s

-- Returns best score from hand.
score :: [Card] -> Score
score h =
  if null valid
    then Score . Left $ minimum busted
    else Score . Right $ maximum valid
  where
    (valid, busted) = span (<= 21) $ scores h

blackjack :: Score
blackjack = Score $ Right 21

isBusted :: Score -> Bool
isBusted (Score (Left _)) = True
isBusted _ = False
