import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State.Lazy
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

cardValue :: Card -> [Int]
cardValue (Card (Numeral v) _) = [v]
cardValue (Card Ace _) = [1, 11]
cardValue _ = [10]

data Score = Busted { score :: Int } | Valid { score :: Int, isSoft :: Bool }

instance Eq Score where
  (==) (Busted _) (Busted _) = True
  (==) (Valid ls _) (Valid rs _) = ls == rs
  (==) _ _ = False

instance Ord Score where
  compare (Busted _) (Busted _) = EQ
  compare (Valid ls _) (Valid rs _) = compare ls rs
  compare (Busted _) (Valid _ _) = LT
  compare (Valid _ _) (Busted _) = GT

instance Show Score where
  show (Valid s True) = show (s-10) ++ "/" ++ show s
  show s = show . score $ s

isBusted :: Score -> Bool
isBusted (Busted _) = True
isBusted _ = False

handScore :: [Card] -> Score
handScore h = if null valid
              then Busted $ minimum busted
              else Valid (maximum valid) (length valid > 1)
  where
    (valid, busted) = span (<=21) . sort . nub . foldr1 (liftM2 (+)) . map cardValue $ h

type Deck = [Card]

getCard :: State Deck Card
getCard = do
  s <- get
  put $ tail s
  return $ head s

getCards :: Int -> State Deck [Card]
getCards x = do
  s <- get
  put (drop x s)
  return (take x s)

data PlayerAction = None | Hit | Stand | DoubleDown | Split | Surrender deriving (Eq, Show)
data GameResult = Unfinished | Win | Lose | Push deriving (Eq, Show)

isPlayerFinished :: Player -> Bool
isPlayerFinished p = isFinished $ lastAction p
  where
    isFinished Stand = True
    isFinished DoubleDown = True
    isFinished Surrender = True
    isFinished _ = False

data Player = Player {
  hand :: [Card],
  lastAction :: PlayerAction
}

hit :: Player -> State Deck Player
hit p = do
  c <- getCard
  return $ p { hand = (c:(hand p))
             , lastAction = Hit }

data Table = Table {
  deck :: Deck,
  player :: Player,
  casino :: Player }

instance Show Table where
  show s =
    (showPlayer "Casino" (hand . casino $ s) (lastAction . casino $ s)) ++ "\n" ++
    (showPlayer "Player" (hand . player $ s) (lastAction . player $ s))
    where
      showPlayer name cards lastAction =
        name ++ ": " ++ showCards cards ++ "\t(" ++ showValue cards ++ ") " ++ show lastAction
      showCards = intercalate " " . map show . reverse
      showValue = show . handScore

newTable :: Deck -> Table
newTable d = Table { deck = newDeck
                            , player = Player h1 None
                            , casino = Player h2 None }
  where deal = do
               h1 <- getCards 2
               h2 <- getCards 2
               return (h1, h2)
        ((h1, h2), newDeck) = runState deal d

frenchDeck :: Deck
frenchDeck = Card <$> allRanks <*> allSuits
  where
    allRanks = (map Numeral [2..10]) ++ [Jack, Queen, King, Ace]
    allSuits = [Club, Diamond, Heart, Spade]

safeRead :: String -> Maybe PlayerAction
safeRead "h" = Just Hit
safeRead "s" = Just Stand
safeRead "d" = Just DoubleDown
safeRead "p" = Just Split
safeRead "u" = Just Surrender
safeRead _ = Nothing

-- STATE TRANSITIONS

playerAction :: PlayerAction -> Player -> Player
playerAction a p = p { lastAction = a }

playerHit :: Table -> Table
playerHit gs = gs { deck = newDeck, player = newPlayer }
  where (newPlayer, newDeck) = runState (hit (player gs)) (deck gs)

gamePlayerAction :: PlayerAction -> Table -> Table
gamePlayerAction a s = s { player = playerAction a (player s) }

casinoHit :: Table -> Table
casinoHit gs = gs { deck = newDeck, casino = newCasino }
  where (newCasino, newDeck) = runState (hit (casino gs)) (deck gs)

casinoStand :: Table -> Table
casinoStand s = s { casino = playerAction Stand (casino s) }

playerTurn :: PlayerAction -> State Table ()
playerTurn Hit = modify playerHit
playerTurn DoubleDown = do modify playerHit
                           modify $ \s -> gamePlayerAction DoubleDown s
playerTurn a = modify $ \s -> gamePlayerAction a s

casinoTurn :: State Table ()
casinoTurn = do s <- get
                let cs = handScore $ hand $ casino s
                modify $ if score cs < 17 || (score cs == 17 && isSoft cs)
                         then casinoHit
                         else casinoStand

result :: Table -> GameResult
result (Table _ player casino)
  | isPlayerFinished player && isPlayerFinished casino =
    case (compare playerScore casinoScore) of
      GT -> Win
      EQ -> Push
      LT -> Lose
  | score playerScore == 21 = if score casinoScore == 21 then Push else Win
  | isBusted playerScore = Lose
  | isBusted casinoScore = Win
  | otherwise = Unfinished
  where
    playerScore = handScore $ hand player
    casinoScore = handScore $ hand casino

-- INPUT/OUTPUT

prompt :: IO String
prompt = putStr "> " >> hFlush stdout >> getLine
  
playerDecision :: IO PlayerAction
playerDecision = do
  putStrLn "[H]it, [s]tand, [d]ouble down, s[p]lit or s[u]rrender?"
  s <- prompt
  case (safeRead (map toLower s)) of
    (Just d) -> return d
    Nothing -> playerDecision
                                    
gameLoop :: Table -> IO GameResult
gameLoop s = do
  putStrLn $ concat $ replicate 25 "-"
  print s
  if result s == Unfinished
  then do turn <- if isPlayerFinished $ player s
                  then return casinoTurn
                  else playerDecision >>= return . playerTurn
          gameLoop $ execState turn s
  else return $ result s

playGame :: Deck -> IO ()
playGame d = gameLoop (newTable d) >>= print

main :: IO ()
main = do g <- getStdGen
          let d = shuffle' frenchDeck (length frenchDeck) g
          playGame d
