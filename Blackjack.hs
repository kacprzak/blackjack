import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import Data.List

sortUniq :: (Ord a) => [a] -> [a]
sortUniq = map head . group . sort

data Suit = Club | Diamond | Heart | Spade

instance Show Suit where
  show Club = [toEnum 9827]
  show Diamond = [toEnum 9830]
  show Heart = [toEnum 9829]
  show Spade = [toEnum 9824]

data Rank = Plain Int | Jack | Queen | King | Ace

instance Show Rank where
  show (Plain v) = show v
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card = Card Rank Suit

instance Show Card where
  show (Card r _) = show r -- ++ show s

cardValue :: Card -> [Int]
cardValue (Card (Plain v) _) = [v]
cardValue (Card Ace _) = [1, 11]
cardValue _ = [10]

handValues :: Hand -> [Int]
handValues = sortUniq . foldr (liftM2 (+)) [0] . map cardValue

handValue :: Hand -> Int
handValue h = if isOver 21 h
              then minimum vs
              else maximum . filter (<=21) $ vs
  where
    vs = handValues h

isOver :: Int -> Hand -> Bool
isOver val = all (>val) . handValues

isBelow :: Int -> Hand -> Bool
isBelow val = any (<val) . handValues

type Deck = [Card]
type Hand = [Card]
type Finished = Bool

data PlayerAction = None | Hit | Stand | DoubleDown | Split | Surrender deriving (Eq, Show)
data GameResult = Unfinished | Win | Lose | Push deriving Show

data GameState = GameState {
  deck :: Deck,
  playerHand :: Hand,
  playerAction :: PlayerAction,
  casinoHand :: Hand,
  casinoAction :: PlayerAction
  }

instance Show GameState where
  show s =
    (showPlayer "Casino" (casinoHand s) (casinoAction s)) ++ "\n" ++
    (showPlayer "Player" (playerHand s) (playerAction s))
    where showPlayer name cards lastAction =
            name ++ ": " ++ showCards cards ++ "\t" ++ showValue cards ++ " " ++ show lastAction
          showCards cards = (concat . intersperse " " . map show . reverse) cards
          showValue cards = "(" ++
                            (concat . intersperse "/" . map show . handValues) cards ++
                            ")"

frenchDeck :: Deck
frenchDeck = Card <$> allRanks <*> allSuits
  where
    allRanks = (map Plain [2..10]) ++ [Jack, Queen, King, Ace]
    allSuits = [Club, Diamond, Heart, Spade]

safeRead :: String -> Maybe PlayerAction
safeRead "h" = Just Hit
safeRead "s" = Just Stand
safeRead "d" = Just DoubleDown
safeRead "p" = Just Split
safeRead "u" = Just Surrender
safeRead _ = Nothing

hitPlayer :: GameState -> GameState
hitPlayer (GameState (d:ds) ph _ ch ca) = GameState ds (d:ph) Hit ch ca
hitPlayer (GameState [] _ _ _ _) = error "No more cards in deck!"

standPlayer :: GameState -> GameState
standPlayer s = s { playerAction = Stand }

hitCasino :: GameState -> GameState
hitCasino (GameState (d:ds) ph pa ch _) = GameState ds ph pa (d:ch) Hit
hitCasino (GameState [] _ _ _ _) = error "No more cards in deck!"

standCasino :: GameState -> GameState
standCasino s = s { casinoAction = Stand }

isFinished :: GameState -> Bool
isFinished (GameState _ _ Stand _ Stand) = True
isFinished (GameState _ ph _ ch _) = isOver 21 ph || isOver 21 ch

playerTurn :: PlayerAction -> State GameState Finished
playerTurn Hit = do modify hitPlayer
                    isFinished <$> get
playerTurn Stand = do modify standPlayer
                      isFinished <$> get
playerTurn _ = error ""

casinoTurn :: State GameState Finished
casinoTurn = do (GameState _ _ _ ch _) <- get
                if isBelow 17 ch
                  then do modify hitCasino
                          isFinished <$> get
                  else do modify standCasino
                          isFinished <$> get

result :: GameState -> GameResult
result (GameState _ player _ casino _)
  | playerResult > 21 = Lose
  | casinoResult > 21 = Win
  | playerResult == casinoResult = Push
  | playerResult > casinoResult = Win
  | otherwise = Lose
  where
    playerResult = handValue player
    casinoResult = handValue casino

prompt :: IO (String)
prompt = putStr "> " >> hFlush stdout >> getLine
  
playerDecision :: IO PlayerAction
playerDecision = do
  putStrLn "Hit, stand, double down, split or surrender? [h,s,d,p,u]"
  s <- prompt
  case (safeRead s) of
    (Just d) -> return d
    Nothing -> playerDecision
                                    
gameLoop :: GameState -> IO GameResult
gameLoop s = do
  turn <- if playerAction s /= Stand
          then do d <- playerDecision
                  return $ playerTurn d
          else return $ casinoTurn
  let (finished, s') = runState turn s
  putStrLn $ concat $ replicate 25 "-"
  putStrLn $ show s'
  if not finished
    then gameLoop s'
    else return $ result s'

initGameState :: Deck -> GameState
initGameState d = GameState (drop 4 d) (take 2 d) None (take 2 (drop 2 d)) None

playGame :: Deck -> IO ()
playGame d = let s = initGameState d
             in do putStrLn $ show s
                   gameLoop s >>= putStrLn . show

main :: IO ()
main = do g <- getStdGen
          let d = shuffle' frenchDeck (length frenchDeck) g
          playGame d
