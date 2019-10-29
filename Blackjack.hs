import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State

data Suit = Club | Diamond | Heart | Spade deriving Show

data Rank = Plain Int | Jack | Queen | King | Ace

instance Show Rank where
  show (Plain v) = show v
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

type Card = (Rank, Suit)

cardValue :: Card -> [Int]
cardValue (Plain v, _) = [v]
cardValue (Ace, _) = [1, 11]
cardValue _ = [10]

handValue :: Hand -> [Int]
handValue = foldr (liftM2 (+)) [0] . map cardValue

isOver :: Int -> Hand -> Bool
isOver val = null . filter (<=val) . handValue

isBelow :: Int -> Hand -> Bool
isBelow val = not . null . filter (<val) . handValue

type Deck = [Card]
type Hand = [Card]
type Finished = Bool

data PlayerAction = Hit | Stand | DoubleDown | Split | Surrender
data GameResult = Unfinished | Win | Lose | Push deriving Show

type GameState = (Deck, Hand, Hand)

frenchDeck :: Deck
frenchDeck = (,) <$> allRanks <*> allSuits
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
hitPlayer (d:ds, player, casino) = (ds, d:player, casino)
hitPlayer ([], _, _) = error "No more cards in deck!"

hitCasino :: GameState -> GameState
hitCasino (d:ds, player, casino) = (ds, player, d:casino)
hitCasino ([], _, _) = error "No more cards in deck!"

playerTurn :: PlayerAction -> State GameState Finished
playerTurn Hit = do modify hitPlayer
                    (_, p, _) <- get
                    return $ isOver 21 p
playerTurn Stand = casinoTurn
playerTurn _ = error ""

casinoTurn :: State GameState Finished
casinoTurn = do (_, _, c) <- get
                if isBelow 17 c
                  then do modify hitCasino
                          casinoTurn
                  else return True

result :: GameState -> GameResult
result (_, player, casino)
  | isOver 21 player = Lose
  | isOver 21 casino = Win
  | score player == score casino = Push
  | score player > score casino = Win
  | otherwise = Lose
  where
    score = maximum . filter (<=21) . handValue

prompt :: IO (String)
prompt = putStr "> " >> hFlush stdout >> getLine
  
playerDecision :: IO PlayerAction
playerDecision = do
  putStrLn "Hit, stand, double down, split or surrender? [h,s,d,p,u]"
  s <- prompt
  case (safeRead s) of
    (Just d) -> return d
    Nothing -> playerDecision

render :: GameState -> IO ()
render (_, player, casino) = do
  putStrLn $ "Casino: " ++ render' casino
  putStrLn $ "Player: " ++ render' player
    where render' cards = ((show . (map fst) . reverse) cards) ++ " (" ++
                          ((show . handValue) cards) ++ ")"
                                    
gameLoop :: GameState -> IO GameResult
gameLoop s = do render s
                d <- playerDecision
                let (finished, s') = runState (playerTurn d) s
                if not finished
                  then gameLoop s'
                  else do render s'
                          return $ result s'

playGame :: Deck -> IO ()
playGame deck = gameLoop ((drop 4 deck), (take 2 deck), (take 2 (drop 2 deck))) >>= putStrLn . show

main :: IO ()
main = do g <- getStdGen
          let deck = shuffle' frenchDeck (length frenchDeck) g
          playGame deck
