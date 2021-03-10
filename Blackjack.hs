import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State.Lazy
import Data.List
import Data.Char

import Cards

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

instance Show Player where
  show (Player hand lastAction) =
    showCards hand ++ "\t(" ++ showScore hand ++ ")\t" ++ show lastAction
    where
      showCards = intercalate " " . map show . reverse
      showScore = show . bestScore

hit :: Player -> State Deck Player
hit p = do
  c <- getCard
  return $ p { hand = (c:(hand p))
             , lastAction = Hit }

data Table = Table {
  deck :: Deck,
  player :: Player,
  casino :: Player
}

instance Show Table where
  show s =
    "Casino: " ++ (show $ casino s) ++ "\n" ++
    "Player: " ++ (show $ player s)

newTable :: Deck -> Table
newTable d = Table { deck = newDeck
                            , player = Player h1 None
                            , casino = Player h2 None }
  where deal = do
               h1 <- getCards 2
               h2 <- getCards 2
               return (h1, h2)
        ((h1, h2), newDeck) = runState deal d

safeRead :: String -> Maybe PlayerAction
safeRead "h" = Just Hit
safeRead "s" = Just Stand
safeRead "d" = Just DoubleDown
safeRead "p" = Just Split
safeRead "u" = Just Surrender
safeRead _ = Nothing

-- STATE TRANSITIONS

playerAction :: PlayerAction -> Player -> State Deck Player
playerAction Hit p = hit p
playerAction DoubleDown p = hit p >>= playerAction DoubleDown
playerAction a p = return $ p { lastAction = a }

playerTurn :: PlayerAction -> State Table ()
playerTurn a = do s <- get
                  let (p', d') = runState (playerAction a (player s)) (deck s)
                  put s { deck = d', player = p' }

casinoTurn :: State Table ()
casinoTurn = do s <- get
                let cs = score $ hand $ casino s
                    bs = bestScore $ hand $ casino s
                    action = if (bs < 17) || (bs == 17 && minimum cs < 17) then
                      Hit else Stand
                    (c', d') = runState (playerAction action (casino s)) (deck s)
                put s { deck = d', casino = c' }

result :: Table -> GameResult
result (Table _ player casino)
  | isPlayerFinished player && isPlayerFinished casino =
    case (compare playerScore casinoScore) of
      GT -> Win
      EQ -> Push
      LT -> Lose
  | playerScore == 21 = if casinoScore == 21 then Push else Win
  | playerScore > 21 = Lose
  | casinoScore > 21 = Win
  | otherwise = Unfinished
  where
    playerScore = bestScore $ hand player
    casinoScore = bestScore $ hand casino

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
