import Cards
import Control.Monad.State.Lazy
import Data.Char
import Data.Either
import Data.List
import System.IO
import System.Random
import System.Random.Shuffle

type Deck = [Card]

getCards :: Int -> State Deck [Card]
getCards x = do
  s <- get
  put (drop x s)
  return (take x s)

getCard :: State Deck Card
getCard = head <$> getCards 1

data PlayerAction = None | Hit | Stand | DoubleDown | Split | Surrender deriving (Eq, Show)

isPlayerFinished :: Player -> Bool
isPlayerFinished p
  | isLeft bs = True
  | bs == Right 21 = True
  | la == Stand = True
  | la == DoubleDown = True
  | la == Surrender = True
  | otherwise = False
  where
    bs = score $ hand p
    la = lastAction p

data Player = Player
  { hand :: [Card],
    lastAction :: PlayerAction
  }

instance Show Player where
  show (Player hand lastAction) =
    showCards hand ++ "\t(" ++ showScore hand ++ ")\t" ++ show lastAction
    where
      showCards = unwords . map show . reverse
      showScore = show . score

hit :: Player -> State Deck Player
hit p = do
  c <- getCard
  return $
    p
      { hand = c : hand p,
        lastAction = Hit
      }

data Table = Table
  { deck :: Deck,
    player :: Player,
    casino :: Player
  }

instance Show Table where
  show s =
    "Casino: " ++ show (casino s) ++ "\n"
      ++ "Player: "
      ++ show (player s)

newTable :: Deck -> Table
newTable d =
  Table
    { deck = newDeck,
      player = Player h1 None,
      casino = Player h2 None
    }
  where
    deal = do
      h1 <- getCards 2
      h2 <- getCards 2
      return (h1, h2)
    ((h1, h2), newDeck) = runState deal d

-- STATE TRANSITIONS

playerAction :: PlayerAction -> Player -> State Deck Player
playerAction Hit p = hit p
playerAction DoubleDown p = hit p >>= playerAction DoubleDown
playerAction a p = return $ p {lastAction = a}

-- INPUT/OUTPUT

safeRead :: String -> Maybe PlayerAction
safeRead "h" = Just Hit
safeRead "s" = Just Stand
safeRead "d" = Just DoubleDown
safeRead "p" = Just Split
safeRead "u" = Just Surrender
safeRead _ = Nothing

prompt :: IO String
prompt = putStr "> " >> hFlush stdout >> getLine

playerDecision :: Player -> IO PlayerAction
playerDecision p = do
  putStrLn "[H]it, [s]tand, [d]ouble down, s[p]lit or s[u]rrender?"
  s <- prompt
  case safeRead (map toLower s) of
    (Just d) -> return d
    Nothing -> playerDecision p

casinoDecision :: Player -> IO PlayerAction
casinoDecision p =
  let cs = scores $ hand p
      bs = fromRight 22 $ score $ hand p
      action =
        if bs < 17 || bs == 17 && minimum cs < 17
          then Hit
          else Stand
   in return action

playerLoop :: Player -> (Player -> IO PlayerAction) -> Deck -> IO (Player, Deck)
playerLoop p logic deck =
  if isPlayerFinished p
    then return (p, deck)
    else do
      action <- logic p
      let (p', d') = runState (playerAction action p) deck
      print p'
      playerLoop p' logic d'

gameLoop :: Table -> IO Ordering
gameLoop table = do
  putStrLn $ concat $ replicate 25 "-"
  print table
  (p', d') <- playerLoop (player table) playerDecision (deck table)
  -- TODO: no need for casino loop if player busted
  (c', _) <- playerLoop (casino table) casinoDecision d'
  return $ compareScore (score $ hand p') (score $ hand c')

playGame :: Deck -> IO ()
playGame d = gameLoop (newTable d) >>= print

main :: IO ()
main = do
  g <- getStdGen
  let d = shuffle' frenchDeck (length frenchDeck) g
  playGame d
