import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State

type Card = Int

type Deck = [Card]
type Hand = [Card]
type Finished = Bool

data PlayerAction = Hit | Stand | DoubleDown | Split | Surrender
data GameResult = Unfinished | Win | Lose | Push deriving Show

type GameState = (Deck, Hand, Hand)

prompt :: IO (String)
prompt = putStr "> " >> hFlush stdout >> getLine

frenchDeck :: Deck
frenchDeck = concat . replicate 4 $ [2..10] ++ (replicate 3 10) ++ [11]

fromString :: String -> Maybe PlayerAction
fromString "h" = Just Hit
fromString "s" = Just Stand
fromString "d" = Just DoubleDown
fromString "p" = Just Split
fromString "u" = Just Surrender
fromString _ = Nothing

hitPlayer :: GameState -> GameState
hitPlayer (d:ds, player, casino) = (ds, d:player, casino)
hitPlayer ([], _, _) = error "No more cards in deck!"

hitCasino :: GameState -> GameState
hitCasino (d:ds, player, casino) = (ds, player, d:casino)
hitCasino ([], _, _) = error "No more cards in deck!"

playerTurn :: PlayerAction -> State GameState Finished
playerTurn Hit = do modify hitPlayer
                    (_, p, _) <- get
                    return $ (sum p >= 21)
playerTurn Stand = casinoTurn
playerTurn _ = error ""

casinoTurn :: State GameState Finished
casinoTurn = do (_, _, c) <- get
                if sum c < 17
                  then do modify hitCasino
                          casinoTurn
                  else return True

result :: GameState -> GameResult
result (_, player, casino)
  | playerScore > 21 = Lose
  | casinoScore > 21 = Win
  | playerScore == casinoScore = Push
  | playerScore > casinoScore = Win
  | otherwise = Lose
  where
    casinoScore = sum casino
    playerScore = sum player
  
playerDecision :: IO PlayerAction
playerDecision = do putStrLn "Hit, stand, double down, split or surrender? [h,s,d,p,u]"
                    s <- prompt
                    case (fromString s) of
                      (Just d) -> return d
                      Nothing -> playerDecision

render :: GameState -> IO ()
render (_, player, casino) = do putStrLn $ "Casino: " ++ render' casino
                                putStrLn $ "Player: " ++ render' player
                             where render' cards = ((show . reverse) cards) ++ " (" ++
                                                   ((show . sum) cards) ++ ")"
                                    
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
