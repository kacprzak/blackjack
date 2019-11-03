import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import Data.List

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

handScore :: Hand -> Score
handScore h = if null valid
              then Busted $ minimum busted
              else Valid (maximum valid) (length valid > 1)
  where
    (valid, busted) = span (<=21) . sort . nub . foldr1 (liftM2 (+)) . map cardValue $ h

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
  casinoAction :: PlayerAction }

instance Show GameState where
  show s =
    (showPlayer "Casino" (casinoHand s) (casinoAction s)) ++ "\n" ++
    (showPlayer "Player" (playerHand s) (playerAction s))
    where
      showPlayer name cards lastAction =
        name ++ ": " ++ showCards cards ++ "\t(" ++ showValue cards ++ ") " ++ show lastAction
      showCards = intercalate " " . map show . reverse
      showValue = show . handScore

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
isFinished (GameState _ ph _ ch _) = let playerSore = handScore ph
                                         casinoScore = handScore ch
                                     in isBusted playerSore || isBusted casinoScore

playerTurn :: PlayerAction -> State GameState Finished
playerTurn Hit = do modify hitPlayer
                    isFinished <$> get
playerTurn Stand = do modify standPlayer
                      isFinished <$> get
playerTurn _ = error "Not implemented!"

casinoTurn :: State GameState Finished
casinoTurn = do s <- get
                let cScore = handScore $ casinoHand s
                if cScore < Valid 17 False || cScore <= Valid 17 True
                  then do modify hitCasino
                          isFinished <$> get
                  else do modify standCasino
                          isFinished <$> get

result :: GameState -> GameResult
result (GameState _ player _ casino _)
  | isBusted playerScore = Lose
  | isBusted casinoScore = Win
  | playerScore == casinoScore = Push
  | playerScore > casinoScore = Win
  | otherwise = Lose
  where
    playerScore = handScore player
    casinoScore = handScore casino

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
          then playerDecision >>= return . playerTurn
          else return casinoTurn
  let (finished, s') = runState turn s
  putStrLn $ concat $ replicate 25 "-"
  print s'
  if not finished
    then gameLoop s'
    else return $ result s'

initGameState :: Deck -> GameState
initGameState d = GameState { deck = drop 4 d
                            , playerHand = take 2 (drop 2 d)
                            , playerAction = None
                            , casinoHand = take 2 d
                            , casinoAction = None }

playGame :: Deck -> IO ()
playGame d = let s = initGameState d
             in do print s
                   gameLoop s >>= print

main :: IO ()
main = do g <- getStdGen
          let d = shuffle' frenchDeck (length frenchDeck) g
          playGame d
