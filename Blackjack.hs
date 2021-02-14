import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State
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

handScore :: Hand -> Score
handScore h = if null valid
              then Busted $ minimum busted
              else Valid (maximum valid) (length valid > 1)
  where
    (valid, busted) = span (<=21) . sort . nub . foldr1 (liftM2 (+)) . map cardValue $ h

type Deck = [Card]
type Hand = [Card]

data PlayerAction = None | Hit | Stand | DoubleDown | Split | Surrender deriving (Eq, Show)
data GameResult = Unfinished | Win | Lose | Push deriving (Eq, Show)

isPlayerFinished :: PlayerAction -> Bool
isPlayerFinished Stand = True
isPlayerFinished DoubleDown = True
isPlayerFinished Surrender = True
isPlayerFinished _ = False

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

newGameState :: Deck -> GameState
newGameState d = GameState { deck = drop 4 d
                            , playerHand = take 2 (drop 2 d)
                            , playerAction = None
                            , casinoHand = take 2 d
                            , casinoAction = None }      

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

playerHit :: GameState -> GameState
playerHit (GameState (d:ds) ph _ ch ca) = GameState ds (d:ph) Hit ch ca
playerHit (GameState [] _ _ _ _) = error "No more cards in deck!"

playerStand :: GameState -> GameState
playerStand s = s { playerAction = Stand }

playerDoubleDown :: GameState -> GameState
playerDoubleDown s = s { playerAction = DoubleDown }

playerSurrender :: GameState -> GameState
playerSurrender s = s { playerAction = Surrender }

casinoHit :: GameState -> GameState
casinoHit (GameState (d:ds) ph pa ch _) = GameState ds ph pa (d:ch) Hit
casinoHit (GameState [] _ _ _ _) = error "No more cards in deck!"

casinoStand :: GameState -> GameState
casinoStand s = s { casinoAction = Stand }

playerTurn :: PlayerAction -> State GameState ()
playerTurn Hit = modify playerHit
playerTurn Stand = modify playerStand
playerTurn DoubleDown = do modify playerHit
                           modify playerDoubleDown
playerTurn Surrender = modify playerSurrender             
playerTurn _ = error "Not implemented!"

casinoTurn :: State GameState ()
casinoTurn = do s <- get
                let cs = handScore $ casinoHand s
                modify $ if score cs < 17 || (score cs == 17 && isSoft cs)
                         then casinoHit
                         else casinoStand

result :: GameState -> GameResult
result (GameState _ player pa casino ca)
  | isPlayerFinished pa && isPlayerFinished ca =
    case (compare playerScore casinoScore) of
      GT -> Win
      EQ -> Push
      LT -> Lose
  | score playerScore == 21 = if score casinoScore == 21 then Push else Win
  | isBusted playerScore = Lose
  | isBusted casinoScore = Win
  | otherwise = Unfinished
  where
    playerScore = handScore player
    casinoScore = handScore casino

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
                                    
gameLoop :: GameState -> IO GameResult
gameLoop s = do
  putStrLn $ concat $ replicate 25 "-"
  print s
  if result s == Unfinished
  then do turn <- if isPlayerFinished $ playerAction s
                  then return casinoTurn
                  else playerDecision >>= return . playerTurn
          gameLoop $ execState turn s
  else return $ result s

playGame :: Deck -> IO ()
playGame d = gameLoop (newGameState d) >>= print

main :: IO ()
main = do g <- getStdGen
          let d = shuffle' frenchDeck (length frenchDeck) g
          playGame d
