{-# Language TypeApplications #-}
-- | Rock paper scissors game implemented with FRP
module Main where

import Dyna
import System.IO
import Text.Read
import Data.Tuple (swap)

data Move = Rock | Paper | Scissors
  deriving (Show, Read, Eq)

-- | Result of the rounds
data Score = Score
  { game'first   :: !Int
  , game'second  :: !Int
  , game'draw    :: !Int
  , game'moves   :: [(Move, Move)]
  }

instance Semigroup Score where
  (<>) (Score a1 b1 c1 d1) (Score a2 b2 c2 d2) = Score (a1 + a2) (b1 + b2) (c1 + c2) (d1 ++ d2)

instance Monoid Score where
  mempty = Score 0 0 0 []

-- | Collect scores
toScore :: Move -> Move -> Score
toScore a b = case (a, b) of
  (Rock, Paper)     -> secondWin
  (Rock, Scissors)  -> firstWin
  (Paper, Scissors) -> secondWin
  _ | a == b        -> draw
  otherwise         -> swapWin $ toScore b a
  where
    firstWin  = Score 1 0 0 [(a, b)]
    secondWin = Score 0 1 0 [(a, b)]
    draw      = Score 0 0 1 [(a, b)]
    swapWin (Score a b draw moves) = Score b a draw (fmap swap moves)

-- | Print current state
printScore :: Int -> (Int, Score) -> String
printScore totalRounds (roundId, Score firstWin secondWin draw moves) = unlines
  [ "Score: round " <> show roundId
  , "  first  : " <> show firstWin
  , "  second : " <> show secondWin
  , "  draw   : " <> show draw
  , if totalRounds == roundId
      then unlines ["  last move: " <> show (last moves), status]
      else "  last move: " <> show (last moves)
  ]
  where
    status
      | firstWin > secondWin = "You win!"
      | firstWin < secondWin = "You lose!"
      | otherwise            = "It's a Draw"

-- | How many rounds to play
total :: Int
total = 5

game :: Evt IO String
game =
  once getLine |> foreverE |>           -- read user input
  mapMay (readMaybe @Move) |>           -- take only valid moves
  takeE total |>                        -- take only so many moves
  withOneOf [Rock, Paper, Scissors] |>  -- add random AI move as first element of the tuple
  foldMapE (uncurry $ flip toScore) |>  -- get the score and accumulate scores
  withCount |>                          -- append the round number
  fmap (printScore total)               -- print the current score

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  greet
  putStrLnE game

greet :: IO ()
greet = putStrLn $ unlines
  [ "Welcome to Rock-paper-scissors game!"
  , unwords ["Let's play for", show total, "rounds"]
  , "Type: Rock, Paper or Scissors to make a move."
  ]

