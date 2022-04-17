-- | 15 Puzzle game implemented in dyna-brick
module Main where

import Dyna.Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random

-------------------------------------------------------------------------------------
-- Types

newtype Board = Board (Vector Int)
  deriving (Eq)

data Move = UpMove | DownMove | LeftMove | RightMove
    deriving (Enum)

data Game = Game
  { game'board :: !Board
  , game'empty :: !Int
  , game'count :: !Int
  }

-------------------------------------------------------------------------------------
-- Logic

isGameOver :: Game -> Bool
isGameOver (Game board _ _) = board == finBoard

finBoard :: Board
finBoard = Board $ V.fromList $ [1..15] ++ [0]

move :: Move -> Game -> Game
move m game@(Game (Board board) emptyPos count)
  | within2d nextPos2d = Game (Board board') nextPos (succ count)
  | otherwise          = game
  where
    orient = case m of
      UpMove    -> (1, 0)
      DownMove  -> (-1, 0)
      LeftMove  -> (0, 1)
      RightMove -> (0, -1)

    nextPos = from2d nextPos2d
    nextPos2d = orient + to2d emptyPos
    within2d (x, y) = within x && within y
    within a = 0 <= a && a < 4
    board' = board V.// [(emptyPos, board V.! nextPos), (nextPos, 0)]

to2d :: Int -> (Int, Int)
to2d n = n `divMod` 4

from2d :: (Int, Int) -> Int
from2d (x, y) = x * 4 + y

drawGame :: Game -> Box
drawGame game@(Game board _ count) = vBox
  [ padBottom (Pad 2) $ drawBoard board
  , if isGameOver game
      then vBox [ str $ unwords ["Hurray! You win in", show count, "steps!"], endGameHint]
      else vBox [ str $ unwords ["Steps:", show count], inGameHint]
  ]
  where
    inGameHint  = str "Press arrows to move and Esc to exit"
    endGameHint = str "Press Esc to exit"

drawBoard :: Board -> Box
drawBoard (Board vs) = vBox $ fmap drawLine [0..3]
  where
    drawLine n = hBox $ fmap (cell n) [0..3]

    cell x y = withBorderStyle unicodeBold $ border $
      padTopBottom 1 $ padLeftRight 2 $ str $ toStr $ vs V.! (from2d (x, y))

    toStr n
      | n == 0    = "  "
      | otherwise = let res = show n
                    in  if length res == 1
                          then ' ' : res
                          else res

-------------------------------------------------------------------------------------
-- random game

shuffle :: Int -> Game -> IO Game
shuffle n g
  | n == 0    = pure $ g { game'count = 0 }
  | otherwise = do
      m <- randomMove
      let g' = (move m g)
      if game'board g == game'board g'
        then shuffle n g
        else shuffle (n - 1) g'
  where
    randomMove = (moves V.! ) . (`mod` 4) <$> randomIO
    moves = V.fromList [UpMove, DownMove, LeftMove, RightMove]

-------------------------------------------------------------------------------------
-- UI

main :: IO ()
main = do
  newGame <- shuffle complexity (Game finBoard 15 0)
  runApp (defSpec emptyAttrMap) $ pure $
    Win (pure . drawGame <$> games newGame) quit
  where
    complexity = 100
    quit = Quit <$ onKey KEsc

games :: Game -> Dyn Game
games g = hold g $ scan step g moves
  where
    step m g
      | isGameOver g = g
      | otherwise    = move m g

moves :: Evt Move
moves = mconcat
  [ UpMove    <$ onKey KUp
  , DownMove  <$ onKey KDown
  , LeftMove  <$ onKey KLeft
  , RightMove <$ onKey KRight
  ]

