---
title: Puzzle 15
---

In this example we will implement "Puzzle 15" game in `dyna-brick`.
Here is what end point position look like:

![Puzzle 15](/images/puzzle-15.png)


The player tries to arrange numbers on the board.
By pressing arrow keys we can substitute some number with
an empty box. We win when all numbers are placed in ascending order
like it is shown on the picture.

You can find the complete example in the examples of the `dyna-brick` directory.

## Types for the game

```haskell
import Data.Vector (Vector)

newtype Board = Board (Vector Int)
  deriving (Eq)

data Move = UpMove | DownMove | LeftMove | RightMove

data Game = Game
  { game'board :: !Board
  , game'empty :: !Int
  , game'count :: !Int
  }
```

We represent our board as one dimensional vector of numbers.
An empty box holds the `0`-number. For the game we will store
the board the place of empty box and number of steps so far.
We store the empty field once again because all moves
happen with this field and if we store it's identifier we
can find it faster on the board. No need to filter the vector of fields. 

We have four possible moves: up, down, right and left.

## Logic of the game

The logic is simple we can update the board with the move and we should be able to check
weather we have reached the final state. This code is not going to be ralted to FRP.
It is just a logic of the game update per single step.

### The final state of the game

Let's check for the final state first:

```haskell
import qualified Data.Vector as V

finBoard :: Board
finBoard = Board $ V.fromList $ [1..15] ++ [0]

isGameOver :: Game -> Bool
isGameOver (Game board _ _) = board == finBoard
```

So we have defined the ordered state of the board `initGame`

### Update on the move

On the move update we are going to swap some adjacent nummeric field
with empty field. By the move we give direction of swap. 
In this function we convert from 1D coordinate representation to 2D one.
We make a swap and check weather direction is within the field. 
For example if empty field is on the upper corner we can not move
it further up. We should watch out for such moves and prohibit them.
Here is the code that does the update:

```haskell
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
```

### Get random shuffle for the puzzle

Also for the game we need to be able to present user with random permutation.
It turns out that it can not be completely random. As some permutations can
not be ordered to the final state. To solve this problem we generate a sequence
of moves and apply them to the final state to make a valid suffle that
can be solved:

```haskell
import System.Random

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
```

Also we check that each move updates the position. Because some 
moves can be invalid as we have discussed that in the previous section.
For example if we try to move empty field away from the board's boundaries.

Ok we are done with the logic let us move to to the brick part of it.
First we need to be able to draw the game state on the screen

## Draw game state

Let's first draw a count of moves and state how many moves
already was done. For a while we leave the drawing of the board unimplemented:

```haskell
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
drawBoard _ = emptyWidget
```

To draw a board we need to draw a four lines of numbers
each contains four cells with digits from 1 to 15 or
one empty field. If the number is zero we treat it as an empty field.

```haskell
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
```

## FRP part

Ok now we have all parts in place and we are ready to define a game loop
for the game. To play the game we are going to receive arrow presses
for the moves and press on Esc button to quit the game.
As we receive a move we will updat the current state of the game.

We start with initialisation of the random puzzle with given complexity
which is encoded to numers of steps per initial shuffle:

```haskell
main :: IO ()
main = do
  newGame <- shuffle complexity (Game finBoard 15 0)
  runApp (defSpec emptyAttrMap) $ pure $
    Win (pure . drawGame <$> games newGame) quit
  where
    complexity = 100
    quit = Quit <$ onKey KEsc

games :: Game -> Evt Game
games = undefined
```

Recall that brick application for `dyna-brick` is a `Win` type
which is a pair of `Dyn Box` and `Evt Act`. The `Box` is a widget
and `Act` signals when to quit the application. In this example
we show that we quit when user presses `Esc`:

```haskell
    quit = Quit <$ onKey KEsc
```

The `onKey` is a standard function from the `dyna-brick` library
that creates event stream that responsible for a single Key press.
The missing part is to read moves and update the game state.
Let's read the moves first:

```haskell
moves :: Evt Move
moves = mconcat
  [ UpMove    <$ onKey KUp
  , DownMove  <$ onKey KDown
  , LeftMove  <$ onKey KLeft
  , RightMove <$ onKey KRight
  ]
```

Here we are using the `Monoid` instance to join 
moves of four different types to a single stream of events.
Recall that `(a <$ )` operator is a convenient shortcut for `fmap (const a)`.
With this event stream of moves we can update the initial state of the game:

```haskell
games :: Game -> Dyn Game
games g = hold g $ scan step g moves
  where
    step m g
      | isGameOver g = g
      | otherwise    = move m g
```

Let's remind the core FRP functions that are used in this definition:

```haskell
hold  :: a -> Evt a -> Dyn a
scan :: (a -> s -> s) -> s -> Evt a -> Evt s
```

So in this definition we are going to respond to moves until the 
game reaches the final state. After that we can only quit application.
And we do not respond to arrows anymore. 

Also we could use the function:

```haskell
scanD :: (a -> s -> s) -> s -> Evt a -> Dyn s
```

It uses the combo of `hold` and `scan` functions. That's it!
we have defined the game. The FRP part is very succint but it does what we need:

```haskell
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
```


------------------------------------------------------------------------------

* `<=` [Introduction](/dyna-brick/tutorial/00-intro)
* `=>` [Quick reference](/dyna-brick/tutorial/101-reference)
* Up: [Table of contents](/dyna-brick/tutorial-toc)

