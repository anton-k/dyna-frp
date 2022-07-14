---
title: Interactive Game for Rock-Paper-Scissors
---

For now we know what is event stream and how to use it. 
Let's define a simple game with them. FRP is often associated with 
UI applications or Graphical games or even Creation of dynamic web pages.

But I'd like to provide a solid understanding of the basic concepts for you with simple example
that can be run right in the REPL.
For that we can use Rock-Paper-Scissors game and build an application for it
as a Haskell function.

We all used to play Rock-Paper-Scissors (RPS) when we were kids to resolve complicated
questions. It was universal judge and authority. There is a question and we don't
know who is right. Let's just solve it with RPS game. 

If you somehow don't know about it. Here is a description. It's a hand game.
Two or more players say a some proverb and as they say it they hold hands
and on the last word the show one of three figures with the hands: Rock (closed fist),
paper (a flat hand), scissors (V-shape with index and middle fingers). 
Rock wins over scissors, paper wins over rock, scissors win over paper and
if two players show the same figure it's a draw.

Let's implement that game. We are going to play with AI which randomly chooses
one of the figures. And we input the figure with `getLine` event stream.

Let's look at the types first:

```haskell
data Move = Rock | Paper | Scissors
  deriving (Show, Read, Eq)
```

So the move is one of the 3 hand figures. Let's describe the result of the game:

```haskell
-- | Result of the rounds
data Score = Score
  { game'first   :: !Int             -- ^ first player wins so many times
  , game'second  :: !Int             -- ^ second player wins
  , game'draw    :: !Int             -- ^ it's a draw
  , game'moves   :: [(Move, Move)]   -- ^ list of moves
  }
```

The list of imports that we going to use:

```haskell
import Dyna
import System.IO
import Text.Read
import Data.Tuple (swap)
```

We are going to play several rounds and we will have only two players. 
As we go along we will count the number of each player's victories
and also we will keep the record of the moves.

Let's calculate the results for a single round:

```haskell
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
```

To make it easy to keep and accumulate the scores we define the obvious monoid instance for it:

```haskell
instance Semigroup Score where
  (<>) (Score a1 b1 c1 d1) (Score a2 b2 c2 d2) = Score (a1 + a2) (b1 + b2) (c1 + c2) (d1 ++ d2)

instance Monoid Score where
  mempty = Score 0 0 0 []
```

Also we need some nice print of the results:

```haskell
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
```

We keep showing the state for all rounds and for the last round
we calculate the final verdict who won the whole game.

With this in place we are ready to define a game in FRP-style.
Let's describe first the algorithm of the game. 

* read user input
* take only valid moves
* take only so many moves as the number of rounds
* add random AI move as first element of the tuple
* get the score and accumulate scores
* append the round number
* print the current score for each round

Each of the step can be expressed as an event stream processing function.
For convenience we will use the pipe operator (hello Elixir).
It's just reversed Haskell's application operator:

```haskell
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
```

Let's turn our description into application line by line:

* read user input

  ```haskell
  once getLine |> forevers |> 
  ```
  We repeatedly query user for input.

* take only valid moves. We use the same trick as we used with `readInts`. The function
  `readMaybe` comes from the standard module `Text.Read`

  ```haskell
  mapMay (readMaybe @Move) |> 
  ```

* take only so many moves as the number of rounds

  ```haskell
  takes total |>
  ```

  Note the clever trick to skip all invalid user inputs prior to `takes` function. Underlying stream will
  query for the next move forever but for the game we will count only valid moves.

* add random AI move as a first element of the tuple

  ```haskell
  withOneOf [Rock, Paper, Scissors] |>  
  ```

  We append to the user's move some random move.
  Note that random move goes first in the result tuple pair.

* get the score and accumulate scores

  ```haskell
  foldMaps (uncurry $ flip toScore) |>  -- get the score and accumulate scores
  ```

  The `foldMaps` function is a combo of `fmap` and `appends`. It transforms
  the event values to something monoidable and appends them all. 
  For our example we calculate the score for a single round and
  accumulate the score across all the rounds.
  We use flip to reverse user's and AI's moves.
  As we want to show user as a first player and AI as second one.

* append the round number

  ```haskell
  withCount |>    
  ```

  To print the number of the current round we count all the events.

* Show the results:

  ```haskell
  fmap (printScore total)               -- print the current score
  ```

  The final step.

Here is the complete code for the FRP-part of the application:

```haskell
game :: Evt IO String
game =
  once getLine |> forevers |>           -- read user input
  mapMay (readMaybe @Move) |>           -- take only valid moves
  takes total |>                        -- take only so many moves
  withOneOf [Rock, Paper, Scissors] |>  -- add random AI move as first element of the tuple
  foldMaps (uncurry $ flip toScore) |>  -- get the score and accumulate scores
  withCount |>                          -- append the round number
  fmap (printScore total)               -- print the current score
```

The pipe operator is defined in the `dyna` lib.

The final parts to give our game a shape:

```haskell
import Dyna
import System.IO
import Text.Read
import Data.Tuple (swap)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  greet
  putStrLns game

greet :: IO ()
greet = putStrLn $ unlines
  [ "Welcome to Rock-paper-scissors game!"
  , unwords ["Let's play for", show total, "rounds"]
  , "Type: Rock, Paper or Scissors to make a move."
  ]
```

We can find out the complete code in the directory `dyna/examples/`.
Wow the FRP-part of the game was just 7 lines of code.
And it looks very declarative although we have used imperative 
functions under the hood. We combined callback processor functions in neat way
to achieve that.

------------------------------------------------------------------------------

* `<=` [Event streams](/dyna-core/tutorial/02-event-streams)
* `=>` [Dynamics - continuous processes](/dyna-core/tutorial/04-dynamics)
* Up: [Table of contents](/dyna-core/tutorial-toc)
