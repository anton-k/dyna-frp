---
title: FRP processing example
---

Let's study another example. 
In this application we are going to paint green and red
circles on the screen. We paint in the mouse position when
we press Right Button and we can change the color when we 
press the Left Button.

The complete code can be found in the directory: `dyna-processing/examples/Ball.hs`.


## Init application

Let's start with an empty screen and setup all the defaults:

```haskell
module Main where

import Dyna.Proc

-- | Screen specification
spec :: Spec
spec = Spec $ size (P2 700 700)

-- | Main app
main = runApp spec $ pure (initDraw <> pic)  
  where
    initDraw = background white
    pic      = mempty
```


This should draw an empty screen.

## Draw a ball at the mouse position

This should be familiar from the tutorial.
Let's draw a filed circle at the mouse position.
With it we indicate what we are going to paint:


```haskell
-- | Ball: color, position
data Ball = Ball Col P2

-- | Draw a Ball
draw :: Ball -> Draw
draw (Ball col pos) = do
  strokeFill col
  circle 25 pos

-- | Ball at the mouse position
pointer :: Dyn Ball
pointer = liftA2 Ball ballCol mouse

-- | Cols alterate on mouse left clicks
ballCol :: Dyn Col
ballCol = hold green $ cycles [red, green] mouseLeft

pic = pointer
```

Here we also define the change of the color by mouse left-button clicks:

```haskell
ballCol = hold green $ cycles [red, green] mouseLeft
```

The `cycles` alterates between two values of the color on every
event of left button click. Also we use `hold` to turn event stream
to continuous dynamic process.

Note how easy it was to construct the ball with applicative instance:

```haskell
pointer :: Dyn Ball
pointer = liftA2 Ball ballCol mouse
```

We map with `Ball` constructor over dynamic color and dynamic position.
Ok we can see a paint pointer that can change the color.
How can we draw balls on the screen?

## Drawing the balls

To do that on every Mouse Right Button press we will save
the current pointer to the list of balls. And on every
frame of animation we will draw not only pointer but also all
the balls which were saved to the list.

Note that order of painting of the balls matters. 
For better user experience instead of list we use `Sequence`
which allows fast appending to the tail:

```haskell
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Balls are saved to the sequence and redrawn on each frame also we draw the pointer.
-- Note that order of storage affects the drawing. Later pictures go in the foreground.
balls :: Dyn (Seq Ball)
balls =
  liftA2 (Seq.|>)
    (scanD (flip (Seq.|>)) Seq.empty $ snap pointer mouseRight)
    pointer
```

In the function `balls` we save all balls including the pointer
to the sequence. We accumulate the balls with function `scanD`.
We `snap` every mouse right click with current value of the pointer
then accumulate all the balls with `scanD`.

Let's draw a complete picture:

```haskell
-- | Main app
main = runApp spec $ pure $
  (mappend initDraw . foldMap draw <$> balls)  -- <> countBalls
  where
    initDraw = background white
```
This completes the example.

------------------------------------------------------------------------------

* `<=` [Introduction](/dyna-processing/tutorial/00-intro)
* `=>` [Quick reference](/dyna-processing/tutorial/101-reference)
* Up: [Table of contents](/dyna-processing/tutorial-toc)

