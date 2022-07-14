---
title: FRP for processing gaming/animation library
---

In this tutorial we will study the FRP binding to the 
[processing-for-haskell](https://hackage.haskell.org/package/processing-for-haskell) library.
The `processing-for-haskell` is suitable to create animations and games.
It embeds Processing language into Haskell.
The application is run with state-machine like approach.
We have a function that renders the state of application
on the screen and we have a function that updates the state
on events.

With FRP we use the same functions for drawing as they are
defined in the gloss 
(see the module [Graphics.Proc](https://hackage.haskell.org/package/processing-for-haskell-0.1.0.1/docs/Graphics-Proc.html#g:9)).

Only to run the application we use FRP approach. The main function is:

```haskell
type Draw = Pio ()

runApp :: Spec -> Run (Dyn Draw) -> IO ()
```

The `Draw` is a set of drawing instructions to the renderer. 
Where `Spec` defines the initial settings of the gloss application:

```haskell
data Spec = Spec
  { spec'setup  :: Pio ()  -- ^ setup instructions }
```

The `spec'setup` is a procedure that is run once at startup.
So to run the application we need to provide the spec and dynamic picture:

```haskell
runApp :: Spec -> Run (Dyn Draw) -> IO ()
```

The `Draw` is a set of instructions to the OpenGL renderer.
It's a bit more higher-level but still imperative. 
It run inside `Pio`-monad which stands for Processing `IO`.
The `Run` is a monad that has instance of `Frp` class, i.e. we can
use `MonadIO` and `MonadBaseControl` methods with it. 
For writing internal mutable variables we use `IORef`.

The library `dyna-processing` re-exports all drawing primitives 
from the `processing-for-haskell` library.
It re-exports the core FRP functions of `dyna`. 
Also it defines special wrappers for `Evt` and `Dyn` to avoid
typing `Evt Run a` since the `Run` monad is fixed for gloss applications.
It's very simple `newtype`-wrappers:

```haskell
newtype Evt a = Evt { unEvt :: Dyna.Evt Run a }
newtype Dyn a = Evt { unDyn :: Dyna.Dyn Run a }
```

So we can use all the functions from the `Dyna` module.

### The first program

Let's define a simple program. It will draw a circle at 
the position of the mouse:

```haskell
module Main where

import Dyna.Proc

main :: IO ()
main = runApp spec $ pure pic
  where
    spec = Spec $ do
      size (P2 500 500)

drawCircle :: P2 -> Draw
drawCircle pos = do
  background white
  translate pos
  fill green
  stroke green
  circle 30 0

pic :: Dyn Draw
pic = drawCircle <$> mouse
```

Let's save that to the file `HelloProcessing.hs`.
To run the programs we can compile it and run executable
or execute right away with `runhaskell`:

```haskell
stack exec -- runhaskell HelloGloss.hs
```

We can close the window to stop the application.
So let's study the code. What happens:

```haskell
pic = drawCircle <$> mouse
```

We take mouse positions:

```haskell
mouse :: Dyn P2
```

The `P2` is 2D vector. And we map the positions to the pictures:

```haskell
drawCircle :: P2 -> Draw
drawCircle pos = do
  background white    -- clear background to white
  translate pos       -- translate center to position pos
  fill green          -- set fill color to green 
  stroke green        -- set stroke color to green
  circle 30 0         -- draw circle with radious 30 at the point (P2 0 0)
```

Note that to draw animations in processing we need to clear the screen 
first to draw. Otherwise the next frame will be drawn on top of
the previous one.

The `P2` is a type for 2D points. Also there is a type for 3D points.
As processing can work in 3D.

### User IO

Instead of getting events over event loop we have 
event streams and dynamics that read the user input. 

Here is most useful of them:

```haskell
-- mouse input
mouse      :: Dyn P2
mouseRight :: Evt P2
mouseLeft  :: Evt P2
mouseWheel :: Evt Float

-- generic gloss events
data Click = Click (Either Key MouseButton) KeyState Modifiers P2

getClicks :: Evt Click

-- key input
keyUp   :: Key -> Evt Modifiers
keyDown :: Key -> Evt Modifiers

charUp   :: Char -> Evt Modifiers
charDown :: Char -> Evt Modifiers
```

See the module `Dyna.Proc.Run` for complete list of the user input functions.

### Conclusion

That's it. We have described the FRP binding to `processing-fro-haskell`.

Peculiarities comparing to `processing-for-haskell`:

* special type for game initialisation `Spec` which contains 
   processing `setup` function
* main function `runApp` expects `Run (Dyn Draw)` to evaluate
* the `Run` is a `Frp`-monad (`MonadIO` and `MonadBaseControl`)

FRP peculiarities:

* special wrappers for `Evt` and `Dyn` that hide generic monad
    which is fixed to the `Run` monad

* re-exports core FRP module `Dyna` with all functions
  specialized for wrapped `Evt/Dyn` types.

* internal mutable updates are done with `IORef`

------------------------------------------------------------------------------

* `=>` [Example](/dyna-processing/tutorial/01-example)
* Up: [Table of contents](/dyna-processing/tutorial-toc)

