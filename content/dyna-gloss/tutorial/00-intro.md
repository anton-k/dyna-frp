---
title: FRP for gloss gaming/animation library
---

In this tutorial we will study the FRP binding to the 
[gloss](https://hackage.haskell.org/package/gloss) library.
The `gloss` is suitable to create animations and games.
It features declarative description of drawings.
The application is run with state-machine like approach.
We have a function that renders the state of application
on the screen and we have a function that updates the state
on events.

With FRP we use the same functions for drawing as they are
defined in the gloss 
(see the module [Graphics.Gloss.Data.Picture](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Picture.html)).

Only to run the application we use FRP approach. The main function is:

```haskell
runApp :: Spec -> Run (Dyn Picture) -> IO ()
```

Where `Spec` defines the initial settings of the gloss application:

```haskell
data Spec = Spec	 
  { spec'display     :: Display    -- ^ display settings
  , spec'background  :: Color      -- ^ background color to clear each frame
  , spec'steps       :: Int        -- ^ number of steps for simulation
  }

defSpec :: Spec
```

The parameters come from the `gloss` library. For docs on display study the module 
[Graphics.Gloss.Data.Display](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Display.html).

So to run the application we need to provide the spec and dynamic picture:

```haskell
runApp :: Spec -> Run (Dyn Picture) -> IO ()
```

The `Picture` is a standard class of drawing primitive from the `gloss` library.
The `Run` is a monad that has instance of `Frp` class, i.e. we can
use `MonadIO` and `MonadBaseControl` methods with it. 
For writing internal mutable variables we use `IORef`.

The library `dyna-gloss` re-exports all drawing primitives from the `gloss` library.
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

import Dyna.Gloss

main :: IO ()
main = runApp defSpec $ pure pic

pic :: Dyn Picture
pic = (\pos -> translate pos $ color green $ circleSolid 50) <$> mouse
```

Let's save that to the file `HelloGloss.hs`.
To run the gloss programs we need to compile them first
and then we can run the executable:

```haskell
stack exec -- ghc -O2 -threaded HelloGloss.hs
./HelloGloss
```

In this example I use stack to compile the code. 
Of course you can use just `ghc` or `cabal` for that.
Be sure to pass the flags `-O2` and `-threaded`. It's important
for `gloss` to run.

We can press Alt+F4 to close the window and the application.
So let's study the code. What happens:

```haskell
pic = (\pos -> translate pos $ color green $ circleSolid 50) <$> mouse
```

We take mouse positions:

```haskell
mouse :: Dyn Vec
```

The `Vec` is 2D vector. And we map the positions to the pictures:

```haskell
(\pos -> translate pos $ color green $ circleSolid 50)
```

We use `translate` function to set the center of circle. 
Also we specify it's color and radius.

### Vec type

In the gloss Points and Vectors are defined as pair of floats:

```haskell
type Point = (Float, Float)
```

Using lazy tuples for such computation intensive domain as real-time
computer graphics is inefficient. We can accumulate unnecessary thunks
or lazy expressions. To avoid this problem in the `dyna-gloss` we use 
unboxed strict pairs of floats:

```haskell
data Vec = Vec
  { vec'x :: {-# UNPACK #-} !Float
  , vec'y :: {-# UNPACK #-} !Float
  }
  deriving (Show, Eq, Ord)
```

This will make computations more efficient. All functions
that work with pairs of floats in the gloss are redefined for `Vec`'s.

The definition of the `Vec` is in the module `Dyna.Gloss.Data.Vec`.
Also we have defined many instances from the 
[vector-space](https://hackage.haskell.org/package/vector-space) package.
This package defines convenient classes to work with vectors
in terms of Linear Algebra.

### User IO

Instead of getting events over event loop we have 
event streams and dynamics that read the user input. 

Here is most useful of them:

```haskell
-- mouse input
mouse      :: Dyn Vec
mouseRight :: Evt Vec
mouseLeft  :: Evt Vec
mouseWheel :: Evt Float

-- generic gloss events
data Click = Click Key KeyState Modifiers Vec

getClicks :: Evt Click

-- key input
keyUp   :: Key -> Evt Modifiers
keyDown :: Key -> Evt Modifiers

charUp   :: Char -> Evt Modifiers
charDown :: Char -> Evt Modifiers
```

See the module `Dyna.Gloss.Run` for complete list of the user input functions.

### Conclusion

That's it. We have described the FRP binding to `gloss`.

Peculiarities comparing to `gloss`:

* usage of strict vectors `Vec` instead of lazy tuples
* special type for game initialisation `Spec`
* main function `runApp` expects `Run (Dyn Picture)` to evaluate
* `vector-space` instances for vectors
* the `Run` is a Frp-monad (`MonadIO` and `MonadBaseControl`)

FRP peculiarities:

* special wrappers for `Evt` and `Dyn` that hide generic monad
    which is fixed to the `Run` monad

* re-exports core FRP module `Dyna` with all functions
  specialized for wrapped `Evt/Dyn` types.

* internal mutable updates are done with `IORef`

------------------------------------------------------------------------------

* `=>` [Example](/dyna-gloss/tutorial/01-example)
* Up: [Table of contents](/dyna-gloss/tutorial-toc)

