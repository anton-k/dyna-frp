---
title: Quick Reference of dyna-gloss functions
---

All FRP core functions can be used (see the reference for `dyna` package).
Main types 
* `Evt a` - event streams
* `Dyn a` - dynamic values
* `Run a` - main app/frp monad
* `P2` / `P3`  - vectors (2D and 3D)
* `Pio a` - processing `IO`-monad
* `Draw` - a sequence of drawing instructions (`Pio ()`)
* `Click` - click event
* `Spec`  - initial config for the application

## Run the application

```haskell
runApp :: Spec -> Run (Dyn Draw) -> IO ()

-- init application, setup instructions
data Spec = Spec { spec'setup :: Pio () }
```

## User interaction

```haskell
-- mouse input
mouse       :: Dyn P2
mouseRight  :: Evt P2
mouseLeft   :: Evt P2
mouseMiddle :: Evt P2
mouseWheel  :: Evt Float

-- mouse pointer velocity
mouseV:: Dyn P2

-- mouse pointer acceleration
mouseA :: Dyn P2

-- detection of drag action
isDrag :: MouseButton -> Dyn Bool
drag   :: MouseButton -> Dyn P2
dragV  :: MouseButton -> Dyn P2

-- generic events
data Click = Click (Either Key MouseButton) KeyState Modifiers P2	

data KeyState = Up | Down

getClicks :: Evt Click

-- Time that has passed since previous step of simulation
timeInterval :: Dyn Float

-- key actions
keyUp    :: Key  -> Evt Modifiers
keyDown  :: Key  -> Evt Modifiers
charUp   :: Char -> Evt Modifiers
charDown :: Char -> Evt Modifiers
```

## Drawing pictures and other processing functions

See the docs for 
[processing-for-haskell](https://hackage.haskell.org/package/processing-for-haskell).

