---
title: Quick Reference of dyna-gloss functions
---

All FRP core functions can be used (see the reference for `dyna` package).
Main types 
* `Evt a` - event streams
* `Dyn a` - dynamic values
* `Run a` - main app/frp monad
* `Vec`   - vectors
* `Point` - same as vector
* `Picture` - drawing
* `Click` - click event
* `Spec`  - initial config for the application

## Run the application

```haskell
runApp :: Spec -> Run (Dyn Picture) -> IO ()

-- init application
data Spec = Spec	 
  { spec'display     :: Display    -- ^ display settings
  , spec'background  :: Color      -- ^ background color to clear each frame
  , spec'steps       :: Int        -- ^ number of steps for simulation
  }

defSpec :: Spec

-- | Describes how Gloss should display its output.
data Display
  -- | Display in a window with the given name, size and position.
  = InWindow   String (Int, Int) (Int, Int)

  -- | Display full screen.
  | FullScreen
```

## User interaction

```haskell
-- mouse input
mouse      :: Dyn Vec
mouseRight :: Evt Vec
mouseLeft  :: Evt Vec
mouseWheel :: Evt Float

-- mouse pointer velocity
mouseV:: Dyn Vec

-- mouse pointer acceleration
mouseA :: Dyn Vec

-- detection of drag action
isDrag :: MouseButton -> Dyn Bool
drag   :: MouseButton -> Dyn Vec
dragV  :: MouseButton -> Dyn Vec

-- generic events
data Click = Click Key KeyState Modifiers Vec	

getClicks :: Evt Click

getFrames :: Evt Float
getResize :: Evt (Int, Int)

-- key actions
keyUp    :: Key  -> Evt Modifiers
keyDown  :: Key  -> Evt Modifiers
charUp   :: Char -> Evt Modifiers
charDown :: Char -> Evt Modifiers
```

## Drawing pictures

```haskell
type Path = [Point]

instance Monoid Picture

-- simple shapes
polygon :: Path  -> Picture
line    :: Path  -> Picture
circle  :: Float -> Picture
thickCircle :: Float -> Float -> Picture
arc :: Float -> Float -> Float -> Picture
thickArc :: Float -> Float -> Float -> Float -> Picture

-- text
text :: String -> Picture

-- bitmap
bitmap :: BitmapData -> Picture

-- picture transformations
color     :: Color -> Picture -> Picture
translate :: Vec   -> Picture -> Picture
rotate    :: Float -> Picture -> Picture
scale     :: Vec   -> Picture -> Picture

-- compound shapes
lineLoop    :: Path -> Picture
circleSolid :: Float -> Picture
arcSolid    :: Float -> Float -> Float -> Picture
sectorWire  :: Float -> Float -> Float -> Picture

-- rectangles
rectanglePath       :: Float -> Float -> Path
rectangleWire       :: Float -> Float -> Picture
rectangleSolid      :: Float -> Float -> Picture
rectangleUpperPath  :: Float -> Float -> Path
rectangleUpperWire  :: Float -> Float -> Picture
rectangleUpperSolid :: Float -> Float -> Picture
```

## Working with vectors

```haskell
-- vector-space functions
-- additive
zeroV   :: Vec
(^+^)   :: Vec -> Vec -> Vec
negateV :: Vec -> Vec
(^-^)   :: Vec -> Vec -> Vec

sumV    :: f Vec -> Vec

-- vector space
type Scalar Vec = Float
(*^) :: Float -> Vec -> Vec

-- inner space
<.>  :: Vec -> Vec -> Float
magnitude   :: Vec -> Float
magnitudeSq :: Vec -> Float
normalized  :: Vec -> Vec
project     :: Vec -> Vec -> Vec

-- linear interpolation
lerp        :: Vec -> Vec -> Float -> Vec
linearCombo :: [(Vec, Float)] -> Vec

-- affine space
(.-.) :: Vec -> Vec -> Vec
(.+^) :: Vec -> Vec -> Vec
distance   :: Vec -> Vec -> Float
distanceSq :: Vec -> Vec -> Float


-- gloss functions
magV :: Vec -> Float
argV :: Vec -> Float
dotV :: Vec -> Vec -> Float
detV :: Vec -> Vec -> Float
mulSV :: Float -> Vec -> Vec
rotateV :: Float -> Vec -> Vec
angleVV :: Vec -> Vec -> Float
normalizeV :: Vec -> Vec
unitVecAtAngle :: Float -> Vec

-- utility
e :: Float -> Vec  -- shortcut for unitVecAtAngle

-- conversions
fromTuple :: (Float, Float) -> Vec
toTuple   :: Vec            -> (Float, Float)
```

## Colors

```haskell
makeColor :: Float -> Float -> Float -> Float -> Color

mixColors :: Float -> Float -> Color -> Color -> Color

-- predefined colors:
black, white, red, green, blue, yellow, cyan, magenta, 
  rose, violet, azure, aquamarine, chartreuse, orange :: Color

greyN :: Float -> Color

-- color transformations:

dim, bright, light, dark :: Color -> Color
addColors :: Color -> Color -> Color
```
