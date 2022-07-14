---
title: Quick reference for dyna-brick
---

All FRP core functions can be used (see the reference for `dyna` package).

Main types:
* `Evt a`  - event streams
* `Dyn a`  - dynamic values
* `Run a`  - main app/frp monad
* `BoxId`  - text based id for the widget
* `Box`    - widget with id parameter set to `BoxId`
* `Act`    - brick app actions (for example `Quit`)
* `Win`    - pair of dynamic widgets and event stream of quit-actions

## Run the application

```haskell
runApp :: Spec -> Run Win -> IO ()

data Spec = Spec
  { spec'attrMap :: AttrMap
  , spec'cursor  :: [CursorLocation BoxId] -> Maybe (CursorLocation BoxId)
  }

defSpec :: AttrMap -> Spec
emptyAttrMap :: AttrMap

data Win = Win
  { win'widgets :: Dyn [Box]   -- ^ window view
  , win'acts    :: Evt Act     -- ^ brick app actions
  }

type Box = Widget BoxId
newtype BoxId = BoxId { getBoxId :: Text }

-- | Actions for Brick rendering engine
data Act
  = Quit -- ^ Quit the app
```

## User interaction

```haskell
-- generic terminal events
vtyEvents :: Evt Event

-- mouse input
mouseUp   :: Evt MouseUpEvent
mouseDown :: Evt MouseDownEvent

data MouseUpEvent   = MouseUpEvent BoxId (Maybe Button) Location
data MouseDownEvent = MouseDownEvent BoxId Button [Modifier] Location

-- keyboard input
keyEvents :: Evt (Key, [Modifier])
onChar    :: Char -> Evt [Modifier]
onKey     :: Key -> Evt [Modifier]
readChars :: Evt Char
```

## Drawing Widgets

See the modules

* [Brick.Widgets.Core](https://hackage.haskell.org/package/brick-0.67/docs/Brick-Widgets-Core.html)
* [Brick.AttrMap](https://hackage.haskell.org/package/brick-0.67/docs/Brick-AttrMap.html)

