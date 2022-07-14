---
title: FRP for building terminal user applications (TUIs)
---

In this tutorial we will study the FRP binding to the 
[brick](https://hackage.haskell.org/package/brick) library.
The `brick` is suitable for creation of TUIs. Interactive
command line tools. 
It features declarative description of the terminal widgets.
The application is run with state-machine like approach.
We have a function that renders the state of application
on the screen and we have a function that updates the state
on events.

Brick is a wonderful library! We can make it even better
if we use FRP with it. With FRP we use the same functions for 
widgets as they are defined in the brick 
(see the module [Brick.Widgets.Core](https://hackage.haskell.org/package/brick-0.67/docs/Brick-Widgets-Core.html)).

Only to run the application we use FRP approach. The main function is:

```haskell
runApp :: Spec -> Run Win -> IO ()
```

The main element is `Win` it's a pair of dynamic `Box`
and event stream of actions `Act`.

```haskell
data Win = Win
  { win'widgets :: Dyn [Box]   -- ^ window view
  , win'acts    :: Evt Act     -- ^ brick app actions
  }

type Box = Widget BoxId
newtype BoxId = BoxId { getBoxId :: Text }
```

The `Box` is a `Widget` with argument to identify the widget
fixed to the type `WidgetId`. The `WidgetId` is based on `Text` representation
of widget identifier. We decided to simplify a bit the `Widget` data type
so that we don't need to pass around one more generic data-type parameter.

The `Act` is a command that is sent to the brick application.
So far it can only quit the app. Maybe in the future it will be extended
with other actions:

```haskell
-- | Actions for Brick rendering engine
data Act
  = Quit -- ^ Quit the app
```

So our main FRP data type contains the dynamic of widgets and event stream
that signals when to stop the app. It's important to define it because otherwise
we will have no way to stop the app.

The `Spec` describes the defaults for the app:

```haskell
data Spec = Spec
  { spec'attrMap :: AttrMap
  , spec'cursor  :: [CursorLocation BoxId] -> Maybe (CursorLocation BoxId)
  }
```

You can study the brick docs on what is `AttrMap` and `cursor` functions.
The `attrMap` defines a map of styling tags which we can attach to widgets
to change their appearance or properties. For example we can highlight the widget
with different color if we attach certain attribute from the `AttrMap` to it.
See the module [`Brick.AttrMap`](https://hackage.haskell.org/package/brick-0.67/docs/Brick-AttrMap.html#t:AttrMap)
on how to do it.

We can use the `defSpec` to start the application with sensible defaults.

```haskell
defSpec :: AttrMap -> Spec
```

We can pass `emptyAttrMap` f we don't need any styling features.

Let's look at the `runApp` signature again:

```haskell
runApp :: Spec -> Run Win -> IO ()
```

Also notice the `Run` type. It's a main `Frp`-monad for our application.
Inside we can do `IO`-actions with `MonadIO` instance.

### The first program

Let's define Hello World brick application. It will show a message "Hello Brick".
And we can quit it by pressing Enter:

```haskell
module Main where

import Dyna.Brick

main :: IO ()
main = runApp (defSpec emptyAttrMap) $ pure $ Win pic quit

pic :: Dyn [Box]
pic  = pure [str "Hello Brick"]

quit :: Evt Act
quit = Quit <$ onKey KEnter
```

Note that in the brick we need to create a list of widgets (boxes)
to render them on the screen.

## Main FRP types

The backbone monad of the application is `Run`-monad. 
We already described it. 

Also we have special wrappers for `Evt` and `Dyn` that
fix the generic monad argument parameter to `Run`:

```haskell
newtype Evt a = Evt { unEvt :: Dyna.Evt Run a }
newtype Dyn a = Dyn { unDyn :: Dyna.Dyn Run a }
```

All core FRP functions are re-defined for the wrapped types.
We can look them app in the docs for the module `Dyna.Brick.Frp`.

## User interaction

There are functions to render user input as event streams and dynamic values:

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

The types `Key`, `Modifier` and `Event` come from the `vty` library
on top of which brick runs. For convenience they are reexported by
the module `Dyna.Brick.Run`.

## Specific types for Brick

There are some type wrappers to make it easier the usage of the brick 
in the FRP setting. 

We have already encountered:

* `Win` - holds the dynamic value of widgets and event stream of when to quit the app
* `BoxId` - text based identifiers for widgets
* `Box` - widget with identifiers set to `BoxId`.

Also we have mouse event wrappers:

```haskell
-- | Mouse down events only
data MouseDownEvent = MouseDownEvent BoxId Button [Modifier] Location

-- | Mouse up events only
data MouseUpEvent = MouseUpEvent BoxId (Maybe Button) Location
```

## Interactive program

Let's define an interactive application. It will listen for the
keyboard input of chars and show it on the screen (we can find complete code
in the directory `dyna-brick/examples/ReadChars.hs`):

Let's setup a basic project:

```haskell
module Main where

import Dyna.Brick

main = runApp def (pure (Win widgets acts))

widgets = undefined
acts    = undefined
```

Let's start with the simplest part and define when we want to quit the app:

```haskell
acts = Quit <$ onKey KEsc <> onKey KEnter
```

So we want to quit if either Escape or Enter button is pressed.
For the widgets we want to display the last entered char
and show the message to the user how to quit the application:

```haskell
widgets :: Dyn [Box]
widgets = fmap (pure . footer) $ hold (str "Hello Brick!") chars
  where
    chars = fmap (\ch -> str $ "Pressed: " <> pure ch) readChars
    footer w = vBox [w, str "Type any char", str "Press Esc or Enter to exit"]
```

In `chars` we read any char input and map it to the input with nice message:

```haskell
    chars = fmap (\ch -> str $ "Pressed: " <> pure ch) readChars
```

In the footer we just inform the user how to quit the app:

```haskell
    footer w = vBox [w, str "Type any char", str "Press Esc or Enter to exit"]
```

Note how we turn event stream of widgets to the dynamic value with `hold`
function:

```haskell
   hold (str "Hello Brick!") chars
```

That's it! Here is the complete code:

```haskell
module Main where

import Dyna.Brick

main = runApp def (pure (Win widgets acts))

widgets :: Dyn [Box]
widgets = fmap (pure . footer) $ hold (str "Hello Brick!") chars
  where
    chars = fmap (\ch -> str $ "Pressed: " <> pure ch) readChars
    footer w = vBox [w, str "Type any char", str "Press Esc or Enter to exit"]

acts :: Evt Act
acts = Quit <$ onKey KEsc <> onKey KEnter
```

### Conclusion

We have described the FRP binding to `brick`.

Peculiarities comparing to `brick`:

* making `Widget` specific in type parameter with text based `BoxId` identifier.
* main function `runApp` expects `Run (Dyn Win)` to evaluate
* the `Run` is a Frp-monad (`MonadIO` and `MonadBaseControl`)
* the `Win` holds dynamic value of widgets and event stream of quit-messages
   to stop the application.

FRP peculiarities:

* special wrappers for `Evt` and `Dyn` that hide generic monad
    which is fixed to the `Run` monad

* re-exports core FRP module `Dyna` with all functions
  specialized for wrapped `Evt/Dyn` types.

* internal mutable updates are done with `TVar`

------------------------------------------------------------------------------

* `=>` [Example: Puzzle 15](/dyna-brick/tutorial/01-puzzle-15)
* Up: [Table of contents](/dyna-brick/tutorial-toc)

