---
title: Using dyna to create bindings to imperative libraries
---

Here we will see how we can use the library to create FRP-based 
bindings to various imperative libraries. We will study the 
common patterns that reappear during the implementation.

For now we have bindings to the libraries [brick]() (TUIs) and [gloss]() (Graphics, gaming).
But if you need to use it with your own favorite library and make it shine with FRP
you can find this chapter useful.


### How to make our own instances for `Frp`

Why do we need our own instances for `Frp` class? Often when we define UI library
we need some global context. And for that case `ReaderT env IO` monad is very handy.

Let's take a look at the real code example from the binding to the gloss graphics
library:

```haskell
-- | Monad that drives the application
newtype Run a = Run { unRun :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadBase IO, MonadRandom)

newtype StMRun a = StMRun { unStMRun :: StM (ReaderT Env IO) a }

instance MonadBaseControl IO Run where
    type StM Run a = StMRun a
    liftBaseWith f = Run $ liftBaseWith $ \q -> f (fmap StMRun . q . unRun)
    restoreM = Run . restoreM . unStMRun

instance D.Frp Run where
  type Ref Run = IORef
```

So to have the access to global parameters of simulation we define
a reader monad wrapper `Run`. And we define the usual chain of `mtl`-instances.
Next we do boiler-plate code to define instance for `MonadBaseControl`.
We define special wrapper type `StMRun` and make an instance. 
I usually copy-paste it from project to project.

The instance of `Frp` is interesting. For we are going to choose 
the data type for mutable references.

```haskell
instance D.Frp Run where
  type Ref Run = IORef
```

For interactive fast application and gaming we want it to be fast and we can sacrifice safety for that. So we use `IORef`.
Also we could choose `TVar` from the `stm` package for safer alternative.
`TVar` is safer with respect to concurrency. Because our processes run in many threads.
The type `Env` holds some global variables useful to interact with gloss application.


### Channels. Interaction with imperative world

We have mentioned that we have access to global parameters of gloss game simulation.
But how do we communicate that to our FRP framework? Enter the channel event streams!

{{< figure src="/images/channel-evt.svg" alt="Monoid events" width="95%" >}}

The channel event streams is special family of event streams that going to 
trigger our callback whenever something is written to concurrent channel queue. 
We can choose between several types of channels: `TVar`, `MVar`, `unagi`-channels:

```haskell
mchanEvt :: Frp m => m (Control.Concurrent.Chan.Chan a) -> Evt m a

tchanEvt :: Frp m => m (TChan a) -> Evt m a

uchanEvt :: Frp m => m (InChan a) -> Evt m a
```

They have various properties. It's safe to start with `uchanEvt`.
How does it work? It takes in a reference to the channel. And starts to listen to it
(in duplicate mode). Whenever something happens it triggers our callback.

This is very flexible way to interact with imperative world. 
Take for example the `gloss` bindings. We would like to listen to the gloss events (mouse clicks,
mouse position, key presses etc.)

For that we create special channels and keep them in the environment of the `ReaderT` Run monad:

```haskell
-- | Applicaition environment
data Env = Env
  { env'frameChan   :: D.UChan Float
  , env'eventChan   :: D.UChan Event
  , env'resizeChan  :: D.UChan (Int, Int)
  , env'keyChan     :: D.UChan Click
  , env'mousePos    :: IORef Vec
  }
```

Whenever event happens in the gloss loop we just write it to the channel.
Then to make it convenient to use in the FRP setting we read the environment
and create event stream out of those channels. For example here is definition
that listens for the clicks in the system:

```haskell
-- | Reads generic click events
getClicks :: Evt Click
getClicks = Evt $ D.uchanEvt $ fst <$> asks env'keyChan
```

As simple as that. We read a dedicated channel with reader method `asks`.
And we create event stream out of it. Also there is a wrapper to hide the monad parameter.

This trick is used everywhere to communicate between FRP world and imperative engines.

### Hiding the monad parameter

I find it convenient for the user to define specialized version of generic
`Evt` and `Dyn` types:

```haskell
newtype Evt a = Evt { unEvt :: D.Evt Run a }
  deriving (Functor, Semigroup, Monoid, Melody, ...)

newtype Dyn a = Dyn { unDyn :: D.Dyn Run a }
  deriving (Functor, Applicative, Num, Fractional, Semigroup, Monoid, IsString,...)
```

And with those functions we get simpler and nicer signatures for
FRP functions:

```haskell
hold :: a -> Evt a -> Dyn a

apply :: Dyn (a -> b) -> Evt a -> Evt b

scan :: (a -> b -> b) -> b -> Evt a -> Evt b
```

It brings lots of boilerplate code to wrap/unwrap. In the end
for me it's worth it, as it simplifies the life of the user of the library
dramatically. And type mismatch are much nicer to read comparing
to generic variants of core FRP-types.

### Structure of the bindings to the State machine library

Many imperative libraries describe computation
as a state update function. Structure can be like this

```haskell
data App state = App
  { draw         :: state -> IO View
  , updateState  :: Event -> state -> IO state
  }

runLib :: App state -> IO ()
```

This structure is used in gloss and in brick libraries for example
with minor variations. We have one function that draws 
the state on the screen `draw` and another one that updates 
the `state` based on incoming events.

And the `View` and `Event` types are from the imperative library.
They define how to draw the widgets and animations on the screen
and which event types are supported. It can be for example
mouse right button or key-board press, window resize etc.

It's very easy to make FRP binding for such a library. 
Our main function will look like this:

```haskell
newtype Run = Run {...}

runApp :: Run (Dyn View) -> IO ()
```

The general idea is that we create main `Run`-monad which
is based on `ReaderT Env IO`.  Where environment holds
references to concurrent channels. It can be TChan, or unagi chan.
Also if we know that some value changes very rapidly we can 
create mutable `IORef` to store the current value. For example
it can be suitable for events that carry mouse position. 
All those references and concurrent channels are stored in the `Env`.
And we have access to them from our FRP eDSL over the `Run`
monad which is reader. So to get the low-level events
we can create event stream that reads from environmental channels.
See the functions `uchanEvt`, `tchanEvt` etc.


Prior to the imperative loop of the app we will create 
the environment with allocated channels and mutable variables.
We will use it as an argument for a `runReaderT`.

```haskell
runApp mdyn = do
  env <- newEnv
  viewRef <- runReaderT (unRun (runDyn mdyn)) env
  runLib $ App
    { draw        = \state -> readDyn viewRef
    , updateState = \event state -> ....
    }
```

And we will run the dynamic process that produces views. 
And we will use it in the `draw` callback function.

To propagate the state to the FRP-expression we
will write events to the dedicated channel inside
the `updateState` callback:

```haskell
  env <- newEnv
  ...
  runLib $ App
    { draw        = ...
    , updateState = \event state -> case event of
       MousePos x y -> writeIORef (envMousePos env) (x, y)
       _            -> writeChan (envEventChan env) event
    }
```


On exit from the event loop we can gracefully shutdown the dynamic process:

```haskell
  runLib App {..}
    `finally` (cancelDyn viewRef)
```

So for now we know how to render views and how to send
low level events from event loop. To read them in FRP-style
we will read the channel from environment and create 
event stream that listens to that channel.
Here is an example definition from the binding to gloss library:

```haskell
import Dyna qualified as D

newtype Evt a = Evt { unEvt :: D.Evt Run a }

-- | Reads generic click events
getClicks :: Evt Click
getClicks = Evt $ D.uchanEvt $ fst <$> asks env'keyChan
```

We have created wrapper for `Evt` and `Dyn` types to hide the fixed monad.
And in the function `getClicks` we can see how click events
are read from the environment variable `env'keyChan`.
This outline of implementation is used in the package `dyna-gloss`.

We can see how easy it is to pass events from imperative event loop
to our FRP application. 

Also we can work in different direction. 
If our application expects some special events
we cam run them prior to the event loop in background
process and we can pass them to the event loop over state of 
the imperative application. This is how it is implemented for brick bindings.

It expects event stream with special event that signals when application is 
shut down. We pass it as a callback to event stream that writes it
to channel of special type on which brick listens to outside events.
You can study the source code for `dyna-brick` library to see how it's 
implemented.

------------------------------------------------------------------------------

* `<=` [Control flow. Recursion and Sharing](/dyna-core/tutorial/06-control-flow)
* `=>` [Parsers for event streams](/dyna-core/tutorial/08-parser)
* Up: [Table of contents](/dyna-core/tutorial-toc)
