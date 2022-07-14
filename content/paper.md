---
title: Callback heaven for FRP
---

## Abstract

The Functional reactive programming provides a declarative solution
to the task of creation of interactive applications. Often it is contrasted
to the imperative approach that relies on callback handlers, event loops
and mutation of the state which is quickly can become hard to understand and 
maintain. In this work we show how with imperative
definition we can build DSL that covers all combinators of the FRP approach.
We will go to the center of imperative hurricane to build declarative
interface that is easy to understand, use and extend. Because of imperative
nature of the definitions it simplifies the creation of bindings to imperative
systems as they speak the same language at the core of the library. Also we show application 
of our approach to various domains such as Animation, gaming, UIs, server-side
web programming.

## Introduction

Working with time is a hard task, because everything starts to depend on context 
of execution and order of calls to procedures. One of the main merits of the 
functional programming and pure functions is that time does not interfere with
execution of the program. Pure functions produce the same result regardless of when 
they are called. Effectful procedures tend to update
mutable state and it is hard to track the updates. As a solution to this problem
FRP treats the whole sequence of events that trigger updates as a single entity
and we can establish relationships between streams of events. And work with them
with convenient interface that looks as if we work with lists. 

By treating the event streams as a whole we surpass the problem of event ordering
because we process the infinite stream of events as a whole. But this introduces 
its own problems. We need to be careful to not to introduce the space leaks because
some events that should be forgotten are kept in memory. Also it can be hard to 
bridge the imperative and declarative approaches because at the end of the day often we
use imperative frameworks as low-level implementation of FRP ideas. Take for example `OpenGL`
or `WxWidgets` which are inherently imperative.

In this paper we propose an implementation for FRP system which is imperative
at its core. We use powerful Haskell features such as lazy evaluation and 
concurrency to implement FRP combinators on top of imperative definitions.
And we make the imperative world composable and to the end user it seems
that the whole model is declarative and reactive. 

Because implementation is imperative at the core it is
easy to implement bindings to such a system and core types a very easy to grasp
and use as example of FRP implementation for education.

Our main contributions:

* Minimal and elegant implementation for FRP which is based on imperative approach.
  
* Use of concurrency in the FRP system

* Generic monad at the core of the FRP system

* Implementation of bindings to several domains: 
  animation (`gloss`, `processing-for-haskell`), UIs (`brick`), web-servers (`scotty`)

It was implemented in the library `dyna`. It is open source and available on Hackage.
The name comes as a short name for "dynamic value".

## FRP refresher

In this section we provide a short overview of functional reactive programming (FRP).
It is a declarative approach to creation of interactive applications. 
It studies continuous processes (called Behaviors or Signals) and infinite
streams of events (called Events). Instead of reacting to the user input
and updating the current state we use typical functional programming routines
like map, fold, filter, monoidal interface to establish relationships between
user input and drawings of widgets on the screen.

We have two main types: Behaviours or continuous signals and event streams.

By the meaning the Behavior can be though of continuous function from `Time` to values:

```haskell
type Behavior a = Time -> a
```

And event stream is possibly infinite list of events, i.e. values that happen on certain time:

```haskell
type Events a = [(Time, a)]
```

The magic starts to happen when we bring typical functional programming functions
to the domain of FRP. We use interface for FRP as it was designed in []:

Typical interface for `Behaviors`:

```haskell
instance Functor Behavior
instance Applicative Behavior
```

From the `Applicative` instance we can derive many useful instances that lift
some instances of the values to the instances of the signals:

```haskell
instance Monoid a     => Monoid (Behabior a)
instance Num a        => Num (Behavior a)
instance Fractional a => Fractional (Behavior a)
instance IsString a   => IsString (Behavrior a)
...
```

The interface for event streams also can be expressed with standard type-classes.
We can map over values of events with `Functor`

```haskell
instance Functor Events where
```

We can join several event streams together with instance of `Monoid`:

```haskell
instance Semigroup (Event a) where
instance Monoid    (Event a) where
```

It is important that we can join events regardless of their type. Whereas for Behaviors
we required that parameter of Behavior is also `Monoid`. Conceptually we can think on join
of events as of merge of two lists of events which are sorted by time of event occurrence.
Also event streams support operations typical for lists:

```haskell
filter :: (a -> Bool) -> Events a -> Events a
take   :: Int         -> Events a -> Events a
drop   :: Int         -> Events a -> Events a
scan   :: (s -> a -> s) -> s -> Events a -> Events s
... etc ... 
```

We need some way to interact between events and behaviors:

```haskell
stepper :: a -> Events a -> Behavior a
apply   :: Behavior (a -> b) -> Events a -> Events b
```
The stepper function creates step-wise behavior that starts with pure 
initial value (the first argument) and when event happens on the event stream (second argument)
it starts to produce that value instead until the next event will happen and overwrite the current
value. The `apply` function applies the dynamic function to the value of event stream when it happens.
The result stream is going to have the same timestamps for events as the input stream
only we are going to map with function which also dynamically changes over time.

We can switch between behaviors and events:

```haskell
switchE :: Events (Events a) -> Events a
switchB :: Behavior a -> Events (Behavior a) -> Behavior a
```

The `switchE` flattens the event streams. It starts to produce the events
from the first event stream that happens until the next event stream will come and
substitute the previous one and so forth.
In the `switchB` we flatten the event stream of behaviors to a single behavior.
It works just like `stepper` only we work not with constant values within the segments
but with behaviors. 

On top of those basic functions we can construct many more primitives but this
is the core of FRP system. There are many ways to implement FRP. Mostly they are 
divided on classical FRP which implements described interface and Arrowised FRP (AFRP) which
introduce the notion of signal function. It is a function which can be thought of 
as iterative transformation of the `Behavior`. 

We propose imperative implementation that is based on the notion of callback procedure.
It turns out that we can create flexible and powerful set of combinators that cover
the whole range of FRP interface with that. But prior to that let us take
a broad view on the problem and study the meaning of the FRP entities from 
the philosophical perspective.

## Imperative definition

What does it mean to be a continuous process or stream of events from the user point of view?
We have just outlined one possible solution described in types:

```haskell
type Behavior a = Time -> a
type Events a = [(Time, a)]
```

It introduces the relation of behaviors and streams to a time. 
We assume that there is a time line in which we observe the events or values 
of behaviors. 

What if we can eliminate the time completely? If instead of thinking 
about what event is we can think about why do we need it in the first place.
This is kind of categorical view on the problem when we study not what objects
are but how they are related to other objects.

For our implementation we assume that user needs event stream to react
on events and it does not matter when event will happen as long as we
know that some procedure is going to be called in that moment.

We can picture it:

{{< figure src="/images/run-evt.svg" alt="Run event" width="90%" >}}

We define the event stream of `a` in terms of function `runEvt` which 
takes in a callback procedure `a -> m ()` and does something
useful with it whenever event happens. The `m` is some monad
that is akin to `IO`. But when does it going to happen
is completely hidden from the user.

Also we can think of behaviors in terms of how they can be used in real world.
Take for example the current temperature of the weather. 
We can try to forecast it but the only reliable thing we can do is to measure
it in the current time. For us behavior is kind of reference to the mutable
process that evolves in background and the only thing we can do is to query
the current value of the process.

```haskell
readDynRef :: DynRef m a -> m a
```

Also as the dynamic value is a background process we need some way to
gracefully shut it down:

```haskell
cancelDyn :: DynRef m a -> m ()
```

We are going to call behaviors of this kind dynamic values (`Dyn` for short in the code)
If we have some definition of the process for dynamic value we can run it in background
and get the reference to it:

```haskell
runDyn :: Dyn m a -> m (DynRef m a)
```

So we have two imperative definitions:

The event stream `Evt` is a callback consumer function:

```haskell
newtype Evt m a = Evt { runEvt :: (a -> m ()) -> m () }
```

And the dynamic value is some background process to which we have access
over reading its current value. We can read it only at the current moment:

```haskell
runDyn    :: Dyn    m a -> m (DynRef m a)
readDyn   :: DynRef m a -> m a
cancelDyn :: DynRef m a -> m ()
```

We need to refine the notion of `Dyn`. What type of process is it?
We define it in terms of event streams. It is observation of current value
of some underlying event stream. 

We are going to have some initial value, event stream and getter function:

```haskell
data Dyn m a = forall s . Dyn 
  { dyn'init    :: s
  , dyn'evt     :: Evt m s
  , dyn'get     :: s -> m a
  }
```

So at the beginning of the process we observe the initial value `dyn'init`
and produce (`dyn'get dyn'init`) until the next event will happen. We store it
in the mutable value (it can be `IORef` for example). When it will happen we will 
update the mutable value and start to produce another value. 
We use trick with `forall` to hide away the internal representation of the state
from the user. The implementation in the library also contains a method:

```haskell
dyn'release :: m ()
```

It holds procedure to release resources allocated for the work of event stream `dyn'evt`
but we omit it for the sake of simplicity.

At the core of our system is a notion of event stream as callback consumer.
The dynamic value is an observation of some event stream. It turns out that
with this imperative definition we can implement all functions from the classical FRP
interface. 

Let us start with interfaces for event streams. As we go we will encounter
constraints for the monad `m`. 

## Event stream interface

Event streams are the core of our system as dynamic values are defined in terms 
of event streams. The vent streams are callback consumer functions. The 
combinators for them are going to take the input callback functions and 
append some useful functionality on top of them.

### Event stream constructors

The simplest possible event stream is `never`. It never happens
and the implementation just ignores the callback and returns:

```haskell
never :: Monad m => Evt m a
never = Evt $ \_ -> pure ()
```

The next basic primitive is `once`. It is some event that happens only once
right at the start:

```haskell
once :: Monad m => m a -> Evt m a
once act = Evt $ \go -> go =<< act
```

It just invokes the callback once and returns. For example with `once` 
we can create an event stream that queries user for input:

```haskell
getLineE :: Evt IO a
getLineE = once getLine
```

To print the events we can define useful functions that calls `print` as  a callback:

```haskell
printE :: Show a => Evt IO a -> IO ()
printE evt = runEvt evt print
```

### Sequential composition

With our definition of events we can introduce sequential composition.
As we define our event stream as monadic procedure it makes it simple
to define the function that implements one monadic action after another one.
It is when we have two event streams that produce the events of the same type
we can run the first one with our callback and when it finishes we can the second one:

```haskell
seqE :: Monad m => Evt m a -> Evt m a -> Evt m a
seqE a b = Evt $ \go -> do
  runEvt a go
  runEvt b go
```

Note how the nature of monad being a sequence of procedures
produces very simple definition for sequencing of event streams.
With this function we can ask user for input several times:

```haskell
getLineE `seqE` getLineE 
```

Also using the same idea it is easy to provide the `foreverE`
function that calls event stream in infinite loop. We just lift
the standard function for monads `forever` to the event streams:

```haskell
foreverE :: Monad m => Evt m a -> Evt m a
foreverE e = Evt $ \go -> forever (runEvt e go)
```

Note how sequential composition does not rely on the notion of time.
It is defined in terms of one goes after another one finishes
but introduction of the time is still avoided.

### Parallel composition

With concurrent libraries we can define the parallel composition of 
event streams. Two event streams happen concurrently if they run at
the same time and we pass the same callback to them and whenever 
event happens on any of the streams it triggers the callback procedure:

```haskell
parE :: MonadBaseControl m => Evt m a -> Evt m a -> Evt m a
parE a b = Evt $ \go -> concurrently_ (runEvt a go) (runEvt b go)
```

We use the function `concurrently_` from the library `async-lifted` to
run both event streams at the same time.The class `MonadBaseControl`
let us run processes concurrently not only on top of `IO` monad
but on any monad that is instance of it. It gives us great flexibility 
over the choice of the underlying monad.

### Frp-monad class

For the sake of brevity we define the `Frp`-class for a monad
that is suitable for using with `Evt`, `Dyn` and FRP interface:

```haskell
class MonadBaseControl m => Frp m where
```

### Monoid instance

With `never` we can define Monoid either with sequential composition
or with parallel composition. In FRP libraries the Monoid instance
usually means parallel composition:

```haskell
instance Frp m => Semigroup (Evt m a) where
  (<>) = parE

instance Frp m => Monoid (Evt m a) where
  mempty = never
```

### Functor instance 

It is also useful to be able to transform event streams. 
For example if we want to read only integer input from the user:

```haskell
readInt :: String -> Maybe Int

readIntE :: Evt IO (Maybe Int)
readIntE = foreverE (fmap readInt getLineE)
```

We assume the existence of the function `readInt` that reads only integers
from the input. 

In the definition of the `Functor` we just apply the function to transform
the input prior to callback invocation:

```haskell
instance Frp m => Functor (Evt m) where
  fmap f e = Evt $ \go -> runEvt e (go . f)
```

### List-like functions

What makes the definition of applications in FRP-style great is
that many combinators look like functions for ordinary list.
We work with event streams as if they are lists although under the hood
thy are functions with side effects. Let us look at couple of definitions.

#### Filtering

Let's define some list-like functions. Let us take for example `filterE`:

```haskell
filterE :: Frp m => (a -> Bool) -> Evt m a -> Evt m a
filterE cond e = Evt $ \go -> 
  runEvt e $ \x -> when (cond x) (go x)
```

Once again we don't need the notion of time to filter the events.
We just ride on top of the monad function `when` that can skip 
execution of monadic actions based on condition. 

#### Scanning

The scan function accumulates the state with some function
whenever event happens on the stream:

```haskell
scan :: (a -> st -> st) -> st -> Evt m a -> Evt m st
```

This function is a bit tricky. As to implement it we are going to need the mutable
variables. We need to store the current state and update it on the event.
For simplicity we take the `IORef` to store the state but the implementation also 
provides `TVar` to store values more safely in the presence of 
possible concurrent updates.

```haskell
scan f init e = Evt $ \go -> do
  ref <- liftIO (newIORef evt s)
  runEvt evt $ \x -> do
    s <- f x <$> liftIO (readIORef ref)
    go s
    liftIO $ writeIORef ref s
```

We allocate a mutable reference under the hood and update it on every trigger
of the callback function. And this is all hidden from the user behind
the declarative interface of function `scan`.

In this manner we can implement many list functions. But we stop for now
and resume with interface for dynamics.

### Rock Paper Scissors game

We would like to provide the feel of usage of FRP library that
we have just defined. As example we list implementation 
of the game of Rock Paper Scissors. In the game two players show random
hand gestures to each other. There are three gestures: Rock, Paper and Scissors.
And Paper wins over Rock, Rock wins over Scissors and Scissors win over Paper.
If gestures are the same it is a draw.

It takes just a 7 lines of FRP-code to implement the logic of the game
and uses only event stream functions.
Here is the complete code for the FRP-part of the Rock-Paper-Scissors game.
The pipe operator `|>` is just flipped `$` application operator:

```haskell
data Move = Rock | Paper | Scissors
data Score = Score {...}
  
toScore    :: Move -> Move -> Score    -- convert moves to score for a single round
printScore                             -- pretty prints the score
           :: RoundId -> Score -> String
total      :: Int                      -- number of rounds

game :: Evt IO String
game =
  once getLine |> foreverE |>           -- read user input
  mapMay (readMaybe @Move) |>           -- take only valid moves
  takeE total |>                        -- take only so many moves
  withOneOf [Rock, Paper, Scissors] |>  -- add random AI move as first element of the tuple
  foldMapE (uncurry $ flip toScore) |>  -- get the score and accumulate scores
  withCount |>                          -- append the round number
  fmap (printScore total)               -- print the current score
```

The steps are explained in the comments. We just create the infinite
stream of user input and filter only valid moves. After that we take
only so many moves as it's needed by the game. We append to the user's
move the random move of the game-engine and `foldMap` it with conversion
to the score. The function `withCount` appends the current count of
the events so far. As the last stage we pretty print the scores on 
each round.

## Interface for dynamic values

We have defined the interface for the core type - event streams.
Let us dive into dynamic values. We can recall that dynamic values
are just event streams in disguise. We turn the streams to processes
by observation of the last event value that has happened on the stream.

Let us reminder the definition:

```haskell
data Dyn m a = forall s . Dyn 
  { dyn'init    :: s           -- ^ initial state
  , dyn'evt     :: Evt m s     -- ^ event stream that updates the state
  , dyn'get     :: s -> m a    -- ^ getter function for the state
  }
```

### Basic building blocks

We can create a dynamic from an effectful value:

```haskell
constDyn :: Monad m => m a -> Dyn m a
constDyn a = Dyn 
  { dyn'init = ()
  , dyn'evt  = never
  , dyn'get  = const a
  }
```

It uses the event that never happens to update the value. 
And the getter function `dyn'get` just uses the input to produce the value.

Also the definition of dynamic value lends itself to the
construction of dynamic values from event streams:

```haskell
hold :: Monad m => a -> Evt m a -> Dyn m a
hold init evt = Dyn
  { dyn'init = init
  , dyn'evt  = evt
  , dyn'get  = pure
  }
```
For dynamics we need to define only couple of interfaces: `Functor` and `Applicative`.
Let's start with the `Functor`.

### Functor instance

The `Functor` case is simple we just need to change our point of view
on the returning value with getter function:

```haskell
instance Fynctor m => Functor (Dyn m) where
  fmap f (Dyn init evt get) = Dyn init evt (fmap f . get)    
```

### Applicative functor instance

Let's define first an easy case for `pure`:

```haskell
instance Frp m => Applicative (Dyn m) where
  pure a = constDyn (pure a)
```

The bind of applicative functor is a bit tricky to get right because
it implies the construction of single event stream for internal
state update out of two argument streams. The main idea is to use monoid 
instance to run two event streams concurrently
and use a pair of states as a result state. And whenever any of the components 
updates we also update the internal pair of sates:


```haskell
  (<*>) :: Dyn m (a -> b) -> Dyn m a -> Dyn m b
  (<*>) (Dyn initF evtF getF) (Dyn initA evtA getA) = Dyn init evt get
    where
      init = (initF, initA)

      evt = Evt $ \go -> runEvt (fmap Left evtF <> fmap Right evtA) $ \(f, a) -> case x of
              Left  f' -> go (f', a)
              Right a' -> go (f, a')

      get (f, a) = pure (f a)
```

We have defined the main functions for dynamic values!

## Interaction between events and dynamics

Let's define application of the dynamic function to the event stream.
To do that we start to listen for dynamic values and each time
the callback is triggered on input stream we just read the
dynamic function by reference and apply it to callback argument:

```haskell
apply :: Frp m => Dyn m (a -> b) -> Evt m a -> Evt m b
apply dyn evt = Evt $ \go -> do
  ref <- runDyn dyn
  runEvt evt (\b -> do
    go . ($ b) =<< readDyn ref)
    `finally` cancelDyn ref
```

We need to be careful to stop the background process
when we do not need it anymore. For that we use the `finally`
primitive which is going to be called even if the exception 
will be thrown in the thread of the event stream.
The implementation for flattening functions `swithE` and `switchB`
is tricky story and we list it in the appendix for the interested reader.

## Where the time is

It is interesting that we still do not need to use the notion of time.
How should we interact with it? We can introduce it with
primitive which calls a procedure on the given time interval.
Assume that we have predefined function `preiodic`:

```haskell
preiodic :: Monad m => Time -> m () -> m ()
```

It calls procedure every so many seconds. It is easy to implement it
with `treadDelay` function which can wait in the thread.

With it we can define an event stream that calls callback periodically:

```haskell
pulse :: Frp m => Time -> Evt m ()
pulse t = Evt $ \go -> periodic t (go ())
```

This is enough to sample the timeline with the granularity that we would
like to have. With this event stream we can start to work with time
but the library of FRP-combinators itself does not need it for implementation.
This separation of interface from the time allows us to keep decisions of  
the granularity and preciseness of time orthogonal to the whole system.

## More fruits from Concurrent Haskell

As we saw the application of a function `concurrently_` from the `asyn-lifted` library.
We can use other functions to give concurrent flavor to the execution
of event streams. For example we can also use `race`:

```haskell
raceE :: Frp m => Evt m a -> Evt m a -> Evt m a
raceE a b = Evt $ \go -> race_ (runEvt a go) (runEvt b go)
```

It will run both of the events concurrently and finish both
of the event streams when one of them will finish. It can be useful
to timeout the execution of event streams.

Also we can create a combinator that handles all callbacks concurrently
by running each callback in a separate thread:

```haskell
forkE :: Evt m a -> Evt m a
forkE e = Evt $ \go -> runEvt e (fork . go)
```

## Recursion

It is often useful to use recursive event streams. For example
we can have a button widget which has dynamic attribute text on the button
and we would like to show how many times
the button was pressed with it. The code can look like this:

```haskell
clicks <- button (fmap show $ hold 0 $ count clicks)
```

Where `count` counts how many times event has occurred on the stream.
So we need clicks as output and as input of the function. 
We can not do it directly with our framework because event streams
are functions under the hood and we can not tie the knot over generic 
function definition. 

To solve this problem we introduce a combinator:

```haskell
fixE :: (Evt m a -> m (Evt m a)) -> Evt m a
```

It takes in a function which transforms the event streams and produces
a single event stream. It feeds the output stream of the function back to itself.
With this combinator we can implement the `button`:

```haskell
fixE $ \clicks -> button (fmap show $ hold 0 $ count clicks)
```

To implement this function we need to introduce another useful concept.
Event streams which are based on concurrent channels. 
We can initialise a channel (for example `TChan` from the library `stm`).
And in the event loop we can listen for messages and each time the message
occurs we are going to trigger the callback.

The definition for `TChan`:

```haskell
tchanEvt :: Frp m => TChan a -> Evt m a
tchanEvt chan = Evt $ \go -> do
  ref <- liftIO $ atomically $ dupTChan chan
  loop ref go
  where
    loop ref go = do
      a <- liftIO $ atomically $ readTChan ref
      go a
      loop chan go
```

With it we can implement the `fixE`. To redirect the events form output
to input we are going to create a channel and we will write all events
from the output to it and we will create an event stream from that channel
and apply it to the recursive function:

```haskell
fixE :: Frp m => (Evt m a -> m (Evt m a)) -> Evt m a
fixE f = Evt $ \go -> do
  chan <- liftIO newTChanIO
  let evt = tchanEvt chan
  evt' <- f evt
  runEvt evt' $ \x -> do
    liftIO $ atomically $ writeTChan chan x
    go x
```

## Sharing of event streams

We have defined a nice DSL of FRP combinators on top of our imperative definition
of event streams. But it's good to be aware of its limitations. 
Each event stream is an effectful function under the hood and we
combine them to produce new functions that can produce side effects too. 
This means that we accumulate the description of execution of some callback
rather than the execution itself. 

It can lead to confusion with the usage of Haskell sharing of the variables
with `let` or `where` expressions. For example we have a function:

```haskell
oneOf :: [a] -> Evt b -> Evt a
```

Which for every event in the second argument stream produces 
event that contains one random element from the list. 
If we use it in the code:

```haskell
let a = oneOf [True, False] evt
    b = a
in  f a b
```

It's useful to understand that `a` and `b` will produce two different
instances of the random process. More than that even in the expression:

```haskell
let a = oneOf [True, False] evt
in  f a a
```

In the function `f` on execution of callback procedure two arguments
of `f` will produce different random sequences although they point to the same
variable `a`. But it is important to understand that `a` contains a description
of the callback consumer and not the evaluation of it. 

Often we want to share the values between streams and to solve this problem
we introduce a special function:

```haskell
newEvt :: Evt m a -> m (Evt m a)
```

It creates a concurrent channel and starts to execute its argument event stream
with callback that writes values of events to the concurrent channel. 
As a result we return the event stream that listens to that channel. 

This way if we copy this description we copy the listener to the same channel and
both of the copies would receive the same events:

```haskell
  do
    a <- newEvt (oneOf [True, False] evt)
    pure (f a a)
```
In this expression the function `f` will receive the same 
events on execution from both `a`s. In the library we have the
same function to share dynamic values.

Some FRP libraries solve this problem by usage of `unsafePerformIO`
to update the network of mutable references to the vent streams and keep sharing consistent
with the sharing of the Haskell values. We decided not to go down this road
and keep it explicit for the user.

## Creation of bindings to imperative libraries

One of the strength of this approach to FRP implementation comes from
how easy it is to create bindings to imperative libraries. 
Because the underlying types use imperative approach too. 
The general technique is to use `ReaderT` monad with 
environment that contains concurrent channels or mutable variables
that are written from imperative evaluation loop of the imperative library.

Let's outline the structure of bindings to the `gloss` animation library.
It offers great declarative interface
for creation of animations but the main rendering loop functions
like an imperative state-machine. 
Here is a simplified version of the function (we omit initialisation parameters):

```haskell
playIO 
 :: world                               -- initial value of the state
 -> (world -> IO Picture)               -- render the state on the screen 
 -> (Event -> world -> IO world)        -- update the state on events
 -> (Float -> world -> IO world)        -- update the state on interation step
 -> IO ()
```

To use it with our FRP-library we create environment type which will
hold the events:

```haskell
newtype Env = Env { unEnv :: TChan Event }
```

And our main FRP-monad would be reader-transformer with access to that channel:

```haskell
type Run a = ReaderT Env IO a
```

Our main rendering function will take a dynamic value of pictures as input:

```haskell
runGloss :: Run (Dyn Run Picture) -> IO ()
```

The `gloss`-level state of our application will be the reference to the dynamic value.
In the rendering on the screen we will just query the current value:

```haskell
runGloss pictureDyn = do
  env <- initEnv
  ref <- runReaderT (runDyn pictureDyn) env
  -- world == DynRef Run Picture
  let draw ref = runReaderT (readDyn ref) env
```

In the rendering function we will initialise the environment and inside
update of the events we will just dump all the events to the channel that is stored in the
environment:

```haskell
  let update event ref = do
    atomically $ writeTChan (unEnv env) event
    pure ref

  let frameUpdate _ ref = pure ref
```

With those functions we are ready to run the imperative gloss rendering function:

```haskell
  playIO draw update frameUpdate
```

That's all we need to do. To provide the user with nice event stream of events
we access the environment by the reader monad and wrap it to the event:

```haskell
getEvents :: Evt Run Event
getEvents = Evt $ \go -> do
  (Env eventChan) <- ask
  runEvt (tchanEvt eventChan) go
```

To make user experience nicer in the real library we also created wrappers
that settle down the generic monad parameter for event streams and dynamic values:

```haskell
newtype Evt a = Evt (Dyna.Evt Run a)
newtype Dyn a = Dyn (Dyna.Dyn Run a)
```

So the signature of the previous example looks more easy to understand:

```haskell
getEvents :: Evt Event
```

## Applications

The same technique of keeping communication with imperative event loop
over concurrent channels works for many other libraries. We have created
bindings to animation libraries `gloss` and `processing-for-haskell`,
also bindings to TUI library `brick` and for server side web-library `scotty`.

### Animations

Creation of animations for the first impulse to creation of FRP approach.
And indeed ti fits nicely to that domain. Let's show the hello world program
for `gloss` bindings to see how those ideas can be used in practice.

In the program we have a solid circle that is drawn in the mouse pointer:

```haskell
main :: IO ()
main = runApp defSpec $ pure pic

pic :: Dyn Picture
pic = (\pos -> translate pos $ color green $ circleSolid 50) <$> mouse
```

Note how we use the `Functor` instance to transform the dynamic value of
current `mouse` pointer position to the pictures of circles.
And it produces the picture as dynamic value.

### TUIs

We can create terminal user interfaces with `brick` library and
to find out how FRP will work in that setting we have created a binding to it
which is called `dyna-brick`. We have implemented a 15 puzzle game 
and 2048 games to test how it works. And it was great experience.
The game becomes a scan over user moves which for both games
are just the arrow moves or `Esc` or `q` to quit and `r` to restart the game.

To give a taste of implementation we can show the most interesting FRP parts
of it. Here is a 15 puzzle

```haskell
show me what you ve got

```

### Web servers


## Comparison to other FRP libraries

There are many FRP libraries in the Haskell ecosystem. 
There is a league of libraries that implement classic FRP like `reactive-banana`, `reflex` or
`frp-now` and also there is big family of libraries that implement Arrow-style FRP,
most notorious are `Yampa` and `Dunai`. As we see it the place of the `dyna` library among
those is for educational reasons, because model is very simple to grasp and also 
it's imperative nature makes it a good fit to create bindings to imperative libraries.
Other nice properties is due to update in place we don't have holding to the past problem and
space leaks that are connected with that another interesting property is that `dyna`
library is based on concurrency from the ground up. 
It would be interesting to see how the concurrency strain of it will evolve
and if it become more prominent and affect the design choices for the applications
written in it.

## Further work

We plan to implement more bindings. It would be interesting to create bindings
to `wxWidgets` library to create desktop applications. As we already tested out the TUIs
with brick. It was easy to implement and start with it is interesting to see how this
approach will develop with Desktop applications which often asks for recursive definitions
of the widgets.

Another interesting direction is to try out the concept for the dynamic DOM applications.
The callback-style implementation requires from the host language to have mutable variables
and being able to create concurrent processes. We are interested to implement 
the library in `purescript` as it offers very lean and great to read `javascript` output
comparing to `ghcjs` compiler. We would like to try out our ideas with `purescript`
and see how far we can get with that as `Javascript` offers concurrent workers
and there are works that make this feature available to `purescript`.

## Conclusion

In this paper we have discussed a novel and elegant approach to FRP implementation.
In this approach instead of hiding away the imperative logic from the user we use
it as a cornerstone of our library and expose internals. We have shown how to implement
the combinators for classical FRP and that it can be done in elegant and generic 
manner with the help of powerful concurrency model of the Haskell language. 
Being able to use generic concurrency routines from the libraries `lifted-base`
and `async-lifted` is a cornerstone of our implementation and what makes it 
easy to implement and extend. 

We hope that this will inspire the FRP users to experiment with this approach
for teaching and for real applications!

