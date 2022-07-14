---
title: Dynamics - continuous processes
---

We have dedicated a significant amount of time to event streams as they are the 
corner stone of our FRP library. Often in FRP we find another key element
called `Behavior`. It's often conceptually represented as a function
from time to value:

```haskell
type Behavoir a = Time -> a
```

This God like vision of the whole function would be hard to implement in practice. 
And in Classical FRP approach Behaviors are often represented with event streams 
as so called stepper functions:

```haskell
data Behavior a = Stepper a (Event (Behavior a))
```

{{< figure src="/images/stepper.svg" alt="Stepper" width="90%" >}}

So we have some initial value as a first argument of the stepper.
And the second argument is an event stream of future behaviors.
Whenever event happens on that stream it brings a new behavior in the scope.
And the next event updates that and so forth. 

For the `dyna` library we took a similar approach. The continuous signal
is an observation of an event stream. Imagine that we have some event stream.
For example the stream of user inputs as we saw it in the previous section. 
What would be a continuous signal for that stream? At initial time we don't have
any input and let's assume that we have some default value. For example an empty string.
If user does not provide the input we continue to use that empty string
as observation. But here comes the user, types "Hasta la vista"
and disappears. From now on we use that new value as current value of the process.
And whenever somebody asks as which value is now we can reply "Hasta la vista".
Until user will come back and type something next.

For our library we call continuous observations of the streams `Dynamics`.
The name is inspired by the convention in the cool another FRP library `reflex`.
Only as we like short names it's shortened to `Dyn`. And this gives the name to the whole
library. Just in case you are curious who that Dyna is.

Let's study the type:

```haskell
data Dyn m a
   = forall s . Dyn
       { dyn'get     :: s -> m a   -- ^ get the value from internal state
       , dyn'evt     :: Evt m s    -- ^ stream of state updates
       , dyn'init    :: m s        -- ^ initial state
       , dyn'release :: m ()       -- ^ release resources for dynamic
       }
     -- ^ event based dynamic
   | ConstDyn a  
```

It seems to be more involved and complicated than the event stream type.
But basically what it says is that we have two cases.
One is for optimisation:

```haskell
   | ConstDyn a  
```

The `ConstDyn` is a constant dynamic. It always equals to the same value. 
Another case is more interesting:

```haskell
   = forall s . Dyn
       { dyn'get     :: s -> m a   -- ^ get the value from internal state
       , dyn'evt     :: Evt m s    -- ^ stream of state updates
       , dyn'init    :: m s        -- ^ initial state
       , dyn'release :: m ()       -- ^ release resources for dynamic
       }
```

It has four parts:

* Initial value `dyn'init :: m s`. This is our empty string from the example
    until user has not typed anything.

* Also it has underlying event stream that we observe:

  ```haskell
  , dyn'evt     :: Evt m s    -- ^ stream of state updates
  ```

  It's just the event stream as we have defined it before. We use it to update current state.

* Also we need a way to observe the state. We turn the internal state into some
   value that we can **see** with getter function:

  ```haskell
  dyn'get     :: s -> m a   -- ^ get the value from internal state
  ```

  This function makes our implementation very flexible. We hide underlying state
  by `forall` trick and the type contains only value of observable type `a`.

* The last piece of the puzzle is a procedure that we call to release resources that
  are needed for execution of an event stream:

  ```haskell
  , dyn'release :: m ()       -- ^ release resources for dynamic

  ```

  We want to be able to gracefully shut down the event streams when we don't need
  the dynamic values anymore. For many dynamic values this going to be just empty
   `pure ()` procedure. But some event streams need this component to be shut down properly.

So beside the details of implementation we have in dynamics just what we 
declared. We have an initial state, some event stream that updates it and also 
we have a function to **observe** the current state. We could use the return value as a state
but it reduces the flexibility. With hidden state we can combine several dynamics that depend
on states of different types. Which would be impossible if we used the same type in state as in
argument of dynamic value (the type `a`). All is generalized over the monad `m`. But for now we assume
that the runner backbone monad is `IO`.

So as we have dynamics how can we work with them? This turns out to be an interesting
and even philosophical question. How powerful we are? What can we really observe in the jungle
of wild infinite reality that surrounds us?

In the library we assume that a dynamic is an opaque black box processes. That can be started
to run. And once it is started we are given sort of reference. With which we can query the
current state of the process, The process itself runs in the background (in separate green thread).
And all we can do is to ask for current value. 

So we have just three functions:


* Start dynamic process

  ```haskell
  runDyn :: Dyn m a -> m (DynRef m a)
  ```

  We start a process and we are given a reference. We can use two functions with it:

* Read current value

  ```haskell
  readDyn :: DynRef m a -> m a
  ```

* Shut down the dynamic process:

  ```haskell
  cancelDyn :: DynRef m a -> m ()
  ```

This seems to be very imperative and limiting. But hold on!
Soon we will define powerful combinators to create interesting dynamics out of simple
ones. And also we will juggle between event streams and dynamics with ease and fluency.

But I'd like to stress out the scope of our vision with dynamics. 
We can observe only current value, and dynamic is a process that runs in the background (separate thread).
It seems to be imperative but we will soon define functional interface to play with dynamics
and combine them with the same ease as we did it with event streams.

### FRP interface for behaviors

Let's recall what fathers of FRP proposed for behaviors:

```haskell
instance Functor Behavior where 

instance Applicative Behavior where

instance Monoid a ñ Monoid (Behavior a) where 
```

{{< figure src="/images/fmap-dyn.svg" alt="Monoid events" width="85%" >}}

So behavior should implement `Functor`, `Applicative` and `Monoid`.
The implementation is a bit more complicated for dynamics than for event streams.
So I don't list it here, and only outline the ideas for solutions. Interested reader can
look up the implementation in the source code.

The functor is easy to define we just compose the functor function with `dyn'get`.
And we redefine the getter. Underlying state update event stream remains the same
and we sort of *shift our view on it*. 
Applicative is tricky to implement. But concept is very simple.
The `pure` case is obvious. It's just constant dynamic. Here we can use our optimization
case `ConstDyn`. The operator `(<*>)` is a tricky beast. But idea is simple.

Let's remind the signature for it:

```haskell
(<*>) :: f (a -> b) -> f a -> f b
```

So we have dynamic continuous signal of functions `a -> b` and
we have another continuous signal of values of type a and we apply the function to the value.
Conceptually it's very simple. Just apply point wise function to the argument 
at any point of the continuous signal. Only we don't have continuous signals. 

Our dynamics are observations of internal state updates. How can we implement that?
Here is the solution. We start concurrently two underlying event streams that update 
functions `a -> b` and arguments `a`. We keep the pair of underlying states
as compound state. Whenever any of the two streams change the state we update
the whole state (pair of states). And thus we can have point wise update. Whenever
user asks for the current value we just take the pair of underlying states.
We get function from the first element and the argument from the second one
and apply the function to the argument. 

The key element of this implementation is that we concurrently run two event streams
and compound state is a pair of underlying internal states. Whenever any of the two
streams change it's part of the state we update the whole pair. That's simple idea
was implemented and it gives us an Applicative instance.

Once we have applicative it's easy to define the monoid. It follows from the Applicative *for free*.
Because our monoid is actually point wise lifting of monoid methods to the Dynamic values:

```haskell
instance Monoid a => Monoid (Dyn m a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (Dyn m a) where
  (<>) a b = (<>) <$> a <*> b
```

That's it. 

Now we know how to apply functions to dynamics. But the most powerful functions
are for interaction with event streams. Here is the interface:

```haskell
-- create dynamic from the event stream
hold :: a -> Evt m a -> Dyn m a
```

{{< figure src="/images/hold.svg" alt="Monoid events" width="85%" >}}

The `hold` creates dynamic from the event stream. Actually it's convenience 
wrapper around the constructor. It wraps the event stream to the internal 
state update stream and getter is just a `pure` identity function.

There is a dual function that unwraps underlying event stream for dynamic value:

```haskell
unhold :: Dyn m a -> Evt m a
```

With next cool function we can sample dynamic with an event stream:

```haskell
snap :: Dyn m a -> Evt m b -> Evt m a
```

So what happens? We have some dynamic process and we want to query it's values
with event stream. Let's consider an example. 
Whenever user types anything as input we show the current time to the user:

```haskell
> import Dyna
> import Data.Time
> t0 <- getCurrentTime
> time = hold t (clock 1)
-- show the time
> prints $ snap time $ forevers (once getLine)
```

So first we create dynamic with `hold` that contains current time:

```haskell
> t0 <- getCurrentTime
> time = hold t (clock 1)
```

Next we sample it with user inputs and print for the user to see it:

```haskell
> prints $ snap time $ forevers (once getLine)
```

There are cool variations on the same theme. Let's list those functions:

```haskell
-- sample with a function
apply :: Dyn m (a -> b) -> Evt m a -> Evt m b

-- infix variant of apply
(<@>) = apply 

-- sample and keep the original value
attach :: Dyn m a -> Evt m b -> Evt m (a, b)

-- sample and apply binary function:
attachWith :: (a -> b -> c) -> Dyn m a -> Evt m b -> Evt m c
```

Also we have variants that filter as they apply the functions. By convention
they have the suffix `May`:

```haskell
applyMay :: Dyn m (a -> Maybe b) -> Evt m a -> Evt m b

attachWithMay :: (a -> b -> Maybe c) -> Dyn m a -> Evt m b -> Evt m c
```

It skips the events is function returns `Nothing`.

### Effectful constant dynamics

With `pure` method of applicative class we can lift to dynamics 
constant values. But imagine an event stream that represents some effectful
operation. For example query current time or current index on the Forex exchange
or weather forecast API. Just for that we have the function:


```haskell
constDyn :: m a -> Dyn m a
```

It takes some effectful dirty getter and turns it to dynamic.
Let's work with dynamics on low level and clarify how it works.
We can query the current time with it:

```haskell
> import Data.Time
> time = constDyn getCurrentTime

-- run the dynamic porcess and get read-only reference
> ref <- runDyn time

-- read the current value
> readDyn ref
2022-02-07 14:37:00.899747188 UTC

-- read again later
> readDyn ref
2022-02-07 14:37:03.499596086 UTC
```

------------------------------------------------------------------------------

* `<=` [Interactive event game](/dyna-core/tutorial/03-event-game-example)
* `=>` [Generic FRP monad](/dyna-core/tutorial/05-backbone-monad)
* Up: [Table of contents](/dyna-core/tutorial-toc)
