## Event streams

The core type of the library is event stream `Evt`. 
The main idea of the library is to use very simple imperative representation of the 
event streams and use convenient combinators to build compound
event streams out of simple ones.

Often in FRP research papers event stream conceptually is represented
as list of some events that happen at certain time stamps:

```haskell
type Events event = [(Time, event)]
```

The list is infinite and we can receive events as they come. 
Instead of that we can think not about what event stream is but
why do we need the event stream in the first place? How is it related to 
the rest of the application? 

In imperative approach we use event streams as source of call back invocations.
**Mr Run Event** says: You can give me a procedure `a -> IO ()` and when event will happen I
gonna call it for you. That's nice to have! Thanks **Mr Run Event**. 

So we define not what event is but what we are going to do with it.
The problem with this approach is that it often leads to a very clumsy code.
It's even called call-back hell. 

The main idea of the `dyna` library is to take this hellish approach and turn it 
into heaven by offering nice interface to combine the callback processors.

The event stream is just a callback processor:

```haskell
newtype Evt m a = Evt {
  runEvt :: (a -> m ()) -> m ()
}
```

It's a real definition from the library (not a simplified one).
So the event stream is that Mr **Run Event** that takes 
our procedure `a -> m ()` and  does something useful with it on our behalf, i.e.
run is as procedure and result is `m ()`. Here `m` is some monad. Let's
for simplicity assume that it's `IO`. We are going to make our tiny version of the
library to understand the main concepts. We are going to work with this simplified
version for now:

```haskell
newtype Evt a = Evt {
  runEvt :: (a -> IO ()) -> IO ()
}
```

### Simple event streams

Let's look at some examples of event streams. 
The most simple one is super lazy and arrogant event stream. It just ignores
the callback and returns:

```haskell
never :: Evt a
never = Evt $ \_ -> pure ()
```

It emulates the empty event stream with events that never happen. 
We can even call it in the interpreter:

```
> ghci
> newtype Evt a = Evt { runEvt :: (a -> IO ()) -> IO () }
>
> never = Evt $ \_ -> pure ()
> runEvt never putStrLn 
```

So it did nothing. Let's define event stream that does
something only `once` and does it right away:

```haskell
once :: IO a -> Evt a
once getter = Evt $ \go -> go =<< getter
```

So we pass a IO-getter function that reads some value and 
on running that event stream we just use that function to get the value
and apply the callback to it.

Let's copy that definition to repl:

```haskell
> once getter = Evt $ \go -> go =<< getter
> :t once
once :: IO a -> Evt a
```

With it we can do something useful. For example we can query user for a
number and show the twice amount of that number. First we define helper function
that asks user for single input:

```haskell
> getLineE = once getLine
```

Let's define the doubler callback and call it with a user input:

```haskell
> doubler str = putStrLn $ "Answer: " <> show (read str * 2)
> :t doubler
doubler :: String -> IO ()
> runEvt getLineE doubler
4                             -- our input
Answer: 8                     -- Mr Run Event produces
Prelude>
```

With this function we asked only for one input. But also we can create
a process that can double input `forever`. Let's define a helper function:

```haskell
 > import Control.Monad

-- foreverE :: Evt a -> Evt a
 > foreverE evt = Evt $ \go -> forever $ runEvt evt go
```

We use standard function `forever` from the module `Control.Monad`
to call procedure in the infinite loop. 
The function `foreverE` takes in an event and calls it all the time in an infinite loop.
With what we have already defined we can use it to create doubler service:

```haskell
> runEvt (foreverE getLineE) doubler
4
Answer: 8
100
Answer: 200
43
Answer: 86
0
Answer: 0
2
Answer: 4
```

That was neat! We can define some useful call-back building abstractions
right in the ghci session. All those functions `getLineE`, `foreverE`, `once`, `never`
are already defined in `dyna`. We just look at the implementation to get familiar 
with the concepts.

I hope that by those examples we can understand the concept behind the event stream.
It's just a callback consumer. It get's a callback and does something useful with it
whenever an event happens.

In the `dyna` we have cool event stream of time stamps that produce the current clock
or passed time:

```haskell
> import Dyna
> runEvt (clock 1) print
```

The `clock` takes in a number of seconds in which to periodically sample the current time.
Try also functions `timer`, `ticks` and `pulse` instead of `clock`.

### Printing the events

Also we can define a useful function to show the events (also standard function):

```
printE evt = runEvt evt print
putStrLnE evt = runEvt evt putStrLn
```

We take an event and pass a printing function to it as a callback.

### Operators for event streams

The most fun things start to happen when we take some tiny basic event streams
and start to build more complicated ones out of them. The Haskell power starts to shine.

We already did that with function `foreverE` as it's a stream processor. It takes one stream 
and turns it to stream of forever loop. Let's discuss other useful operations.

#### Analogy with a List

Many operations are easy to understand if we think about event stream as an infinite
list of events. Later on we will borrow many list functions and redefine them for event streams.
Only for our implementation we just trigger some callback whenever element is added to the list.
But ofcourse there is no list whatesoever. It's just helpful analogy.

#### Functor 

Let's recall the doubler function:

```haskell
> doubler str = putStrLn $ "Answer: " <> show (read str * 2)
```

It does 3 things. It:

* parses integer from string input

* doulbes the input as integer

* turns it to the output string with nice prefix

If we had the list of strings as input we could apply the doubler like this:

```haskell
> toAnswer x = "Answer: " <> show x
> fmap (toAnswer . (2 *) . read) inputs
```

Here we use standard function `fmap` from the functor typeclass:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

For the lists it takes in a function and applies it to every element in the list.
For the list that conceptually contains all possible events it is cool to have opration like `fmap`.
and it can easily be defined:

```haskell
instance Functor Evt where
  fmap f evt = Evt $ \go -> runEvt evt (go . f)
```

So we take the callback for the new input of type `b` and to use the 
event stream defined on `a`'s we use function `f` to adjust the input.
We can save that instance to file with our defenitions and use it. 
Let's create the module `Evt.hs` and save there all definitions from the current session.
After we load it we can try out our `Functor` instance:


```haskell
printE $ fmap reverse (foreverE $ once getLine)
Hi!
"!iH"
Bill
"lliB"
Bob
"boB"
hit Ctrl+C to exit
```
We have defined a service that reverses every line of the input.
Let's define our dubler function as a composition of smaller parts:


```haskell
> :set -XTypeApplications
> readInts = fmap (read @Int) $ foreverE $ once getLine
> :t readInts
readInts :: Evt IO Int
> toAnswer x = "Answer: " <> show x
> putStrLnE $ fmap (toAnswer . (2 * )) readInts
3
Answer: 6
7
Answer: 14
23
Answer: 46
7687
Answer: 15374
Press Ctrl+C to stop
```

Notice how we used `fmap` once to define the input stream of integers
and another one to double the number and show it to the user.

There is another useful function:

```
mapMay :: (a -> Maybe b) -> Evt a -> Evt b
```

It has very simple definition:

```
mapMay f evt = Evt $ \go -> runEvt evt (mapM_ go . f)
```

It skips all the events that return `Nothing` and puts to output stream all
events that return `Just`. It is mapping combined with filtering. 
For example if user writes non-integer input program will just break up with exception.
But we can do better with `mapMay`:

```haskell
> import Text.Read
> readInts = mapMay (readMaybe @Int) $ foreverE $ once getLine
```

This definition is more solid. Because it skips all non integers.
We can try it out with doubler and see how it skips invalid input.

One very often used case of functor is combo with const. When we want 
to substitute all the events with the constant:

```haskell
printE (100 <$ readInts)
```

#### Monoid

Another useful class to have for events is Monoid (and Semigroup).
For a thing to be a Monoid it have to support `mappend` operation which is associative
and have neutral element `mempty`. 

The meaning of monoidal append for two events is to trigger callback whenever
anything happens on both of the events. The neutral element we have seen already. It's `never` stream.
So if we combine it with any another stream it will be equivalent by behavior to the original 
stream. Which is exactly what we expect from monoidal neutral element. 

Let's define the Monoid:

```haskell
instance Monoid Evt where
  mempty = never
```

To define the append we are going to use function `concurrently_` from the library `async`.
It takes in two procedures and executes them concurrently (or **at the same time**).

```haskell
instance Semigroup Evt where
  as <> bs = Evt $ \go -> concurrently_ (runEvt as go) (runEvt bs go)
```

That's it! So in the result we take single callback and execute it on 
both of the event streams concurrently.
The nice property of the `concurrently_` is that if we force stop of execution
by exception it will stop both of the event processes and we won't have any 
leakage of the resources with unwanted background processes.

Monoid is useful to aggregate several event streams to a single one.

#### More list-like functions

There are plenty of list-like functions in the `dyna` library.
They do just what we expect form lists but lifted to the event streams.
Usually they have the same name but with suffix `E`. 

Let's load the `dyna` lib to interpreter and see some of most useful functions.
Let's start with simple ones: `cycleE` 

```haskell
> import Dyna
> printE $ cycleE [1,2,3,4] (ticks 1)
```

Also we can do filtering with `filterE`:

```haskell
filterE :: (a -> Bool) -> Evt m a -> Evt m a

> printE $ filterE odd $ cycleE [1,2,3,4] (ticks 1)
```

Notice that the event happens once per two seconds because
we skip even numbers. We can sum and product with `sumE` and `productE`.
Also we can count the number of events on the stream `count`. 
We can count how many times user provided the input:

```haskell
printE $ count $ foreverE (once getLine)
```

Sometimes it's also useful to keep the original value. We can 
use `withCount` for that:

```haskell
printE $ withCount $ foreverE (once getLine)
```

The `sumE` and `productE` can be generalized with single function `appendE`.
It appends all the events that are instance of some monoid. 

```haskell
appendE :: (Monoid a) => Evt m a -> Evt m a
```

Ok, we have seen the simple functions. One of the most useful function is `scanE`.
It iterates over elements of the stream and updates the state on every new input:

```haskell
scanE :: (a -> b -> b) -> b -> Evt m a -> Evt m b
```

For example we can redefine the function `count` with `scanE`:

```haskell
> printE $ scanE (+) 0 (1 <$ ticks 1)
```

We used combination of functor and scan. Can you find out how to define it without a functor
and use only scan?

Function that is close to scan is `iterateE`. It ignores the events on the stream
and just updates the state:

```haskell
iterateE :: (a -> a) -> a -> Evt m b -> Evt m a
```

Just as count it has special version that keeps the elements of the stream
alongside with updated state.  It's called `withIterateE`.

Also there are familiar list functions `takeE`, `dropE`, `takeWhileE`, `dropWhileE`.
They do the same stuff as the corresponding list functions.

For many functions that perform map or update of the state with accumulator
we have `withXxx` variants that keep the original value or also we `xxxMay`
variants that can filter and map at the same time. 

Also there are effectful variants that map or filter with dirty effectful functions.
They have the same name but end up with tick at the end. 
For example we can filter with effectful predicate:

```
filterE' :: (a -> m Bool) -> Evt m a -> Evt m a
```

#### Random event streams

When we implement a game we need some source of surprise. Something
unexpected happens and we are happy to deal with that. 
For those cases it's great to use random generators. In the library
we have handful of functions for that. We can toss a coin with function `oneOf`:

```haskell
> data Coin = Heads | Tails  deriving (Show, Eq)
>
> printE (oneOf [Heads, Tails] (ticks 1))
Heads
Heads
Heads
Tails
Heads
Tails
Tails
Tails
Heads
```

The `oneOf` selects one element at random when event on the stream happens. 
The variant `withOneOf` keeps also the original event value alongside with random value.
Also we can have stream of random values:

```haskell
toRandom  :: (Random b) => Evt m a -> Evt m b
toRandomR :: (Random b) => (b, b) -> Evt m a -> Evt m b

withRandom :: (Random b) => Evt m a -> Evt m (b, a)
withRandomR :: (Random b) => (b, b) -> Evt m a -> Evt m (b, a)
```

Also useful function is `freqOf` it allows us to use time varying probability
of event occurence. 

#### Event streams recap

So far so good! We have covered alot of ground based on event streams.
Let's recap. An event stream is a callback consumer/processor. It has very
simple definition. If you give me callback procedure `a -> m ()` I can 
call it whenever any event happen on the stream. But we don't know when it's 
gonna happen. 

```
newtype Evt m a = Evt {
  runEvt :: (a -> m ()) -> m ()
}
```

by using the power of haskell functions we have built a nice DSL on top of this definition.
We have defined instances for `Functor` and `Monoid` classes. We have defined
lots of list-like functions (for example `scanE`, `takeE`, `interateE`, `dropE`, etc). 
We encountered some simple event streams: `once`, `never`, `clock`, `timer`, `ticks`, `pulse`.
They can generate various basic event streams. 


