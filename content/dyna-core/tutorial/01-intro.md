---
title: Introduction
---

FRP allows us to implement interactive applications with nice and 
declarative style. We can push IO-interaction to the boundaries of 
the application. And for some applications it even seems that
all functions are pure and no dirty operations happen.

There are many implementations of the FRP in Haskell. Why
do we need yet another one? The main focus of the `dyna` library is simplicity
of implementation. Once you grasp the core data types and concepts it is easy to
derive the implementation.

The main Zen assumptions: 

* There are continuous signals (`Dyn` short for dynamic) and event 
    streams (`Evt` short for events)
* Everything happens just now for the processes (`Dyn`)
* We don't know when event (`Evt`) is going to happen, but when it 
  will happen we can call a callback
* Everything happens concurrently
* Every process is an observation of some event stream

In the following section we describe the basics of FRP. 
Feel free to skip it if you know the concept and move
to the tutorial for the `dyna` library.

## Introduction to FRP

There is much debate about what functional reactive programming (FRP) 
actually is and what it's not. Some claim that it's essential to use continuous time
others stress on Map/Reduce-like functional approach to build applications.

FRP vs state machine/callback approach reminds me of the problem of particles
from the Physics. When we go deeper to the roots of the basic elements of the matter
it turns out that the matter is mysterious object. 

It can be viewed as a particle, non-divisible thing, and also it can be viewed
as a wave, continuous transformation of the matter that interacts 
with other waves. This is called 
[Wave–particle duality](https://en.wikipedia.org/wiki/Wave%E2%80%93particle_duality) 
concept.

I'd like to think of Finite state machine (FSM) approach when we describe
interactive system as transformation of the current state as it reacts
to events as a particle view of the UI system.

The only thing we are aware of is our current state and we run
the **event loop** to query user for events. And when something happen we update
the state. The current state is sort of indivisible object that is transformed.

The FRP approach is to think about units of transformations as a whole. 
We don't work with individual events or states but we work with signals or waves
of events or continuous transformation of the elements from which the application
is constructed. 

So instead of a single Event like right mouse button was pressed we
have an event stream of all possible button presses. And we establish relationship
between those streams and properties of our system. From that stems the interactivity. 

Let's describe a simple application with two models to feel the difference. 
Imagine that we have a circle that is drawn in the mouse position and when user
clicks on the mouse button the circle changes the color.

How we can solve that with our particle system view. 

#### FSM approach

Let's look at finite state machine approach to the task.
We have the state:

```haskell
data St = St
  { st'position :: (Float, Float)   
     -- ^ center of the circle (x, y) coordinates
  , st'color    :: Color
     -- ^ color of the circle
  }
```

If position changes or mouse button was pressed we update the state:

```haskell
eventLoop :: Event -> St -> St
eventLoop event st = 
  case event of
    MousePosition x y -> st { st'position = (x, y) }  
    MouseRightButton  -> st { st'color    = nextColor (st'color st) }
    _                 -> st
```

Where `nextColor` is some function to update the color. 


And we have some `draw` or `view` function that draws the state on the screen:

```haskell
draw :: St -> Picture
draw (St pos color) = setColor col (circleFill pos)
```

This is function in some imaginary graphics framework. So we 
can draw a circle in the state position with given color. 

#### FRP apporach

For functional reactive programming we have event stream of all
mouse button presses and continuous signal (or wave) of mouse positions.
And we use typical functional programming tools like (`map`, `filter`, `fold`)
to work with streams and waves as if they are lists.

To me this is the essence of FRP. Reusing the go-to functional programming
tools to work with events as if they are infinite streams and establish 
relationships with parts of the application making it dynamic. 

Let's imagine that we have library functions:

```haskell
mousePos         :: Signal (Float, Float)
mouseButtonClick :: Stream ()
```

Mouse position is a signal of all positions that user brings
the mouse into during the life of the application.
Mouse button clicks contains all possible events of the type mouse click.
When user clicks on the mouse we put an event to that stream.

To draw the picture we use `Functor` instance to map all mouse positions
to circles:

```haskell
circlePicture :: Signal Picture
circlePicture = fmap (\pos -> circleFill pos) mousePos
```

So with `fmap` we set up a relationship between the picture on the screen
and current mouse position. Also we can update the colors:

```haskell
colors :: Stream Color
colors = iterateE green nextColor $ mouseButtonClick

coloredPicture :: Color Picture
coloredPicture = liftA2 setColor (hold green colors) circlePicture
```

We use function `iterates` to update some value with a function
whenever anything happens on the stream. By convention if function
is defined on event type and clashes with some function from Prelude
we append suffix `s` to the end of the verb (i.e. `drops`, `takes`).

```haskell
iterates :: a -> (a -> a) -> Stream b -> Stream a
```

We use function `hold` to turn streams of events to waves.
It just remembers the last value that has happened on stream and
keeps producing it until something new will happen:

```haskell
hold :: a -> Stream a -> Signal a
```

To set up the color we use Applicative instance for signals:

```haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
```

So the `liftA2` takes our pure function to set color of the picture
`setColor :: Color -> Picture -> Picture` and adapts it to work on signals
of values.

And we render the whole app with some root evaluation function
that expects the signal of pictures:

```haskell
runApp :: Signa Picture -> IO ()
```

So for me the essence of FRP is to treat user interactions
as streams of events and dynamic changes and work with those values
as a whole. We establish connections between dynamic values with
standard functional programming interfaces. 

## FRP interface

FRP often has two types of values:

* Behaviors - continuous signals (it was `Signal` in our example )
* Event streams - infinite streams of values that happen over time

Conceptually we can think of Behaviors as functions from time to value
and about event stream as infinite stream of values tagged with the time of happening:

```haskell
type Behavior a = Time -> a

type Events a = [(Time, a)]
```

For those values in FRP we define standard interfaces.

### Behaviors

We can map over behaviors with functions of single or multiple arities:

```haskell
instance Functor Behavior where
instance Applicative Behavior where
```

The `fmap` method of `Functor` let's us to lift single argument function
to domain of Behaviors:

{{< figure src="/images/fmap-behavior.svg" alt="Fmap Behavior" width="90%" >}}

```haskell
fmap :: (a -> b) -> Behavior a -> Behavior b
```

The applicative instance let us create constant behaviors:

```haskell
pure :: a -> Behavior a
```

And lift pure functions with arbitrary number of arguments to Behaviors:

```haskell
liftA2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
```

This is an example for function of two arguments, but we can achieve that
for arbitrary number of arguments with combination of two operators:

```haskell
f <$> a <*> b <*>  ... <*> z
```

If we have Applicative instance we can derive many useful interfaces for free.
We can take some specific class an lift all it's methods to `Behaviors`.
Take for example monoid:

```haskell
instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)
```

The same trick can be done with `Num`, `IsString`, `Fractional` and many other standard classes.

### Event streams

The event streams should support interface:

```haskell
instance Functor Events where
instance Monoid (Events a) where
```

With functor we just map over values of the events and keep the time
of happening the same. 

{{< figure src="/images/fmap-events.svg" alt="Fmap Event" width="70%" >}}

With `mappend` we merge two lists of event streams together
into single event stream. 

{{< figure src="/images/monoid-events.svg" alt="Monoid events" width="80%" >}}

That's it! Everything we need for the events.
Usually libraries support many functions that are typically associated with lists.
Like `filter`, `take`, `drop`, `fold`, `scan`, `cycle`, `iterate`, `takeWhile` etc.
Because it's very convenient to think of event streams as of infinite lists of events.

### Interaction between Behaviors and Event streams

FRP library is expected to implement functions that let us convert
streams to behaviors and back. 

We have function

{{< figure src="/images/stepper.svg" alt="Stepper" width="80%" >}}

```haskell
stepper :: a -> Events a -> Behavior a
stepper initial events
```

It makes piecewise-constant function out of event stream. 
It starts with constant initial value and produces it until some event
happens on the stream. Then that initial value is substituted with 
new taken value from the stream and get's produced until the next event will 
arrive. 


Often we define the dual function:

```haskell
changes :: Behavior a -> Events a
```

It triggers new event when value on input behavior changes. 
Also some libraries implement more generic `stepper` function:

{{< figure src="/images/switch-behavior.svg" alt="Stepper" width="80%" >}}

```haskell
switch :: Behavior a -> Events (Behavior a) -> Behavior a
```

It starts with initial behavior and when event happens it
carries the next behavior to switch to. 

Also often we have the same function for event streams:

{{< figure src="/images/switch-events.svg" alt="Stepper" width="80%" >}}

```haskell
switch :: Events (Events a) -> Events a
```

It produces the event stream from the first event. When the second 
event happens it stops to listen for the events on first event stream
and starts to listen for the events on the new event stream.
This is very powerful abstraction. 

In fact together with event stream that contains a single event
that happens on start of event stream the `switch` forms
a `Monad` instance. For it `switch` can be though of as `join`
for a monad. 

### Recursive event streams

Sometimes it is useful to have event streams with feedback loop.
Imagine a button widget and it displays a text that shows
how many times it was clicked.

This widget can be created with function:

```haskell
newButton :: Behavior Text -> Events ()
```

It takes in the dynamic text that is shown on the button
and produces event stream of clicks. To change the text based
on the event stream we need to introduce feedback loop:

```haskell
clicks = newButton msg 
msg = stepper "Zero clicks" (fmap (\n -> "Clicked " <> show n " times") $ count clicks)
```

So we need to use a recursion. Various libs solve this differently.
Some allow direct recursion to be defined some introduce special 
fix-like functions to express it. In the `dyna` we took later approach.
We have `fix1` function:

```haskell
fix1 :: (Events a -> Events a) -> Events a
```

This is simplified version of it. It takes in a function that expects
an event stream as argument and produce just event stream. 
The meaning of it is that it sends every output event back to the input 
of the event processor function making a loop.

### Conclusion 

This covers the whole repertoire of FRP libraries. FRP is great concept
that allows us to build declarative interactive applications. 
In the next chapters we will see how those concepts are implemented 
and used in the `dyna` library and what makes it special.

----------------------------------------------------------

* `=>` [Event streams](/dyna-core/tutorial/02-event-streams)
* Up: [Table of Contents](/dyna-core/tutorial-toc)

