---
title: Functions to control the flow of application
---

In this section we discuss some peculiarities of the library. 
How to make recursive definitions with event streams and
how to share effectful event streams and dynamic values.

## Grain of salt. Sharing event streams

If all it was so cool and dandy. But there is grain of salt to it
It is better to be aware of. All event streams perform dirty operations
under the hood. So in some sense they are like `IO ()` and 
this means that we can not share the values by means valid for 
pure values. By using `let`, `where` or in the argument definition 
We will share the **description** of a dirty event stream and not 
actual **execution** or stream of events.

Event stream combinators offer a nice interface to build
complex streams out of simple ones. But keep in mind that
the `Evt` is a function. When we apply combinator we create
a new **dirty** function out of existing ones. So using it by the same 
name will duplicate the side-effects of the function during execution.

Under the hood it performs various non-pure operations and
when we use Haskell's `let` or `where` to reuse event stream
or when we pass it as an argument to the stream the event
stream **execution** is not shared it will be duplicated.

Let's illustrate it by example. Let's create a stream
of random numbers:

```haskell
> a = toRandomR (0, 9) $ clock 1
> prints a
```

Let's create a duplicate event stream and call it `b`:

```haskell
> b = a
```

We might expect that when we will run both of the streams
they will produce the same values. But it's not the case:

```haskell
> b = a
> prints $ snap (liftA2 (,) (hold 0 a) (hold 0 b)) (clock 1)
(0,0)
(9,4)
(5,4)
(5,9)
(0,4)
...
```

The same will happen if we use stream as argument to the function. 
For example:

```haskell
> f x = prints $ snap (liftA2 (,) (hold 0 x) (hold 0 x)) (clock 1)
> f a
(0,0)
(5,7)
(4,4)
(1,6)
(0,6)
(4,9)
```

This can be very counter-intuitive. But what happens is
that we have two instances of the same dirty function
and when it's called it produces side effects that are not shared
even if the name is the same. 

It's like to have the code `(IO a)` even if we pass it by the
same name if it's dirty inside we might get different answers
each time we call it.

How do we share the execution if we need it?
To share the actual execution of streams there is function `newEvt`:

{{< figure src="/images/new-evt.svg" alt="Monoid events" width="95%" >}}

```haskell
newEvt :: Frp m => Evt m a -> m (Evt m a)
```

Under the hood it runs the argument event stream and for
result it creates a channel (form the package `unagi-channels`)
and sends events from the argument that runs in background to the
output channel. This way we can get the same values for both
random arguments:

First let's redefine the `f` so that it takes both event streams:

```haskell
> f x y = prints $ snap (liftA2 (,) (hold 0 x) (hold 0 y)) (clock 1)
```

Let's share the event streams:

```haskell
a = toRandomR (0, 9) (clock 1)
> b <- newEvt a
> c <- newEvt b
> f b c
(0,0)
(1,1)
(0,0)
(3,3)
(9,9)
(8,8)
```

So we see equal numbers for both streams.
Note that this solution also does not work:

```haskell
> b <- newEvt a
> c <- newEvt a
> f b c
```

Because in both `b` and `c` we create background process
with two dirty versions of `a`. And thus we have two replica
of that process. 

But for the right solution:

```haskell
a = toRandomR (0, 9) (clock 1)
> b <- newEvt a
> c <- newEvt b
> f b c
```

We instantiate (run in background) only one random event stream `a`
and we copy events from it to `b`. And in the line
`c <- newEvt b` we copy events from `b` to `c`.
In fact we can save one background process for `b` to `c` and
just reuse the `b` definition since it's sort of a reference to
the original event stream `a`. So this also achieves our goal of sharing
the random events:

```haskell
> b <- newEvt $ toRandomR (0, 9) (clock 1)
> c = b
> f b c
(0,0)
(7,7)
(9,9)
(9,9)
(8,8)
```

This is subtle effect of execution model that we need to be aware of.
For example if you want to share the same clock for all parts of the application.
Just use this at the top of application definition:

```haskell
app = runApp $ do
  sync <- newEvt $ clock 1
  ... use sync in the code ... 
```

Some FRP libraries perform memorization under the hood
or use `unsafePerformIO` to allocate references for such event streams
that lexically look the same. But here we take another approach and 
make it explicit to the user.

This can be inconvenient but this is how it works in the `dyna` library.

### Switching between the event streams

There are king's level functions to structure FRP applications. 
We can flatten the event streams:

{{< figure src="/images/switch-evts.svg" alt="Monoid events" width="90%" >}}

```haskell
switch :: Evt m (Evt m a) -> Evt m a
```

So we have the event stream of event streams and produce a flat event stream out of that.
What should it do? Imagine as it starts nothing happens and the output stream produces nothing.
But something comes on our way and event produces new event stream as a result. We start to
listen to that event stream for events and produce them further to the output stream.
Next on underlying argument the second event happens that brings another event stream into the scope.
We shut down the process with the first event stream and start to listen for the next current event stream.
And we start to channel it's events to the output. And so forth with the third, forth and other events.

The `switch` function is useful to structure the application on bigger events. 
We have events that produce whole event streams and it's sort of switch between several
sources of event streams that are triggered with the stream themselves. 

By the way `once` and `switch` form a Monad instance for `Evt`.


### Switching between the dynamic processes

Can you recall our conceptual definition of behavior from which we have started the section?
It turns out that it can be implemented as library function:

{{< figure src="/images/switch-dyn.svg" alt="Monoid events" width="90%" >}}

```haskell
switchD :: Dyn m a -> Evt m (Dyn m a) -> Dyn m a
```

We can create a compound dynamic with an initial dynamic value
and event stream of dynamics. Whenever new event fires we shutdown
the previous dynamic process and substitute it with current dynamic
that was brought into the scope by the event stream.

We have defined Monoid instance for dynamics with applicative instance.
It turns out that plethora of instances can be generated in the same way.
In the library we have `Num`, `Fractional`, `IsString`, `Boolean` and many others defined in the same way.

### Loops

An advanced usage of event streams is to be able to loop back to the original 
event stream. So we have a function that takes in event stream and as a result it produces
the event stream. But we want to somehow route the events from the result to the input.

We can use `fix1` function:

```haskell
fix1 :: (Evt m a -> m (Evt m a)) -> Evt m a 
```

{{< figure src="/images/fix-evt.svg" alt="Monoid events" width="80%" >}}

This can be very useful in applications with widgets when user input
affects the look and feel of the widget with which user interacts.
Also there are variants for 2, 3 and 4 recursive arguments (`fix2`, `fix3`, `fix4`). 
If you want more recursive arguments it can be easily expressed with `fix1`. See the
docs on Hackage on how to achieve that.


------------------------------------------------------------------------------

* `<=` [Generic FRP monad](/dyna-core/tutorial/05-backbone-monad)
* `=>` [How to make bindings](/dyna-core/tutorial/07-make-bindings)
* Up: [Table of contents](/dyna-core/tutorial-toc)
