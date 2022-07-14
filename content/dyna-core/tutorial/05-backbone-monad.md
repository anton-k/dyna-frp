---
title: Generic FRP monad
---

Can you recall how we said that event streams and dynamics are general 
in terms of the backbone monad but we assume it to be `IO`. Now time has come
to find what monad can we use here. By default in the interpreter session 
it will be specialized to `IO`. But it's useful to know that it can be more generic.

It turns out that to be able to use a monad in place of `IO` we need to
have following properties:

* we should be able to run concurrent processes with it (instance of `MonadBaseControl`)

* we should be able to run IO-procedures (instance of `MonadIO`)

* we should be able to use mutable variables with it (`IORef` or `TVar` are defined as presets)

It's all encapsulated in the type class `Frp`:

```haskell
class class (IsRef (Ref m), MonadBaseControl IO m, MonadIO m) => Frp m where
  type Ref :: (* -> *) -> * -> *
  type family Ref m 
```

The `MonadBaseControl` is a scary beast that let's us to use concurrency features
on generic monads not only on `IO`. It looks complicated but there are standard ways
to easily derive the instances. 

The `MonadIO` is a class that let us execute IO inside our monad:

```haskell
clas MonadIO m where
  liftIO :: IO a -> m a
```

The `Ref` is an interesting thing. It's a special type tag that
let us choose between two predefined defaults for storing internal state
update values. We can choose `IORef` or `TVar`. The `IORef` is better to use for 
fast interactive applications like OpenGL graphics game engines. `TVar` can be
more suitable for slow apps like GUIs or TUIs. 

So in many places for simplicity I've omitted the `Frp m` constraint
but it's ubiquitous in the library. For example:

```haskell
apply :: Frp m => Dyn m (a -> b) -> Evt m a -> Evt m b
```

So the `m` is constrained by `Frp` class.
As a user of the FRP libraries you may encounter a specific monad
in the bindings. Often it's called `Run` monad and it contains magic global variables
that are needed to run the interactive application.  

How to create our own instances of `Frp` class we will know soon from the chapter

That is dedicated to creation of bindings to other libraries.

------------------------------------------------------------------------------

* `<=` [Dynamics - continuous processes](/dyna-core/tutorial/04-dynamics)
* `=>` [Control flow. Recursion and Sharing](/dyna-core/tutorial/06-control-flow)
* Up: [Table of contents](/dyna-core/tutorial-toc)
