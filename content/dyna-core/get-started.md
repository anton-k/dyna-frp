---
title: Get started
---

The `dyna` is functional reactive programming (FRP) library for Haskell. 
It implements classical FRP and provides easy to use interface to 
write call-back heavy interactive applications. It takes an imperative 
definition for event streams as a basis and 
granted with Haskell power for abstractions builds elegant DSL on top of it.

## Installation

The library can be installed with `cabal` from hackage

```
> cabal install dyna --lib 
```

Also it can be used with stack. We just need to list it in the `extra-deps`
of the `stack.yaml`.

We recommend to start with tutorial on core concepts of the library
and then move on to the domain of your interests. With `dyna`
we can create animations (see [dyna-gloss](/dyna-gloss/tutorial-toc) 
or [dyna-processing](/dyna-processing/tutorial-toc)), terminal user interfaces
with [dyna-brick](/dyna-brick/tutorial-toc).

## The Paper

The paper [Callback Heaven for FRP](/paper) offers a good outline of the whole system and concepts behind 
the `dyna` library.

## Dyna core FRP tutorial

* [Introduction](/dyna-core/tutorial/01-intro)
* [Event streams](/dyna-core/tutorial/02-event-streams)
* [Game example](/dyna-core/tutorial/03-event-game-example)
* [Dynamic values](/dyna-core/tutorial/04-dynamics) 
* [Main FRP monad](/dyna-core/tutorial/05-backbone-monad)
* [Control flow. Recursion and sharing](/dyna-core/tutorial/06-control-flow)
* [How to make bindings](/dyna-core/tutorial/07-make-bindings)
* [Parser for event streams](/dyna-core/tutorial/08-parser)
* [Conclusion](/dyna-core/tutorial/09-conclusion)
* [Quick Reference](/dyna-core/tutorial/101-reference)
* [Resources](/dyna-core/tutorial/102-resources)

## Resources

* [Hackage docs](https://hackage.haskell.org/package/dyna) - docs for `dyna` library
* [Conal Elliott FRP publications](http://conal.net/papers/frp.html) - great papers from one of the inventors of FRP concept
* [Ivan Perez FRP publications](https://github.com/ivanperez-keera/dunai#reading) - great papers explainig the FRP concept and arrow style FRP
* [Haskell FRP zoo](https://github.com/gelisam/frp-zoo) - lists lots of examples in various FRP libraries

