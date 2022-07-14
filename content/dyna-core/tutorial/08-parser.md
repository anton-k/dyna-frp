---
title: Parsers for event streams
---

This is an experimental feature.
The parser is an interesting way to accumulate events from the stream.
Imagine that event stream carries a tokens for some parser. 
And we can parse a single value from several successive values.
We expect that our value is compound and we gradually take it bit by bit
from the event stream. 

In Haskell there are nice parser combinators libraries which
create very flexible parsers with instance to `Applicative`.
In `dyna` there is an experimental feature that provides this for event streams.

```haskell
data Parser m a b = ...
```

Parser is some stateful accumulator function that expects
as input events of type `a` and produces events of the type `b`.

The simplest parser is `headP`. It just takes the first element 
of any stream:

```haskell
headP :: Frp m => Parser m a a
```

Also we have `maybeP` parser that skips every element 
that results in `Noting` but if the argument function produces
`Just val` it takes that value and completes:

```haskell
maybeP :: Frp m => (a -> Maybe b) -> Parser m a b
```

### Running the parser

Once we have a parser we can apply it to the event stream in several ways.
The simplest one is to apply the parser to the event stream:

```haskell
runParser :: Frp m => Parser m a b -> Evt m a -> m (Maybe b)
```

It starts to consume events from the stream and parser accumulates
the value until it is done. Let's take a first input from the user:

```haskell
runParser headP (foreverE $ once getLine)
Hello!
Just "Hello!"
```

Parser returns `Just val` if it's succeeded with parsing and `Nothing`
if the parser fails. If it was all that we can do it would
be very involved way to do the stuff.

### Parser combinators

Parsers a great that we can build complex parser out of simple ones
with instance to `Functor`  and `Applicative`. 

The `Functor` is obvious. It just maps over result when it's parsed.
But `Applicative` does more interesting stuff. It behaves like
Haskell parsec combinators. 

Let's take for example:

```haskell
f <*> a
```

So it first applies to event stream the parser `f` and result produces
the function `a -> b`, and after that it applies the parser `a`
to the rest of the event stream and applies the function to the result of the parsing
with the second parser. 

Let's for example take a string and an integer from the user input:

```haskell
> import Text.Read
> runParser ((,) <$> headP <*> maybeP (readMaybe @Int)) (forevers $ once getLine)
Hi
23
Just ("Hi",23)
```

Note that the second parser will skip all invalid input until the number
will be typed by the user.

### Accumulation of values on event streams

We can also use parsers to transform event streams. 
So the input will supply the building blocks for compound value
and parser will produce event to the output when whole value will be constructed.

We have two functions for that:

```haskell
takeP :: Frp m => Parser m a b -> Evt m a -> Evt m b
```

The `takeP` will take only one compound value from the stream.
Once the value is constructed it shut downs the stream.

Also we can produce infinite stream of compound values as they got parsed
with `cycleP`:

```haskell
cycleP :: Frp m => Parser m a b -> Evt m a -> Evt m b
```

Let's check it out in the `ghci`:

```haskell
> data Person = Person { name :: String, age :: Int } deriving (Show)

> parsePerson = liftA2 Person headP (maybeP readMaybe)
> :t parsePerson
parsePerson :: Frp m => Parser m String Person
> prints $ cycleP parsePerson (forevers $ once getLine)
Liza
10
Person {name = "Liza", age = 10}
John
5
Person {name = "John", age = 5}
Bob
Williams         -- skipped by the integer parser
Rick             -- also skipped
4                -- ** done: compound value  can be returned as event 
Person {name = "Bob", age = 4}
```

We have created a type for a `Person` with name and age fields.
We also created a parser that reads required types:

```haskell
> parsePerson = liftA2 Person headP (maybeP readMaybe)
> :t parsePerson
parsePerson :: Frp m => Parser m String Person
```

As the last step we run cyclic parsing of the compound
person value from the input stream and echo-print the results:

```haskell
> prints $ cycleP parsePerson (foreverE $ once getLine)
```

We can find another example of parser application in the 
file `dyna/examples/InputForm.hs` in source code repo.


The parsers are experimental so far. It was cool to try
out the ideas from the parser combinators and apply them 
to the event streams.


------------------------------------------------------------------------------

* `<=` [How to make bindings](/dyna-core/tutorial/07-make-bindings)
* `=>` [Conclusion](/dyna-core/tutorial/09-conclusion)
* Up: [Table of contents](/dyna-core/tutorial-toc)

