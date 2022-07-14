---
title: Quick reference to the core FRP functions
---

There are two core data types:

* `Evt m a` - for event streams
* `Dyn m a` - for dynamic processes

The `Evt` is a callback processor. Giving some callback procedure
it can run it each time some event happens. 

The `Dyn` is an observation of an event. It's stepwise constant continuous
signal of values that starts with some initial value. It produces
that value until the next value happen on the event stream then
it produces that value until the next value will come. 

Both events and dynamics are parametrised by some monad `m` 
which allows us to run IO-actions (instance of `MonadIO`)
and allows us to run things in parallel (instance of `MonadBaseControl`).
Together those instances form a `Frp` class.
In all signatures we omit ubiquitous constraint for instance 
of that class `fun :: Frp m => ...`
Also we omit the parameter from the `Evt` and `Dyn`.

## Functions producing Evt

```haskell
-- basic building blocks:

-- no events
never :: Evt a

-- single event that happens right away
once :: m a -> Evt m a

-- extract event from dynamic
unhold :: Dyn a -> Evt a

-- Transform Evt to Evt using function  
fmap    :: (a       -> b) -> Evt a -> Evt b
mapMay  :: (a -> Maybe b) -> Evt a -> Evt b
filters :: (a -> Bool)    -> Evt a -> Evt a
<$      ::             a  -> Evt _ -> Evt a

-- Transform event to event by sampling the dynamic
whenE         ::                        Dyn Bool -> Evt a -> Evt a
snap          ::                        Dyn a    -> Evt _ -> Evt a
<@            ::                        Dyn a    -> Evt _ -> Evt a 
attach        ::                        Dyn a    -> Evt b -> Evt (a, b)
attachWith    :: (a -> b -> c)       -> Dyn a    -> Evt b -> Evt c
attachWithMay :: (a -> b -> Maybe c) -> Dyn a    -> Evt b -> Evt c
apply         ::                    Dyn (a -> b) -> Evt a -> Evt b
<@>           ::                    Dyn (a -> b) -> Evt a -> Evt b
applyMay      ::              Dyn (a -> Maybe b) -> Evt a -> Evt b

-- Combine multiple Events
<>            :: Evt a -> Evt a -> Evt a
mconcat       :: [Evt a] -> Evt a
switch        :: Evt (Evt a) -> Evt a
switchDyn     :: Dyn (Evt a) -> Evt a

-- list-like functions
scan     :: (a -> b ->       b) -> b -> Evt a -> Evt b 
scanMay  :: (a -> b -> Maybe b) -> b -> Evt a -> Evt b

accum    :: (a -> s ->       (b, s)) -> s -> Evt a -> Evt b 
accumMay :: (a -> s -> Maybe (b, s)) -> s -> Evt a -> Evt b 

iterates     :: (a -> a) -> a -> Evt _ -> Evt a 
withIterates :: (a -> a) -> a -> Evt b -> Evt (a, b)

sums, products :: (Num a) => Evt a -> Evt a
count          :: Evt a -> Evt Int
withCount      :: Evt a -> Evt (Int, a)
appends        :: (Monoid a) => Evt a -> Evt a 
foldMaps       :: (Monoid b) => (a -> b) -> Evt a -> Evt b

takes, drops :: Int -> Evt a -> Evt a
takesWhile, dropsWhile :: (a -> Bool) -> Evt a -> Evt a

cycles :: [a] -> Evt _   -> Evt a 
listAt :: [a] -> Evt Int -> Evt a

toToggle :: Evt a -> Evt Bool

filters    :: (a -> Bool) -> Evt a -> Evt a
filterJust :: Evt (Maybe a) -> Evt a 

-- either events
splits :: Evt (Either a b) -> (Evt a, Evt b)
lefts  :: Evt (Either a b) -> Evt a
rights :: Evt (Either a b) -> Evt b

-- recursion for events
fix1 :: (Evt m a -> m (Evt m a)) -> Evt m a
fix2 :: 
     (Evt m a -> Evt m b -> m (Evt m a, Evt m b)) 
  -> (Evt m a, Evt m b)
fix3 :: 
     (Evt m a -> Evt m b -> Evt m c -> m (Evt m a, Evt m b, Evt m c)) 
  -> (Evt m a, Evt m b, Evt m c)
fix4 :: 
     (Evt m a -> Evt m b -> Evt m c -> Evt m d -> m (Evt m a, Evt m b, Evt m c, Evt m d)) 
  -> (Evt m a, Evt m b, Evt m c, Evt m d)

-- share execution of the event
newEvt :: Evt m a -> m (Evt m a)

-- control of execution functions
forevers :: Evt a -> Evt a
forks    :: Evt a -> Evt a
races    :: Evt a -> Evt a -> Evt a 

-- IO input
getLines :: Evt String

-- call procedures pre/post event execution
foreach  :: (a -> m ()) -> Evt a -> Evt a
posteach :: (a -> m ()) -> Evt a -> Evt a

-- Time utilities
clock, timer, ticks, pulse :: NominalDiffTime -> Evt UTCTime

delay     :: NominalDiffTime -> Evt a -> Evt a
delayFork :: NominalDiffTime -> Evt a -> Evt a

-- Random events
toRandom    :: Random a =>           Evt _ -> Evt a
toRandomR   :: Random a => (a, a) -> Evt _ -> Evt a

withRandom  :: Random b =>           Evt a -> Evt (b, a)
withRandomR :: Random b => (a, a) -> Evt a -> Evt (b, a)

oneOf       :: [a] -> Evt b -> Evt a
withOneOf   :: [a] -> Evt b -> Evt (a, b) 

freqOf :: (MonadRandom m) => 
    Dyn m [(a, Rational)] -> Evt m b -> Evt m a

withFreqOf :: MonadRandom m => 
    Dyn m [(a, Rational)] -> Evt m b -> Evt m (a, b)

randSkip   :: Dyn Double          -> Evt a -> Evt a 
randSkipBy :: Dyn m (a -> Double) -> Evt a -> Evt a 
```

### Render event streams

```haskell
runEvt :: Evt m a -> (a -> m ()) -> m ()
heads  :: Evt m a -> m a 
prints :: Show a => Evt m a -> m ()
putStrLns :: Evt m String -> m () 

folds  :: Monoid a => Evt m a -> m a
foldls :: (b -> a -> b) -> b -> Evt m a -> m b
foldrs :: (a -> b -> b) -> b -> Evt m a -> m b 
```

## Functions producing Dyn

```haskell
-- basic functions
constDyn :: m a -> Dyn m a
pure     ::   a -> Dyn a

-- mapping over dynamics
fmap ::     (a -> b) -> Dyn a -> Dyn b
<*>  :: Dyn (a -> b) -> Dyn a -> Dyn b

liftA2 :: (a -> b -> c)      -> Dyn a -> Dyn b -> Dyn c
liftA3 :: (a -> b -> c -> d) -> Dyn a -> Dyn b -> Dyn c -> Dyn d

-- lifted interfaces
-- Dyn is Num, Fractional, IsString, Monoid, VectorSpace, AffineSpace, Boolean, EqB, OrdB
-- if it's argument is from the clas

-- evt/dyn interaction
hold     :: a -> Evt a -> Dyn a
scanD    :: (a -> b ->       b) -> b -> Evt a -> Dyn b
scanMayD :: (a -> b -> Maybe b) -> b -> Evt a -> Dyn b 

switchD  :: Dyn a -> Evt (Dyn a) -> Dyn a

-- time utils
timerD :: NominalDiffTime -> Dyn NominalDiffTime

-- solution of differential equations
sumD :: Num a => NominalDiffTime -> Dyn a -> Dyn a

integrate :: (VectorSpace v, Real (Scalar v), Fractional (Scalar v)) => 
  Scalar v -> Dyn v -> Dyn v 

integrate2 :: (VectorSpace v, Real (Scalar v), Fractional (Scalar v)) => 
  Scalar v -> Dyn v -> Dyn v 
```

### Render dynamic processes

```haskell
-- get reference to the running dynamic process
runDyn    :: Dyn m a -> DynRef m a

-- query current value or cancel the process
readDyn   :: DynRef m a -> m a
cancelDyn :: DynRef m a -> m ()
```

## Sharing events and dynamics

```haskell
newEvt :: Evt m a -> m (Evt m a)
newDyn :: Dyn m a -> m (Dyn m a)

withDyn :: Dyn m a -> (m a -> m b) -> m b 
```

## Effectful dirty API

If you see any transformation with pure function there is 
also function to do it with dirty function. By convention
we use suffix tick `'` to turn it to such a function.
Some examples:

```haskell
filters' :: (a -> m Bool)    -> Evt m a -> Evt m a 
apply'   :: Dyn m (a -> m b) -> Evt m a -> Evt m b 

fmap'    :: (a -> m b) -> Evt m a -> Evt m b 
fmap'    :: (a -> m b) -> Dyn m a -> Dyn m b 
```

## Parsers

Parsers accumulate compound values from event stream. 
It can be though of as simple parsec combinators library for event streams
as input.

```haskell
-- basic parsers
headP  :: Parser m a b
maybeP :: (a -> Maybe b) -> Parser m a b

-- combination of parsers (Functor and Applicative)
fmap :: (a -> b) -> Parser m x a -> Parser m x b
pure :: a        -> Parser m x a
<*>     Parser m x (a -> b) -> Parser m x a -> Parser m x b

-- functions on events:
takeP  :: Parser m a b -> Evt m a -> Evt m b
cycleP :: Parser m a b -> Evt m a -> Evt m b
```

### Render parsers

```haskell
runParser :: Parser m a b -> Evt m a -> m (Maybe b)
```

