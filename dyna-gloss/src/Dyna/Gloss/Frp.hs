-- | FRP main types and combinators
module Dyna.Gloss.Frp(
  -- * Events
  Evt,
  once,
  never,
  -- * Dynamics
  Dyn,
  constDyn,
  -- ** Observing the Dyn
  DynRef,
  runDyn,
  readDyn,
  cancelDyn,

  -- * Control
  newEvt,
  newDyn,

  -- * API
  -- * Event API
  foldE,
  foldMaybeE,
  mapMaybe,
  accumE,
  accumB,
  accumMaybeE,
  filterE,
  filterJust,
  whenE,
  splitE,
  leftE,
  rightE,
  iterateE,
  withIterateE,

  fixE,
  fixE2,
  fixE3,
  fixE4,
  switchE,
  joinE,

  delay,
  delayFork,

  sumE,
  sumD,
  integrate,
  integrate2,
  productE,
  count,
  withCount,
  appendE,
  takeE,
  dropE,
  takeWhileE,
  dropWhileE,
  cycleE,
  listAt,
  toToggle,

  printE,
  putStrLnE,
  -- * Event/Dynamic interaction
  hold,
  unhold,
  foldD,
  foldMaybeD,
  switchD,
  switchDyn,
  apply,
  applyMaybe,
  snap,
  attach,
  attachWith,
  attachWithMaybe,
  (<@>),
  (<@),
  -- * Effectful API
  RunFunctor(..),
  foreach,
  posteach,
  iterateE',
  foldE',
  foldMaybeE',
  accumE',
  accumMaybeE',
  filterE',
  mapMaybe',
  apply',
  applyMaybe',
  -- * Utilities

  -- **  Channels (interaction with the world)
  mchanEvt,
  tchanEvt,
  uchanEvt,

  -- ** Clock
  clock,
  pulse,
  ticks,
  timer,
  timerD,
  -- ** Random
  toRandom,
  toRandomR,
  withRandom,
  withRandomR,
  oneOf,
  withOneOf,
  freqOf,
  withFreqOf,
  randSkip,
  randSkipBy,

  -- * Re-exports
  liftA2,
  liftA3,
) where

import Control.Applicative (liftA2, liftA3)
import Control.Concurrent.Chan qualified as M
import Control.Concurrent.STM
import Control.Concurrent.Chan.Unagi (InChan)
import Data.Bifunctor
import Data.Time
import System.Random
import Data.VectorSpace

import Dyna.Gloss.Types
import Dyna qualified as D

infixl 4 <@>
infixl 4 <@

-- | Reference to running dynamic process by which we can query values (@readDyn@).
-- Also note that we no longer need the reference we should release the resources
-- by calling @cancelDyn@.
newtype DynRef a = DynRef { unDynRef :: D.DynRef Run a }

-- | Runs dynamic within the scope of the function.
-- It provides a callback with dyn getter as argument and after
-- callback finishes it shutdowns the dyn process.
withDyn :: Dyn a -> (Run a -> Run b) -> Run b
withDyn (Dyn dyn) f = D.withDyn dyn f

-- | Dyn that is constructed from effectful callback.
constDyn :: Run a -> Dyn a
constDyn act = Dyn $ D.constDyn act

-- | Executes dynamic for observation. The dynamic is step-wise constant
-- function that is driven by some event stream. The function runs the event stream
-- process in background and samples the updated state.
--
-- We can observe the value with @readDyn@. We need to shut down the stream when
-- we no longer need it with @cancelDyn@ function.
runDyn :: Dyn a -> Run (DynRef a)
runDyn (Dyn d) = DynRef <$> D.runDyn d
--
-- | Event that happens only once and happens right away.
once :: Run a -> Evt a
once ask = Evt $ D.once ask

-- | Event that never happens. Callback function is ignored.
never :: Evt a
never = Evt D.never

-- | Runs the argument event stream as background process
-- and produces event stream that is fed with events over channel (unagi-channel package).
-- When result event stream shuts down the background process also shuts down.
newEvt :: Evt a -> Run (Evt a)
newEvt (Evt evt) = Evt <$> D.newEvt evt

-- | Runs the dynamic process in background and returns dynamic
-- that just samples the background proces with @readDyn@.
newDyn :: Dyn a -> Run (Dyn a)
newDyn (Dyn dyn) = Dyn <$> D.newDyn dyn

-- | Accumulate over event stream.
accumE :: (a -> s -> (b, s)) -> s -> Evt a -> Evt b
accumE f s (Evt evt) = Evt $ D.accumE f s evt

-- | Accumulate over event stream.
accumE' :: (a -> s -> Run (b, s)) -> s -> Evt a -> Evt b
accumE' f s (Evt evt) = Evt $ D.accumE' f s evt

-- | Accumulate over event stream.
accumMaybeE :: (a -> s -> Maybe (b, s)) -> s -> Evt a -> Evt b
accumMaybeE f s (Evt evt) = Evt $ D.accumMaybeE f s evt

-- | Accumulate over event stream.
accumMaybeE' :: (a -> s -> Run (Maybe (b, s))) -> s -> Evt a -> Evt b
accumMaybeE' f s (Evt evt) = Evt $ D.accumMaybeE' f s evt

-- | Fold over event stream. Example:
--
-- > naturals = foldE (+) 0 pulse
foldE :: (a -> b -> b) -> b -> Evt a -> Evt b
foldE f s (Evt evt) = Evt $ D.foldE f s  evt

-- | Fold over event stream with effectful function.
foldE' :: (a -> b -> Run b) -> b -> Evt a -> Evt b
foldE' f s (Evt evt) = Evt $ D.foldE' f s evt

-- | Fold combined with filter. If accumulator function produces @Nothing@ on event then
-- that event is ignored and state is kept to previous state.
foldMaybeE :: (a -> b -> Maybe b) -> b -> Evt a -> Evt b
foldMaybeE f s (Evt evt) = Evt $ D.foldMaybeE f s evt


-- | Fold combined with filter for effectful function. See @foldMaybeE@ for details.
foldMaybeE' :: (a -> b -> Run (Maybe b)) -> b -> Evt a -> Evt b
foldMaybeE' f s (Evt evt) = Evt $ D.foldMaybeE' f s evt

-- | Iterates over event stream. It's like foldE but it ignores the values of underying stream
-- and starts with initial value as first element.
iterateE :: (a -> a) -> a -> Evt b -> Evt a
iterateE f val (Evt evt) = Evt $ D.iterateE f val evt

withIterateE :: (a -> a) -> a -> Evt b -> Evt (a, b)
withIterateE f val (Evt evt) = Evt $ D.withIterateE f val evt

-- | Effectful version for @iterateE@.
iterateE' :: (a -> Run a) -> a -> Evt b -> Evt a
iterateE' f val (Evt evt) = Evt $ D.iterateE' f val evt

-- | Reads current dynamic value.
readDyn :: DynRef  a -> Run a
readDyn (DynRef ref) = D.readDyn ref

-- | Shuts down the background process for dynamic and releases resulrces for
-- event stream that drives the dynamic.
cancelDyn :: DynRef a -> Run ()
cancelDyn (DynRef ref) = D.cancelDyn ref

-- | Turns event stream to dynamic. It holds the values of
-- events until the next event happen. It starts with initial value.
--
-- > hold initVal events = ...
hold :: a -> Evt a -> Dyn a
hold s (Evt evt) = Dyn $ D.hold s evt

-- | Counts how many events accured so far on the stream.
count :: Evt a -> Evt Int
count (Evt evt) = Evt (D.count evt)

withCount :: Evt a -> Evt (Int, a)
withCount (Evt evt) = Evt (D.withCount evt)

-- | Turns dynamic into event stream of underlying events
-- that trigger dynamic updates.
unhold :: Dyn a -> Evt a
unhold (Dyn dyn) = Evt $ D.unhold dyn

-- | Folds over event stream and converts it to dynamic.
foldD :: (a -> b -> b) -> b -> Evt a -> Dyn b
foldD f s (Evt evt) = Dyn $ D.foldD f s evt

-- | Accumulates the values with event stream that produce functions.
accumB :: a -> Evt (a -> a) -> Dyn a
accumB a (Evt evt) = Dyn $ D.accumB a evt

-- | Dynamic fold that can also filter out events. If Nothing is produced then the event is skipped.
foldMaybeD :: (a -> b -> Maybe b) -> b -> Evt a -> Dyn b
foldMaybeD f s (Evt evt) = Dyn $ D.foldMaybeD f s evt

-- | Adds some procedure to callback. Procedure is called prior to callback execution.
foreach :: (a -> Run ()) -> Evt a -> Evt a
foreach call (Evt evt) = Evt $ D.foreach call evt

-- | Adds some procedure to callback. Procedure is called after callback execution.
posteach :: (a -> Run ()) -> Evt a -> Evt a
posteach call (Evt evt) = Evt $ D.posteach call evt

--------------------------------------------------------------------------------
-- applications

-- | Applies a function to event stream value. The function is sampled
-- from dynamic process.
apply :: Dyn (a -> b) -> Evt a -> Evt b
apply (Dyn dyn) (Evt evt) = Evt $ D.apply dyn evt

-- | Effectful variant of @apply@.
apply' :: Dyn (a -> Run b) -> Evt a -> Evt b
apply' (Dyn dyn) (Evt evt) = Evt $ D.apply' dyn evt

-- | Infix variant of @apply@
(<@>) :: Dyn (a -> b) -> Evt a -> Evt b
(<@>) = apply

-- | Infix variant of @snap@.
(<@) :: Dyn a -> Evt b -> Evt a
(<@) = snap

-- | Apply combined with filter.
applyMaybe :: Dyn (a -> Maybe b) -> Evt a -> Evt b
applyMaybe (Dyn dyn) (Evt evt) = Evt $ D.applyMaybe dyn evt

-- | Effectful @applyMaybe@.
applyMaybe' :: Dyn (a -> Run (Maybe b)) -> Evt a -> Evt b
applyMaybe' (Dyn dyn) (Evt evt) = Evt $ D.applyMaybe' dyn evt

-- | Snapshot of dynamic process with event stream. All values
-- in the event stream are substituted with current value of dynamic.
snap :: Dyn a -> Evt b -> Evt a
snap (Dyn dyn) (Evt evt) = Evt (D.snap dyn evt)

-- | Kind of @zipWith@ function for dynamics and event streams.
attach :: Dyn a -> Evt b -> Evt (a, b)
attach (Dyn dyn) (Evt evt) = Evt $ D.attach dyn evt

-- | Kind of @zipWith@ function for dynamics and event streams.
attachWith :: (a -> b -> c) -> Dyn a -> Evt b -> Evt c
attachWith f (Dyn dyn) (Evt evt) = Evt $ D.attachWith f dyn evt

-- | Attach with filtering. When @Nothing@ is produced event is omitted from the stream.
attachWithMaybe :: (a -> b -> Maybe c) -> Dyn a -> Evt b -> Evt c
attachWithMaybe f (Dyn dyn) (Evt evt) = Evt $ D.attachWithMaybe f dyn evt

-- | Map with filtering. When @Nothing@ is produced event is omitted from the stream.
mapMaybe :: (a -> Maybe b) -> Evt a -> Evt b
mapMaybe f (Evt evt) = Evt $ D.mapMaybe f evt

-- | Map with filtering. When @Nothing@ is produced event is omitted from the stream.
mapMaybe' :: (a -> Run (Maybe b)) -> Evt a -> Evt b
mapMaybe' f (Evt evt) = Evt $ D.mapMaybe' f evt

--------------------------------------------------------------------------------
-- filters

-- | Filtering of the event strewams. Only events that produce True remain in the stream.
filterE :: (a -> Bool) -> Evt a -> Evt a
filterE f (Evt evt) = Evt $ D.filterE f evt

-- | Effectful filtering for event streams.
filterE' :: (a -> Run Bool) -> Evt a -> Evt a
filterE' f (Evt evt) = Evt $ D.filterE' f evt

-- | Filters based on Maybe. If @Nothing@ is produced forthe event it is omitted from the stream.
filterJust :: Evt (Maybe a) -> Evt a
filterJust (Evt evt) = Evt $ D.filterJust evt

-- | Filters with dynamic. When dynamic is true events pass through and when it's false
-- events are omitted.
whenE :: Dyn Bool -> Evt a -> Evt a
whenE (Dyn dyn) (Evt evt) = Evt $ D.whenE dyn evt

-- | Splits the either event stream.
splitE :: Evt (Either a b) -> (Evt a, Evt b)
splitE evt = (leftE evt, rightE evt)

-- | Gets all left events from the stream
leftE :: Evt (Either a b) -> Evt a
leftE (Evt evt) = Evt $ D.leftE evt

-- | Gets all right events from the stream
rightE :: Evt (Either a b) -> Evt b
rightE (Evt evt) = Evt $ D.rightE evt

-- | Takes only so many events from the stream
takeE :: Int -> Evt a -> Evt a
takeE n (Evt evt) = Evt $ D.takeE n evt

-- | Takes only so many events from the stream
dropE :: Int -> Evt a -> Evt a
dropE n (Evt evt) = Evt $ D.dropE n evt

-- | Takes events only while predicate is true.
takeWhileE :: (a -> Bool) -> Evt a -> Evt a
takeWhileE pred (Evt evt) = Evt $ D.takeWhileE pred evt

-- | Takes events only while predicate is true.
dropWhileE :: (a -> Bool) -> Evt a -> Evt a
dropWhileE pred (Evt evt) = Evt $ D.dropWhileE pred evt

-- | Takes elements from the list by index. If index is out of bounds the event is omitted.
listAt :: [a] -> Evt Int -> Evt a
listAt vals (Evt evt) = Evt $ D.listAt vals evt

-- | Turns event stream to toggle stream. It produce cyclic sequence of [True, False]
toToggle :: Evt a -> Evt Bool
toToggle (Evt evt) = Evt (D.toToggle evt)

-- | Cycles the values in the list over event sream.
cycleE :: [a] -> Evt b -> Evt a
cycleE vals (Evt evt) = Evt (D.cycleE vals evt)

-- | Sums all the elements in the event stream
sumE :: (Num a) => Evt a -> Evt a
sumE = foldE (+) 0

-- | Sums all the elements in the event stream
sumD :: (Num a) => Float -> Dyn a -> Dyn a
sumD dt (Dyn dyn) = Dyn (D.sumD (realToFrac dt) dyn)

integrate :: (VectorSpace v, Scalar v ~ Float) => Float -> Dyn v -> Dyn v
integrate dt (Dyn dyn) = Dyn $ D.integrate dt dyn

integrate2 :: (VectorSpace v, Scalar v ~ Float) => Float -> Dyn v -> Dyn v
integrate2 dt (Dyn dyn) = Dyn $ D.integrate2 dt dyn

-- | Finds the product of all elements in the event stream.
productE :: (Num a) => Evt a -> Evt a
productE = foldE (*) 1

-- | Monoidal append of all elements in the stream
appendE :: (Monoid a) => Evt a -> Evt a
appendE = foldE (<>) mempty

-- | Starts event stream process and as callback prints it values.
printE :: (Show a) => Evt a -> Run ()
printE (Evt evt) = D.printE evt

-- | Starts event stream process and as callback prints it values.
putStrLnE :: Evt String -> Run ()
putStrLnE (Evt evt) = D.putStrLnE evt

-- | Queries the event stream form dynamic and runs it all next event streams are ignored.
switchDyn :: Dyn (Evt a) -> Evt a
switchDyn (Dyn dyn) = Evt $ D.switchDyn $ fmap unEvt dyn

-- | Joins event stream of streams. If stream is started it runs until the end.
joinE :: Evt (Evt a) -> Evt a
joinE (Evt evt) = Evt $ D.joinE $ fmap unEvt evt

-- | Recursion on event streams. As event streams are functions we can not use
-- normal recursion that haskell provides. It will stuck the execution.
-- But we can use @fixE@ to create event stream that feeds back the events to itself.
--
-- Note that any sort of recursion can be implemented with @fixE@.
-- For example if we need 3-recursive event stream:
--
--  > fixE3 ::
--  >      (Evt a -> Evt b -> Evt c -> (Evt a, Evt b, Evt c))
--  >   -> (Evt a, Evt b, Evt c)
--
-- we can use sum tpye tags
-- to join it to single stream:
--
-- > data Tag a b c = TagA a | TagB b | TagC c
--
-- > fixE3 f = unwrap $ fixE g
-- >   where
-- >      g x = wrap (f (unwrapA x) (unwrapB x) (unwrapC x))
-- >
-- >      wrap a b c = mconcat [TagA <$> a, TagB <$> b, TagC <$> c]
-- >      unwrap evt = (unwrapA evt, unwrapB evt, unwrapC evt)
-- >
-- >      unwrapA = flip mapMaybe $ \x -> case x of
-- >                                  TagA a -> Just a
-- >                                  _      -> Nothing
--
-- We can use this trck with any number of streams. There are helper functions: @fixE2@, @fixE3@, @fixE4@
fixE :: (Evt a -> Evt a) -> Evt a
fixE f = Evt $ D.fixE (unEvt . f . Evt)

-- | Recursion for binary functions
fixE2 :: (Evt a -> Evt b -> (Evt a, Evt b)) -> (Evt a, Evt b)
fixE2 f = bimap Evt Evt $ D.fixE2 (\a b -> bimap unEvt unEvt $ f (Evt a) (Evt b))

-- | Recursion for ternary functions
fixE3 ::
     (Evt a -> Evt b -> Evt c -> (Evt a, Evt b, Evt c))
  -> (Evt a, Evt b, Evt c)
fixE3 f = wrap $ D.fixE3 (\a b c -> unwrap $ f (Evt a) (Evt b) (Evt c))
  where
    wrap (a, b, c) = (Evt a, Evt b, Evt c)
    unwrap (a, b, c) = (unEvt a, unEvt b, unEvt c)

-- | Recursion for functions of four arguments
fixE4 ::
     (Evt a -> Evt b -> Evt c -> Evt d -> (Evt a, Evt b, Evt c, Evt d))
  -> (Evt a, Evt b, Evt c, Evt d)
fixE4 f = wrap $ D.fixE4 (\a b c d -> unwrap $ f (Evt a) (Evt b) (Evt c) (Evt d))
  where
    wrap (a, b, c, d) = (Evt a, Evt b, Evt c, Evt d)
    unwrap (a, b, c, d) = (unEvt a, unEvt b, unEvt c, unEvt d)

-- | Flattens event stream producer by switching between event streams.
-- When next event stream happens it shuts down the previous one.
switchE :: Evt (Evt a) -> Evt a
switchE (Evt evt) = Evt $ D.switchE $ fmap unEvt evt

-- | Switches between dynamic producers.
switchD :: Dyn a -> Evt (Dyn a) -> Dyn a
switchD (Dyn d) (Evt evts) = Dyn $ D.switchD d (fmap unDyn evts)

---------------------------------------------------------
-- channels

-- | Creates the event stream that listens to MVar based channel.
-- If any value is put chan the event stream fires the callback.
mchanEvt :: M.Chan a -> Evt a
mchanEvt mchan = Evt $ D.mchanEvt (pure mchan)

-- | Creates the event stream that listens to MVar based channel.
-- If any value is put chan the event stream fires the callback.
tchanEvt :: TChan a -> Evt a
tchanEvt tchan = Evt $ D.tchanEvt (pure tchan)

-- | Creates the event stream that listens to unagi channel (package @unagi-chan@).
-- If any value is put chan the event stream fires the callback.
uchanEvt :: InChan a -> Evt a
uchanEvt uchan = Evt $ D.uchanEvt (pure uchan)

---------------------------------------------------------------------------
-- utilities

-- | Queries current time periodically with given period in seconds.
clock :: NominalDiffTime -> Evt UTCTime
clock t = Evt $ D.clock t

-- | Produces pulse events with given period in seconds.
pulse :: NominalDiffTime -> Evt ()
pulse t = Evt $ D.pulse t

-- | Produces pulse events with given period in seconds
-- and also tells how many seconds exactly has passed.
-- It can be useful for simulations of models that are based on differential equations.
-- As event streams carries how much time has passed between simulation steps.
ticks :: Float -> Evt Float
ticks t = Evt $ realToFrac <$> D.ticks (realToFrac t)

-- | Timer behaves like tocks only it produces accumulated time since beginning
-- of the process. It calculates them by querying current time and suntracting start time from it.
--
-- It can be though of as:
--
-- > sumE ticks
timer :: Float -> Evt Float
timer t = Evt $ realToFrac <$> D.timer (realToFrac t)

-- | Continuous timeline updated at given interval.
timerD :: Float -> Dyn Float
timerD t = Dyn $ realToFrac <$> D.timerD (realToFrac t)

--------------------------------------------------------------------------------

-- | Substitutes values in event stream with random values.
toRandom :: (Random b) => Evt a -> Evt b
toRandom (Evt evt) = Evt $ D.toRandom evt

-- | Substitutes values in event stream with random values from the given range.
toRandomR :: (Random b) => (b, b) -> Evt a -> Evt b
toRandomR range (Evt evt) = Evt $ D.toRandomR range evt

-- | Substitutes values in event stream with random values.
withRandom :: (Random b) => Evt a -> Evt (b, a)
withRandom (Evt evt) = Evt $ D.withRandom evt

-- | Substitutes values in event stream with random values from the given range.
withRandomR :: Random b => (b, b) -> Evt a -> Evt (b, a)
withRandomR range (Evt evt) = Evt $ D.withRandomR range evt

-- | Picks at random one element from the list
oneOf :: [a] -> Evt b -> Evt a
oneOf xs (Evt evt) = Evt $ D.oneOf xs evt

-- | Picks at random one element from the list
withOneOf :: [a] -> Evt b -> Evt (a, b)
withOneOf xs (Evt evt) = Evt (D.withOneOf xs evt)

-- | Picks at random one element from the list. We also provide distribution of events.
-- Probability to pick up the element. Sum of probabilities should equal to 1.
freqOf :: Dyn [(a, Rational)] -> Evt b -> Evt a
freqOf (Dyn dynVals) (Evt evts) = Evt $ D.freqOf dynVals evts

-- | Picks at random one element from the list. We also provide distribution of events.
-- Probability to pick up the element. Sum of probabilities should equal to 1.
withFreqOf :: Dyn [(a, Rational)] -> Evt b -> Evt (a, b)
withFreqOf (Dyn dynVals) (Evt evts) = Evt $ D.withFreqOf dynVals evts

-- | Skips at random elements from the list. We provide frequency to skip events with dynamic first argument.
randSkip :: Dyn Double -> Evt a -> Evt a
randSkip (Dyn prob) (Evt evt) = Evt $ D.randSkip prob evt

-- | Skips elements at random. The probability to skip element depends on the element itself.
randSkipBy :: Dyn (a -> Double) -> Evt a -> Evt a
randSkipBy  (Dyn prob) (Evt evt) = Evt $ D.randSkipBy prob evt

--------------------------------------------------------------------------------

-- | Delays in the thread of execution. Note that it can interfere
-- and screw up functions like clock, timer, pulse, ticks
delay :: NominalDiffTime -> Evt a -> Evt a
delay dt (Evt evt) = Evt $ D.delay dt evt

-- | Delays in background by forking on each event.
-- Note tht if delayed event was put into background prior
-- to stopping of the main event stream it will fire anyway.
-- There is no way to stop it.
delayFork :: NominalDiffTime -> Evt a -> Evt a
delayFork dt (Evt evt) = Evt $ D.delayFork dt evt

--------------------------------------------------------------------------------
-- effectful functor

class RunFunctor f where
  fmap' :: (a -> Run b) -> f a -> f b

instance RunFunctor Evt where
  fmap' f (Evt evt) = Evt $ D.fmap' f evt

instance RunFunctor Dyn where
  fmap' f (Dyn dyn) = Dyn $ D.fmap' f dyn

