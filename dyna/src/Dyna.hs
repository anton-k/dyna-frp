-- | Dyna is functional reactive programming library.
-- It describes event streams that are based on callbacks.
-- The event stream can produce something useful with callback that it consumes.
-- Also we have continous signals called @Dyn@ (short for dynamic).
-- The Dyn is sort of observance process of an event stream. For any
-- event that happen on event stream we remember that value and produce it
-- at any time until the next event will happen.
--
-- # Events
--
-- The event stream is just callback consumer funtion:
--
-- > newtype Evt m a = Evt {
-- >   runEvt :: (a -> m ()) -> m ()
-- > }
--
-- So it tells us: If you give me some callback @(a -> m ())@ I will apply it to the event
-- when event will occur. But when it will occur we don't know until we run the event.
-- All events happen at the same time. Every event triggers a callback.
-- This has some special nuance to it. That can differ from other FRP libraries.
-- For example monoidal append of two event streams:
--
--  > evtA <> evtB
--
-- In many FRP libraries we choose which element will happen or should we also append the events
-- if they happen "at the same time". For this library we spawn two concurrent processes
-- on background so if two events will happen at the same time callback will be called twice.
--
-- # Dynamics
--
-- The assumption is that dynamic is a process that evolves in time.
-- And as a human beings we can only ask for current values while process happens.
-- So we assemble the dynamics with combinators an after that we can run it's process:
--
-- > ref <-runDyn dynamicValue
--
-- It produces reference to the process which we can use to sample the current value in real time:
--
-- > readDyn ref
-- >  10
-- > readDyn ref  -- 5 seconds later
-- >  10
-- > readDyn ref  -- 5 seconds later
-- >  3
--
-- This reminds us of the notion of present moment. Take for example a weather temperature.
-- We can claim to build a model of weather and have an assumption of which value will happen tomorrow
-- but the exact value for it we can only measure at the moment when it will actually happen.
--
-- So the library is based on simple assumptions:
--
-- * Event stream is a callback processor
--
-- * Event stream happen at the same time as concurrent process
--
-- * Dynamic is a process and we can only query the current value for it
--
-- * Dynamics are based on event streams. The dynamic is an observation of some underlying event streams.
--    We just remember the last event and keep producing it until the next one wil arrive.
module Dyna(
  -- * Pipe
  (|>),
  -- * Class
  Frp(..),
  -- * Events
  Evt(..),
  once,
  never,
  -- * Dynamics
  Dyn(..),
  constDyn,
  runDyn,
  DynRef(..),
  readDyn,
  cancelDyn,

  -- * Control
  newEvt,
  newDyn,
  withDyn,

  -- * API
  -- * Event API
  scan,
  scanMay,
  mapMay,
  accum,
  accumB,
  accumMay,
  filters,
  filterJust,
  whens,
  splits,
  lefts,
  rights,
  iterates,
  withIterates,

  fix1,
  fix2,
  fix3,
  fix4,
  switch,
  joins,

  delay,
  delayFork,

  sums,
  sumD,
  integrate,
  integrate2,
  products,
  count,
  withCount,
  appends,
  foldMaps,
  takes,
  drops,
  takesWhile,
  dropsWhile,
  cycles,
  listAt,
  toToggle,

  forevers,
  races,
  forks,
  -- * Render streams
  heads,
  prints,
  putStrLns,
  folds,
  foldls,
  foldls',
  foldrs,
  foldrs',
  Parser,
  runParser,
  takeP,
  cycleP,
  headP,
  maybeP,
  -- * Event/Dynamic interaction
  hold,
  unhold,
  scanD,
  scanMayD,
  switchD,
  switchDyn,
  apply,
  applyMay,
  snap,
  attach,
  attachWith,
  attachWithMay,
  (<@>),
  (<@),
  -- * Effectful API
  FunctorM(..),
  foreach,
  posteach,
  iterates',
  scan',
  scanMay',
  accum',
  accumMay',
  filters',
  mapMay',
  apply',
  applyMay',
  -- * Utilities

  -- **  Channels (interaction with the world)
  mchanEvt,
  tchanEvt,
  uchanEvt,
  UChan,
  newTriggerEvt,
  -- ** IO
  getLines,

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
  BasisArity(..),
  module X,
) where

import Prelude hiding ((<*))
import Data.IORef
import Control.Applicative (liftA2, liftA3)
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Data.Functor
import Data.Bifunctor
import Data.Function
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.AdditiveGroup as X
import Data.AffineSpace as X
import Data.Basis
import Data.Cross as X
import Data.VectorSpace as X
import Data.String
import Control.Concurrent.Lifted
import Control.Concurrent.Thread.Delay qualified as D
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.Chan.Unagi (InChan)
import Control.Concurrent.Chan.Unagi qualified as U
import Control.Concurrent.Chan qualified as M

import Control.Monad.Trans.Control
import Dyna.Ref
import Data.Time
import System.Random (Random, newStdGen, randomR, random)
import Control.Exception.Lifted
import Control.Monad.Random.Class qualified as R
import Temporal.Class as X

import Data.Boolean

infixl 4 <@>
infixl 4 <@
infixl 0  |>

{-# inline (|>) #-}
-- | Pipe operator. We often write processors of event streams
-- It makes it convenient write them from first to the last:
--
-- > evt = proc1 |> proc2 |> ... |> procN
--
-- Instead of reversed order with @($)@:
--
-- > evt = procN $ ... $ proc2 $ proc1
(|>) :: a -> (a -> b) -> b
a |> f = f a


class (IsRef (Ref m), MonadBaseControl IO m, MonadIO m) => Frp m where
  type Ref m :: * -> *

instance Frp IO where
  type Ref IO = TVar

-- | Dynamics are step-wise constant effectful functions
-- each step transition is driven by underlying stream of events.
--
-- Meaning of the Dyn is a process that evolves in time.
-- We can start the process by running @runDyn@. It produces a reference to the
-- process that runs in background.
--
-- > runDyn :: Frp m => Dyn m a -> DynRef m a
--
-- When reference is initialized we can query current  value of it:
--
-- > readDyn :: DynRef m a -> m a
--
-- When we are done with observations we should shut down the background process with:
--
-- > cancelDyn :: DynRef m a -> m ()
--
-- It kills the background process and triggers the release function of underlying event stream.
data Dyn m a
  = forall s . Dyn
      { dyn'get     :: s -> m a   -- ^ get the value from internal state
      , dyn'evt     :: Evt m s    -- ^ stream of state updates
      , dyn'init    :: m s        -- ^ initial state
      , dyn'release :: m ()       -- ^ release resources for dynamic
      }
    -- ^ event based dynamic
  | ConstDyn a
    -- ^ Constant value

-- | Reference to running dynamic process by which we can query values (@readDyn@).
-- Also note that we no longer need the reference we should release the resources
-- by calling @cancelDyn@.
data DynRef m a
  = forall s . DynRef (s -> m a) (Ref m s) ThreadId (m ())
  | ConstRef a

-- | Runs dynamic within the scope of the function.
-- It provides a callback with dyn getter as argument and after
-- callback finishes it shutdowns the dyn process.
withDyn :: Frp m => Dyn m a -> (m a -> m b) -> m b
withDyn dyn f = bracket (runDyn dyn) cancelDyn (\ref -> f (readDyn ref))

-- | Dyn that is constructed from effectful callback.
constDyn :: Frp m => m a -> Dyn m a
constDyn act = Dyn (const act) never (pure ()) (pure ())

instance Functor m => Functor (Dyn m) where
  fmap f (ConstDyn a)                = ConstDyn (f a)
  fmap f (Dyn extract evt s release) = Dyn (fmap f . extract) evt s release

instance Frp m => Applicative (Dyn m) where
  pure a = ConstDyn a
  (ConstDyn f) <*> (ConstDyn a) = ConstDyn (f a)
  (ConstDyn f) <*> (Dyn aget aevt as release) = Dyn (\s -> f <$> aget s) aevt as release
  (Dyn fget fevt fs release) <*> (ConstDyn a) = Dyn (\s -> ($ a) <$> fget s) fevt fs release
  (Dyn fget fevt fs releaseF) <*> (Dyn aget aevt as releaseA) =
    Dyn (\(f, a) -> (fget f) <*> (aget a)) evt (liftA2 (,) fs as) (releaseF >> releaseA)
    where
      evt = Evt $ \go -> do
        tv <- proxyNewRef evt =<< liftA2 (,) fs as
        void $ fork $ runEvt joint $ \case
          Left  s -> do
            liftIO $ modifyRef tv (first $ const s)
            go =<< liftIO (readRef tv)
          Right s -> do
            liftIO $ modifyRef tv (second $ const s)
            go =<< liftIO (readRef tv)

      joint = fmap Left fevt <> fmap Right aevt

-- | Event stream. The meaning of an event is a callback consumer function.
-- If we give callback to it it will do something useful based on it.
--
-- The main function is runEvt:
--
-- > runEvt :: Evt m a -> (a -> m ()) -> m ()
-- > runEvt events callback = ...
--
-- Let's look at simple examples of the event streams:
--
-- Event that never produce anything:
--
-- > never = Evt {
-- >    runEvt _ = pure ()
-- >  }
--
-- So it just ignores the callback and returns right away.
--
-- Event that happens only once:
--
-- > once :: m a -> Evt m a
-- > once get = Evt {
-- >     runEvt go = go =<< get
-- >  }
--
-- It just gets the value right away and applies callback to it.
-- We can try it out in the interpreter:
--
-- > putStrLnE $ fmap ("Your message: " <> ) $ once getLine
--
-- We have useful functions to print out the events: @putStrLnE@ and @printE@.
--
-- Also we have event streams that happen periodically:
--
-- > printE $ clock 1  -- prints time every second
--
-- ## Duplication of the events.
--
-- Note that event streams are functions that do side-effects within some monad.
-- We use them as values but it means that two values with the same event stream definition
-- can produce different results. For example:
--
-- > a = toRandomR (0, 10) $ clock 1
-- > b = a
--
-- Note that a and b will each have their own copy of underlying random event stream.
-- So if you use it in the code don't expect values to be the same.
--
-- But if we want them to be the same we can copy event from it's definition with function:
--
-- > newEvt :: Evt m a -> m (Evt m a)
--
-- It starts the underying event stream process n background and sends all events
-- to the result by channel. With nice property of when we shut down the result event the
-- background process also shuts down.
--
-- > a <- newEvt toRandomR (0, 10) $ clock 1
-- > b = a
--
-- In this example event streams @a@ and @b@ will have the same events during execution.
newtype Evt m a = Evt { runEvt :: (a -> m ()) -> m () }

-- | Event that happens only once and happens right away.
once :: Frp m => m a -> Evt m a
once ask = Evt $ \go -> go =<< ask

-- | Event that never happens. Callback function is ignored.
never :: Frp m => Evt m a
never = Evt (const $ pure ())

-- | Runs the argument event stream as background process
-- and produces event stream that is fed with events over channel (unagi-channel package).
-- When result event stream shuts down the background process also shuts down.
newEvt :: Frp m => Evt m a -> m (Evt m a)
newEvt evt = do
  ch <- liftIO $ U.newChan
  tid <- fork $ runEvt evt $ liftIO . U.writeChan (fst ch)
  pure $ uchanEvtFinally tid (pure $ fst ch)

uchanEvtFinally :: (Frp m) => ThreadId -> m (InChan a) -> Evt m a
uchanEvtFinally tid mchan = Evt $ \go -> do
  chan <- liftIO . U.dupChan =<< mchan
  loop chan go `finally` (killThread tid)
  where
    loop chan go = do
      a <- liftIO $ U.readChan chan
      go a
      loop chan go

-- | Runs the dynamic process in background and returns dynamic
-- that just samples the background proces with @readDyn@.
newDyn :: Frp m => Dyn m a -> m (Dyn m a)
newDyn dyn = do
  ref <- runDyn dyn
  pure $ Dyn readDyn never (pure ref) (cancelDyn ref)

instance Functor (Evt m) where
  fmap f (Evt evt) = Evt $ \proc -> evt (proc . f)

instance Frp m => Semigroup (Evt m a) where
  (<>) (Evt a) (Evt b) = Evt $ \proc ->
    concurrently_ (a proc) (b proc)

-- | Shutdown the remaining event if one of the events close up early.
races :: Frp m => Evt m a -> Evt m a -> Evt m a
races (Evt a) (Evt b) = Evt $ \go ->
  race_ (a go) (b go)

-- | Execute each callback in separate thread
forks :: Frp m => Evt m a -> Evt m a
forks evt =
  Evt $ \go -> runEvt evt $ void . fork . go

instance Frp m => Monoid (Evt m a) where
  mempty = never

instance Frp m => Applicative (Evt m) where
  pure a = once (pure a)
  f <*> a = a >>= (\x -> fmap ( $ x) f)

instance Frp m => Monad (Evt m) where
  (>>=) a f = switch (fmap f a)

-- | Accumulate over event stream.
accum :: Frp m => (a -> s -> (b, s)) -> s -> Evt m a -> Evt m b
accum f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    (b, s) <- f x <$> liftIO (readRef ref)
    go b
    liftIO $ writeRef ref s

-- | Accumulate over event stream.
accum' :: Frp m => (a -> s -> m (b, s)) -> s -> Evt m a -> Evt m b
accum' f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    (b, s) <- f x =<< liftIO (readRef ref)
    go b
    liftIO $ writeRef ref s

-- | Accumulate over event stream.
accumMay :: Frp m => (a -> s -> Maybe (b, s)) -> s -> Evt m a -> Evt m b
accumMay f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    mRes <- f x <$> liftIO (readRef ref)
    forM_ mRes $ \(b, s) -> do
      go b
      liftIO $ writeRef ref s

-- | Accumulate over event stream.
accumMay' :: Frp m => (a -> s -> m (Maybe (b, s))) -> s -> Evt m a -> Evt m b
accumMay' f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    mRes <- f x =<< liftIO (readRef ref)
    forM_ mRes $ \(b, s) -> do
      go b
      liftIO $ writeRef ref s

-- | scan over event stream. Example:
--
-- > naturals = scan (+) 0 pulse
scan :: Frp m => (a -> b -> b) -> b -> Evt m a -> Evt m b
scan f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    s <- f x <$> liftIO (readRef ref)
    go s
    liftIO $ writeRef ref s

-- | scan over event stream with effectful function.
scan' :: Frp m => (a -> b -> m b) -> b -> Evt m a -> Evt m b
scan' f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    s <- f x =<< liftIO (readRef ref)
    go s
    liftIO $ writeRef ref s

-- | scan combined with filter. If accumulator function produces @Nothing@ on event then
-- that event is ignored and state is kept to previous state.
scanMay :: Frp m => (a -> b -> Maybe b) -> b -> Evt m a -> Evt m b
scanMay f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    ms <- f x <$> liftIO (readRef ref)
    forM_ ms $ \s -> do
      go s
      liftIO $ writeRef ref s

-- | scan combined with filter for effectful function. See @scanMay@ for details.
scanMay' :: Frp m => (a -> b -> m (Maybe b)) -> b -> Evt m a -> Evt m b
scanMay' f s evt = Evt $ \go -> do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> do
    ms <- f x =<< liftIO (readRef ref)
    forM_ ms $ \s -> do
      go s
      liftIO $ writeRef ref s

-- | Iterates over event stream. It's like scan but it ignores the values of underying stream
-- and starts with initial value as first element.
iterates :: Frp m => (a -> a) -> a -> Evt m b -> Evt m a
iterates f val evt = Evt $ \go -> do
  ref <- proxyNewRef evt val
  runEvt evt $ \_ -> do
    s <- liftIO (readRef ref)
    go s
    liftIO $ writeRef ref (f s)

withIterates :: Frp m => (a -> a) -> a -> Evt m b -> Evt m (a, b)
withIterates f val evt = Evt $ \go -> do
  ref <- proxyNewRef evt val
  runEvt evt $ \x -> do
    s <- liftIO (readRef ref)
    go (s, x)
    liftIO $ writeRef ref (f s)


-- | Effectful version for @iterates@.
iterates' :: Frp m => (a -> m a) -> a -> Evt m b -> Evt m a
iterates' f val evt = Evt $ \go -> do
  ref <- proxyNewRef evt val
  runEvt evt $ \_ -> do
    s <- liftIO (readRef ref)
    go s
    liftIO . writeRef ref =<< f s

instance (Frp m, Num a) => Num (Dyn m a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

instance (Frp m, Fractional a) => Fractional (Dyn m a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance (Frp m, Semigroup a) => Semigroup (Dyn m a) where
  (<>) = liftA2 (<>)

instance (Frp m, Monoid a) => Monoid (Dyn m a) where
  mempty = pure mempty

instance (Frp m, IsString a) => IsString (Dyn m a) where
  fromString = pure . fromString

-- | Reads current dynamic value.
readDyn :: Frp m => DynRef m a -> m a
readDyn (ConstRef val) = pure val
readDyn (DynRef extract ref _ _) = do
  s <- liftIO (readRef ref)
  extract s

-- | Shuts down the background process for dynamic and releases resulrces for
-- event stream that drives the dynamic.
cancelDyn :: Frp m => DynRef m a -> m ()
cancelDyn (ConstRef _) = pure ()
cancelDyn (DynRef _ _ tid release) = killThread tid >> release

-- | Executes dynamic for observation. The dynamic is step-wise constant
-- function that is driven by some event stream. The function runs the event stream
-- process in background and samples the updated state.
--
-- We can observe the value with @readDyn@. We need to shut down the stream when
-- we no longer need it with @cancelDyn@ function.
runDyn :: Frp m => Dyn m a -> m (DynRef m a)
runDyn (ConstDyn val) = pure (ConstRef val)
runDyn dyn@(Dyn extract evt init release) = do
  ref <- proxyNewRefDyn dyn =<< init
  tid <- fork $ runEvt evt $ \s -> do
    liftIO $ writeRef ref s
  pure (DynRef extract ref tid release)

-- | Turns event stream to dynamic. It holds the values of
-- events until the next event happen. It starts with initial value.
--
-- > hold initVal events = ...
hold :: Frp m => a -> Evt m a -> Dyn m a
hold s evt = Dyn pure evt (pure s) (pure ())

-- | Counts how many events accured so far on the stream.
count :: Frp m => Evt m a -> Evt m Int
count = scan (const succ) 0

withCount :: Frp m => Evt m a -> Evt m (Int, a)
withCount = accum (\a b -> ((b, a), succ b)) 1

-- | Turns dynamic into event stream of underlying events
-- that trigger dynamic updates.
unhold :: Frp m => Dyn m a -> Evt m a
unhold (ConstDyn val) = Evt $ \go -> go val
unhold (Dyn extract evts init release) = Evt $ \go -> do
  go =<< extract =<< init
  runEvt evts (go <=< extract) `finally` release

-- | scans over event stream and converts it to dynamic.
scanD :: Frp m => (a -> b -> b) -> b -> Evt m a -> Dyn m b
scanD f s evt = hold s (scan f s evt)

-- | Accumulates the values with event stream that produce functions.
accumB :: Frp m => a -> Evt m (a -> a) -> Dyn m a
accumB a evt = scanD ($) a evt

-- | Dynamic scan that can also filter out events. If Nothing is produced then the event is skipped.
scanMayD :: Frp m => (a -> b -> Maybe b) -> b -> Evt m a -> Dyn m b
scanMayD f s evt = hold s (scanMay f s evt)

-- | Adds some procedure to callback. Procedure is called prior to callback execution.
foreach :: Frp m => (a -> m ()) -> Evt m a -> Evt m a
foreach call evt = Evt $ \go ->
  runEvt evt $ \x -> do
    call x
    go x

-- | Adds some procedure to callback. Procedure is called after callback execution.
posteach :: Frp m => (a -> m ()) -> Evt m a -> Evt m a
posteach call evt = Evt $ \go ->
  runEvt evt $ \x -> do
    go x
    call x

--------------------------------------------------------------------------------
-- applications

-- | Applies a function to event stream value. The function is sampled
-- from dynamic process.
apply :: Frp m => Dyn m (a -> b) -> Evt m a -> Evt m b
apply dyn evt = Evt $ \go -> do
  ref <- runDyn dyn
  runEvt evt (\b -> do
      go . ($ b) =<< readDyn ref
    )
    `finally` cancelDyn ref

-- | Effectful variant of @apply@.
apply' :: Frp m => Dyn m (a -> m b) -> Evt m a -> Evt m b
apply' dyn evt = Evt $ \go -> do
  ref <- runDyn dyn
  runEvt evt (\b -> do
    (\f -> go =<< f b) =<< readDyn ref)
    `finally` cancelDyn ref

-- | Infix variant of @apply@
(<@>) :: Frp m => Dyn m (a -> b) -> Evt m a -> Evt m b
(<@>) = apply

-- | Infix variant of @snap@.
(<@) :: Frp m => Dyn m a -> Evt m b -> Evt m a
(<@) = snap

-- | Apply combined with filter.
applyMay :: Frp m => Dyn m (a -> Maybe b) -> Evt m a -> Evt m b
applyMay dyn evt = Evt $ \go -> do
  ref <- runDyn dyn
  runEvt evt (\b -> do
    mapM_ go . ($ b) =<< readDyn ref)
    `finally` cancelDyn ref

-- | Effectful @applyMay@.
applyMay' :: Frp m => Dyn m (a -> m (Maybe b)) -> Evt m a -> Evt m b
applyMay' dyn evt = Evt $ \go -> do
  ref <- runDyn dyn
  runEvt evt (\b -> do
    (\f -> mapM_ go =<< f b) =<< readDyn ref)
    `finally` cancelDyn ref

-- | Snapshot of dynamic process with event stream. All values
-- in the event stream are substituted with current value of dynamic.
snap :: Frp m => Dyn m a -> Evt m b -> Evt m a
snap dyn evt = apply (const <$> dyn) evt

-- | Attach element from dyn to event stream.
attach :: Frp m => Dyn m a-> Evt m b -> Evt m (a, b)
attach dyn evt = attachWith (,) dyn evt

-- | Kind of @zipWith@ function for dynamics and event streams.
attachWith :: Frp m => (a -> b -> c) -> Dyn m a -> Evt m b -> Evt m c
attachWith f dyn evt = apply (f <$> dyn) evt

-- | Attach with filtering. When @Nothing@ is produced event is omitted from the stream.
attachWithMay :: Frp m => (a -> b -> Maybe c) -> Dyn m a -> Evt m b -> Evt m c
attachWithMay f dyn evt = applyMay (f <$> dyn) evt

--------------------------------------------------------------------------------
-- filters

-- | Map with filtering. When @Nothing@ is produced event is omitted from the stream.
mapMay :: Frp m => (a -> Maybe b) -> Evt m a -> Evt m b
mapMay f evt = Evt $ \go -> runEvt evt (mapM_ go . f)

-- | Effectful @mapMay@
mapMay' :: Frp m => (a -> m (Maybe b)) -> Evt m a -> Evt m b
mapMay' f evt = Evt $ \go -> runEvt evt (mapM_ go <=< f)

-- | Filtering of the event strewams. Only events that produce True remain in the stream.
filters :: Frp m => (a -> Bool) -> Evt m a -> Evt m a
filters f evt = Evt $ \go -> runEvt evt (\x -> when (f x) (go x))

-- | Effectful filtering for event streams.
filters' :: Frp m => (a -> m Bool) -> Evt m a -> Evt m a
filters' f evt = Evt $ \go -> runEvt evt (\x -> (\cond -> when cond (go x)) =<< f x)

-- | Filters based on Maybe. If @Nothing@ is produced forthe event it is omitted from the stream.
filterJust :: Frp m => Evt m (Maybe a) -> Evt m a
filterJust evt = Evt $ \go -> runEvt evt (mapM_ go)

-- | Filters with dynamic. When dynamic is true events pass through and when it's false
-- events are omitted.
whens :: Frp m => Dyn m Bool -> Evt m a -> Evt m a
whens dyn evt = Evt $ \go -> do
  ref <- runDyn dyn
  runEvt evt $ \b -> do
    a <- readDyn ref
    when a (go b)

-- | Splits the either event stream.
splits :: Frp m => Evt m (Either a b) -> (Evt m a, Evt m b)
splits evt = (lefts evt, rights evt)

-- | Gets all left events from the stream
lefts :: Frp m => Evt m (Either a b) -> Evt m a
lefts evt = mapMay (either Just (const Nothing)) evt

-- | Gets all right events from the stream
rights :: Frp m => Evt m (Either a b) -> Evt m b
rights evt = mapMay (either (const Nothing) Just) evt

-- | Takes only so many events from the stream
takes :: Frp m => Int -> Evt m a -> Evt m a
takes n evt = Evt $ \go -> do
  ref <- proxyNewRef evt 0
  waitAsync $ do
    runEvt evt $ \x -> do
      cur <- liftIO (readRef ref)
      when (cur < n) $ do
        go x
        when (cur == n - 1) stopSelf
        liftIO $ writeRef ref (cur + 1)

-- | Drops first so many events from the stream
drops :: Frp m => Int -> Evt m a -> Evt m a
drops n evt = Evt $ \go -> do
  tv <- proxyNewRef evt n
  runEvt evt $ \x -> do
    cur <- liftIO (readRef tv)
    if (cur <= 0)
      then go x
      else liftIO (modifyRef tv pred)

stopSelf :: Frp m => m ()
stopSelf = killThread =<< myThreadId

waitStop :: Frp m => Async a -> m ()
waitStop x = void $ liftIO $ waitCatch x

waitAsync :: Frp m => m () -> m ()
waitAsync act = do
  tid <- async act
  waitStop tid

-- | Takes events only while predicate is true.
takesWhile :: Frp m => (a -> Bool) -> Evt m a -> Evt m a
takesWhile pred evt = Evt $ \go -> do
  waitAsync $ do
    runEvt evt $ \x -> do
      if (pred x)
        then go x
        else stopSelf

-- | Drops events while predicate is true.
dropsWhile :: Frp m => (a -> Bool) -> Evt m a -> Evt m a
dropsWhile pred evt = Evt $ \go -> do
  tv <- proxyNewRef evt True
  runEvt evt $ \x -> do
    cur <- liftIO (readRef tv)
    if cur
      then
        unless (pred x) $ do
          liftIO $ writeRef tv False
          go x
      else go x

-- | Takes elements from the list by index. If index is out of bounds the event is omitted.
listAt :: Frp m => [a] -> Evt m Int -> Evt m a
listAt vals evt = mapMay (vec V.!?) evt
  where
    vec = V.fromList vals

-- | Turns event stream to toggle stream. It produce cyclic sequence of [True, False]
toToggle :: Frp m => Evt m a -> Evt m Bool
toToggle = iterates not True

-- | Cycles the values in the list over event sream.
cycles :: Frp m => [a] -> Evt m b -> Evt m a
cycles vals evt = fmap (vec V.!) $ iterates ((`mod` len) . succ) 0 evt
  where
    vec = V.fromList vals
    len = V.length vec

-- | Sums all the elements in the event stream
sums :: (Frp m, Num a) => Evt m a -> Evt m a
sums = scan (+) 0

-- | Integrates signal of vectors with given time step
integrate :: (Frp m, VectorSpace v, Real (Scalar v), Fractional (Scalar v)) => (Scalar v) -> Dyn m v -> Dyn m v
integrate dt dyn =
  hold zeroV $ scan (^+^) zeroV (attachWith (\v k -> realToFrac k *^ v) dyn (ticks (realToFrac dt)))

-- | More accurate integration of signal of vectors with given time step
integrate2 :: (Frp m, VectorSpace v, Real (Scalar v), Fractional (Scalar v)) => (Scalar v) -> Dyn m v -> Dyn m v
integrate2 dt dyn =
  hold zeroV $ fmap snd $ scan go (Nothing, zeroV) (attach dyn (ticks (realToFrac dt)))
  where
    go (v, h) (mPrev, res) = ((Just (v, h), ) . (res ^+^ )) $ case mPrev of
      Nothing       -> realToFrac h *^ v
      Just (v0, h0) -> (realToFrac h * 0.5) *^ (v0 ^+^ v)

-- | Sums all points in the signal with given time step
sumD :: (Frp m, Num a) => NominalDiffTime -> Dyn m a -> Dyn m a
sumD dt dyn = hold 0 $ sums (snap dyn (pulse dt))


-- | Finds the product of all elements in the event stream.
products :: (Frp m, Num a) => Evt m a -> Evt m a
products = scan (*) 1

-- | Monoidal append of all elements in the stream
appends :: (Frp m, Monoid a) => Evt m a -> Evt m a
appends = scan (flip (<>)) mempty

-- | Same as foldMap only for streams.
foldMaps :: (Frp m, Monoid b) => (a -> b) -> Evt m a -> Evt m b
foldMaps f = appends . fmap f

-- | Monoidal fold for event streams, note that stream have to be finite for
-- the function to complete
folds :: (Frp m, Monoid a) => Evt m a -> m a
folds = foldls (<>) mempty

-- | Left fold for event streams, note that stream have to be finite for
-- the function to complete
foldls :: (Frp m) => (b -> a -> b) -> b -> Evt m a -> m b
foldls f s evt = do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> liftIO $ modifyRef ref $ flip f x
  liftIO $ readRef ref

-- | Effectful left fold
foldls' :: (Frp m) => (b -> a -> m b) -> b -> Evt m a -> m b
foldls' f s evt = do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> liftIO . writeRef ref =<< flip f x =<< liftIO (readRef ref)
  liftIO $ readRef ref

-- | Right fold for event streams, note that stream have to be finite for
-- the function to complete
foldrs :: (Frp m) => (a -> b -> b) -> b -> Evt m a -> m b
foldrs f s evt = do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> liftIO $ modifyRef ref $ f x
  liftIO $ readRef ref

-- | Effectful right fold
foldrs' :: (Frp m) => (a -> b -> m b) -> b -> Evt m a -> m b
foldrs' f s evt = do
  ref <- proxyNewRef evt s
  runEvt evt $ \x -> liftIO . writeRef ref =<< f x =<< liftIO (readRef ref)
  liftIO $ readRef ref

-- | Starts event stream process and as callback prints it values.
prints :: (Frp m, Show a) => Evt m a -> m ()
prints evt = runEvt evt (liftIO . print)

-- | Starts event stream process and as callback prints it values.
putStrLns :: (Frp m) => Evt m String -> m ()
putStrLns evt = runEvt evt (liftIO . putStrLn)

-- | Stream of user inputs
getLines :: Frp m => Evt m String
getLines = once (liftIO getLine)

-- | Queries the event stream form dynamic and runs it all next event streams are ignored.
switchDyn :: Frp m => Dyn m (Evt m a) -> Evt m a
switchDyn dyn = Evt $ \go -> do
  ref <- runDyn dyn
  evt <- readDyn ref
  runEvt evt go

-- | Joins event stream of streams. If stream is started it runs until the end.
joins :: Frp m => Evt m (Evt m a) -> Evt m a
joins evt = Evt $ \go ->
  runEvt evt $ \e -> void $ fork $ runEvt e go

-- | Recursion on event streams. As event streams are functions we can not use
-- normal recursion that haskell provides. It will stuck the execution.
-- But we can use @fixE@ to create event stream that feeds back the events to itself.
--
-- Note that any sort of recursion can be implemented with @fixE@.
-- For example if we need 3-recursive event stream:
--
--  > fixE3 ::
--  >      (Evt m a -> Evt m b -> Evt m c -> m (Evt m a, Evt m b, Evt m c))
--  >   -> (Evt m a, Evt m b, Evt m c)
--
-- we can use sum tpye tags
-- to join it to single stream:
--
-- > data Tag a b c = TagA a | TagB b | TagC c
--
-- > fixE3 f = unwrap $ fixE g
-- >   where
-- >      g x = wrap <$> f (unwrapA x) (unwrapB x) (unwrapC x)
-- >
-- >      wrap a b c = mconcat [TagA <$> a, TagB <$> b, TagC <$> c]
-- >      unwrap evt = (unwrapA evt, unwrapB evt, unwrapC evt)
-- >
-- >      unwrapA = flip mapMay $ \x -> case x of
-- >                                  TagA a -> Just a
-- >                                  _      -> Nothing
--
-- We can use this trck with any number of streams. There are helper functions: @fixE2@, @fixE3@, @fixE4@
fix1 :: Frp m => (Evt m a -> m (Evt m a)) -> Evt m a
fix1 f = Evt $ \go -> do
  chan <- liftIO U.newChan
  let evt = uchanEvt (fst chan)
  evt' <- f evt
  runEvt evt' $ \x -> do
    liftIO $ U.writeChan (fst chan) x
    go x

-- | Recursion for binary functions
fix2 :: Frp m => (Evt m a -> Evt m b -> m (Evt m a, Evt m b)) -> (Evt m a, Evt m b)
fix2 f = splits $ fix1 g
  where
    g x = wrap <$> f (lefts x) (rights x)
    wrap (a, b) = (Left <$> )a <> (Right <$> b)

data Tag3 a b c = TagA3 a | TagB3 b | TagC3 c

-- | Recursion for ternary functions
fix3 :: Frp m
  => (Evt m a -> Evt m b -> Evt m c -> m (Evt m a, Evt m b, Evt m c))
  -> (Evt m a, Evt m b, Evt m c)
fix3 f = unwrap $ fix1 g
  where
    g x = wrap <$> f (unwrapA x) (unwrapB x) (unwrapC x)
    wrap (a, b, c) = (TagA3 <$> a) <> (TagB3 <$> b) <> (TagC3 <$> c)

    unwrap x = (unwrapA x, unwrapB x, unwrapC x)

    unwrapA = mapMay $ \case
                TagA3 a -> Just a
                _       -> Nothing

    unwrapB = mapMay $ \case
                TagB3 a -> Just a
                _       -> Nothing

    unwrapC = mapMay $ \case
                TagC3 a -> Just a
                _       -> Nothing


data Tag4 a b c d = TagA4 a | TagB4 b | TagC4 c | TagD4 d

-- | Recursion for functions of four arguments
fix4 :: Frp m =>
     (Evt m a -> Evt m b -> Evt m c -> Evt m d -> m (Evt m a, Evt m b, Evt m c, Evt m d))
  -> (Evt m a, Evt m b, Evt m c, Evt m d)
fix4 f = unwrap $ fix1 g
  where
    g x = wrap <$> f (unwrapA x) (unwrapB x) (unwrapC x) (unwrapD x)
    wrap (a, b, c, d) = (TagA4 <$> a) <> (TagB4 <$> b) <> (TagC4 <$> c) <> (TagD4 <$> d)

    unwrap x = (unwrapA x, unwrapB x, unwrapC x, unwrapD x)

    unwrapA = mapMay $ \case
                TagA4 a -> Just a
                _       -> Nothing

    unwrapB = mapMay $ \case
                TagB4 a -> Just a
                _       -> Nothing

    unwrapC = mapMay $ \case
                TagC4 a -> Just a
                _       -> Nothing

    unwrapD = mapMay $ \case
                TagD4 a -> Just a
                _       -> Nothing

-- | Flattens event stream producer by switching between event streams.
-- When next event stream happens it shuts down the previous one.
switch :: Frp m => Evt m (Evt m a) -> Evt m a
switch evts = Evt $ \go -> do
  tidRef <- proxyNewRef evts Nothing
  let stop = mapM_ killThread =<< liftIO (readRef tidRef)
  lock <- newEmptyMVar  -- we use this lock to make sure that next process
                        -- does not start before we saved it's threadId for stopping.
  runEvt evts (\evt -> do
    stop
    tid <- fork (takeMVar lock >> runEvt evt go)  -- delay until threadId is saved
    liftIO $ writeRef tidRef (Just tid)           -- save tid (for stopping)
    putMVar lock ()                               -- start the event process
    ) `finally` stop

-- | Switches between dynamic producers.
switchD :: Frp m => Dyn m a -> Evt m (Dyn m a) -> Dyn m a
switchD d evts = Dyn extract resEvt init (pure ())
  where
    init    = runDyn d
    extract = readDyn

    resEvt = Evt $ \go -> do
      runEvt evts $ \dyn -> do
        ref <- runDyn dyn
        go ref `finally` cancelDyn ref

---------------------------------------------------------
-- channels

-- | Creates the event stream that listens to MVar based channel.
-- If any value is put chan the event stream fires the callback.
mchanEvt :: (Frp m) => M.Chan a -> Evt m a
mchanEvt chan = Evt $ \go -> do
  chan <- liftIO $ M.dupChan chan
  loop chan go
  where
    loop chan go = do
      a <- liftIO $ M.readChan chan
      go a
      loop chan go

-- | Creates the event stream that listens to @TChan@ based channel.
-- If any value is put chan the event stream fires the callback.
tchanEvt :: (Frp m) => TChan a -> Evt m a
tchanEvt chan = Evt $ \go -> do
  chan <- liftIO $ atomically $ dupTChan chan
  loop chan go
  where
    loop chan go = do
      a <- liftIO $ atomically $ readTChan chan
      go a
      loop chan go

-- | Creates the event stream that listens to unagi channel (package @unagi-chan@).
-- If any value is put chan the event stream fires the callback.
uchanEvt :: (Frp m) => InChan a -> Evt m a
uchanEvt chan = Evt $ \go -> do
  chan <- liftIO $ U.dupChan chan
  loop chan go
  where
    loop chan go = do
      a <- liftIO $ U.readChan chan
      go a
      loop chan go

type UChan a = (U.InChan a, U.OutChan a)

--------------------------------------------------------------------------------

proxyNewRef :: Frp m => Evt m a -> b -> m (Ref m b)
proxyNewRef _ v = liftIO $ newRef v

proxyNewRefDyn :: Frp m => Dyn m a -> b -> m (Ref m b)
proxyNewRefDyn _ v = liftIO $ newRef v

proxyFunRes :: (a -> b) -> b
proxyFunRes _ = undefined

---------------------------------------------------------------------------
-- utilities

-- | Queries current time periodically with given period in seconds.
clock :: Frp m => NominalDiffTime -> Evt m UTCTime
clock t = Evt $ \go -> periodic t $ go =<< liftIO getCurrentTime

-- | Produces pulse events with given period in seconds.
pulse :: Frp m => NominalDiffTime -> Evt m ()
pulse t = Evt $ \go -> periodic t (go ())

-- | Produces pulse events with given period in seconds
-- and also tells how many seconds exactly has passed.
-- It can be useful for simulations of models that are based on differential equations.
-- As event streams carries how much time has passed between simulation steps.
ticks :: Frp m => NominalDiffTime -> Evt m NominalDiffTime
ticks t = Evt $ \go -> do
  startRef <- liftIO $ newIORef =<< getCurrentTime
  periodic t $ do
    dt <- liftIO $ do
      cur   <- getCurrentTime
      start <- readIORef startRef
      writeIORef startRef cur
      pure $ cur `diffUTCTime` start
    go dt

-- | Timer behaves like tocks only it produces accumulated time since beginning
-- of the process. It calculates them by querying current time and suntracting start time from it.
--
-- It can be though of as:
--
-- > sumE ticks
timer :: Frp m => NominalDiffTime -> Evt m NominalDiffTime
timer t = Evt $ \go -> do
  start <- liftIO getCurrentTime
  periodic t $ go =<< liftIO ((`diffUTCTime` start) <$> getCurrentTime)

-- | Timer as dynamic signal.
timerD :: Frp m => NominalDiffTime -> Dyn m NominalDiffTime
timerD t = hold 0 $ timer t

{-# NOINLINE periodic #-}
-- | Periodically triggers callback.
periodic :: MonadIO m => NominalDiffTime -> m () -> m ()
periodic dur proc = do
  startRef <- liftIO $ newIORef =<< getCurrentTime
  fix $ \next -> do
    proc
    time <- liftIO $ do
      last <- readIORef startRef
      cur  <- getCurrentTime
      let dt = max 0 $ dur - (cur `diffUTCTime` last)
      writeIORef startRef (addUTCTime dt cur)
      pure dt
    sleep time
    next

-- | Stop the thread for some time in seconds.
sleep :: MonadIO m => NominalDiffTime -> m ()
sleep dt = liftIO . D.delay $ toMicroseconds dt

-- | Convert time to microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds t = ceiling $ toRational t * 1000000

--------------------------------------------------------------------------------

-- | Substitutes values in event stream with random values.
toRandom :: forall m a b . (Frp m, Random b) => Evt m a -> Evt m b
toRandom evt = Evt $ \go -> do
  tv <- proxyNewRef evt =<< liftIO newStdGen
  runEvt evt $ \_ -> do
    (a, g) <- liftIO $ random <$> readRef tv
    go a
    liftIO $ writeRef tv g

-- | Substitutes values in event stream with random values from the given range.
toRandomR :: forall m a b . (Frp m, Random b) => (b, b) -> Evt m a -> Evt m b
toRandomR range evt = Evt $ \go -> do
  tv <- proxyNewRef evt =<< liftIO newStdGen
  runEvt evt $ \_ -> do
    (a, g) <- liftIO $ randomR range <$> readRef tv
    go a
    liftIO $ writeRef tv g

-- | Substitutes values in event stream with random values.
withRandom :: forall m a b . (Frp m, Random b) => Evt m a -> Evt m (b, a)
withRandom evt = Evt $ \go -> do
  tv <- proxyNewRef evt =<< liftIO newStdGen
  runEvt evt $ \x -> do
    (a, g) <- liftIO $ random <$> readRef tv
    go (a, x)
    liftIO $ writeRef tv g

-- | Substitutes values in event stream with random values from the given range.
withRandomR :: forall m a b . (Frp m, Random b) => (b, b) -> Evt m a -> Evt m (b, a)
withRandomR range evt = Evt $ \go -> do
  tv <- proxyNewRef evt =<< liftIO newStdGen
  runEvt evt $ \x -> do
    (a, g) <- liftIO $ randomR range <$> readRef tv
    go (a, x)
    liftIO $ writeRef tv g

-- | Picks at random one element from the list
oneOf :: Frp m => [a] -> Evt m b -> Evt m a
oneOf xs evt = listAt xs $ toRandomR (0, len - 1) evt
  where
    len = length xs

-- | Picks at random one element from the list
withOneOf :: Frp m => [a] -> Evt m b -> Evt m (a, b)
withOneOf xs evt = first (vec V.! ) <$> withRandomR (0, len - 1) evt
  where
    len = V.length vec
    vec = V.fromList xs

-- | Picks at random one element from the list. We also provide distribution of events.
-- Probability to pick up the element. Sum of probabilities should equal to 1.
freqOf :: (R.MonadRandom m, Frp m) => Dyn m [(a, Rational)] -> Evt m b -> Evt m a
freqOf dynVals evts = applyMay' ((\vals -> const (go vals)) <$> dynVals) evts
  where
    go vals = R.fromListMay vals

-- | Picks at random one element from the list. We also provide distribution of events.
-- Probability to pick up the element. Sum of probabilities should equal to 1.
withFreqOf :: (R.MonadRandom m, Frp m) => Dyn m [(a, Rational)] -> Evt m b -> Evt m (a, b)
withFreqOf dynVals evts = applyMay' (go <$> dynVals) evts
  where
    go vals x = fmap (fmap ((, x))) $ R.fromListMay vals

-- | Skips at random elements from the list. We provide frequency to skip events with dynamic first argument.
randSkip :: Frp m => Dyn m Double -> Evt m a -> Evt m a
randSkip prob evt = randSkipBy (const <$> prob) evt

-- | Skips elements at random. The probability to skip element depends on the element itself.
randSkipBy :: Frp m => Dyn m (a -> Double) -> Evt m a -> Evt m a
randSkipBy  prob evt = attachWithMay f prob $ withRandomR (0, 1 :: Double) evt
  where
    f getProb (curProb, a)
      | curProb < getProb a  = Nothing
      | otherwise            = Just a

--------------------------------------------------------------------------------

-- | Delays in the thread of execution. Note that it can interfere
-- and screw up functions like clock, timer, pulse, ticks
delay :: Frp m => NominalDiffTime -> Evt m a -> Evt m a
delay dt evt = Evt $ \go ->
  runEvt evt $ \x -> sleep dt >> go x

-- | Delays in background by forking on each event.
-- Note tht if delayed event was put into background prior
-- to stopping of the main event stream it will fire anyway.
-- There is no way to stop it.
delayFork :: Frp m => NominalDiffTime -> Evt m a -> Evt m a
delayFork dt evt = Evt $ \go ->
  runEvt evt $ \x -> void $ fork $ sleep dt >> go x

--------------------------------------------------------------------------------
-- effectful functor

class FunctorM f where
  fmap' :: Frp m => (a -> m b) -> f m a -> f m b

instance FunctorM Evt where
  fmap' f evt = Evt $ \go -> runEvt evt $ \x -> go =<< f x

instance FunctorM Dyn where
  fmap' f (ConstDyn a)                = Dyn f never (pure a) (pure ())
  fmap' f (Dyn extract evt s release) = Dyn (f <=< extract) evt s release

--------------------------------------------------------------------------------
-- Boolean instances

instance (Boolean b, Frp m) => Boolean (Dyn m b) where
  true = pure true
  false = pure false
  notB = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)

type instance BooleanOf (Dyn m a) = Dyn m (BooleanOf a)

instance (Frp m, IfB a) => IfB (Dyn m a) where
  ifB = liftA3 ifB

instance (EqB a, Frp m) => EqB (Dyn m a) where
  (==*) = liftA2 (==*)

instance (OrdB a, Frp m) => OrdB (Dyn m a) where
  (<*) = liftA2 (<*)
  (>*) = liftA2 (>*)
  (<=*) = liftA2 (<=*)
  (>=*) = liftA2 (>=*)

--------------------------------------------------------------------------------
-- Vector Space instances

instance (AdditiveGroup a, Frp m) => AdditiveGroup (Dyn m a) where
  zeroV = pure zeroV
  (^+^) = liftA2 (^+^)
  (^-^) = liftA2 (^-^)
  negateV = fmap negateV

instance (VectorSpace a, Frp m) => VectorSpace (Dyn m a) where
  type Scalar (Dyn m a) = Dyn m (Scalar a)
  (*^) = liftA2 (*^)

instance (AffineSpace p, Frp m) => AffineSpace (Dyn m p) where
  type Diff (Dyn m p) = Dyn m (Diff p)
  (.-.) = liftA2 (.-.)
  (.+^) = liftA2 (.+^)

class BasisArity v where
  basisArity :: v -> Int

instance BasisArity Float where
  basisArity _ = 1

instance BasisArity Double where
  basisArity _ = 1

instance (BasisArity a, BasisArity b) =>  BasisArity (a, b) where
  basisArity v = basisArity (proxyA v) + basisArity (proxyB v)
    where
      proxyA :: (a, b) -> a
      proxyA _ = undefined

      proxyB :: (a, b) -> b
      proxyB _ = undefined

instance (BasisArity a, BasisArity b, BasisArity c) =>  BasisArity (a, b, c) where
  basisArity v = basisArity (proxyA v) + basisArity (proxyB v) + basisArity (proxyC v)
    where
      proxyA :: (a, b, c) -> a
      proxyA _ = undefined

      proxyB :: (a, b, c) -> b
      proxyB _ = undefined

      proxyC :: (a, b, c) -> c
      proxyC _ = undefined

instance (Frp m, BasisArity v) => BasisArity (Dyn m v) where
  basisArity v = basisArity (proxy v)
    where
      proxy :: Dyn m v -> v
      proxy _ = undefined

instance (BasisArity v, HasBasis v, Frp m) => HasBasis (Dyn m v) where
  type Basis (Dyn m v) = Dyn m (Basis v)
  basisValue = fmap basisValue
  decompose v = fmap unTupleD $ unListD (basisArity v) $ fmap decompose v

  decompose' = liftA2 decompose'

unTupleD :: Frp m => Dyn m (a, b) -> (Dyn m a, Dyn m b)
unTupleD x = (fmap fst x, fmap snd x)

unListD :: Frp m => Int -> Dyn m [a] -> [Dyn m a]
unListD n ds = fmap (\a -> fmap ( !! a) ds) [0.. pred n]

instance (HasNormal v, Frp m) => HasNormal (Dyn m v) where
  normalVec = fmap normalVec

instance (HasCross2 v, Frp m) => HasCross2 (Dyn m v) where
  cross2 = fmap cross2

instance (HasCross3 v, Frp m) => HasCross3 (Dyn m v) where
  cross3 = liftA2 cross3

--------------------------------------------------------------------------------
-- Temporal media instances

instance Frp m => Melody (Evt m a) where
  (+:+) evtA evtB = Evt $ \go -> do
      runEvt evtA go
      runEvt evtB go

instance Frp m => Harmony (Evt m a) where
  (=:=) = (<>)

instance Frp m => Compose (Evt m a) where

instance Frp m => Loop (Evt m a) where
  loop evt = Evt $ \go -> forever (runEvt evt go)

-- | Takes an event and repeats it all the time.
forevers :: Frp m => Evt m a -> Evt m a
forevers evt = Evt $ \go -> forever (runEvt evt go)

type instance DurOf (Evt m a) = NominalDiffTime

instance Frp m => Limit (Evt m a) where
  lim t evt = Evt $ \go ->
    race_ (runEvt evt go) (sleep t)

--------------------------------------------------------------------------------
-- Parser

data St a = Final a | Cont a
  deriving (Functor)

data Parser m a b = forall s . Parser
  { parser'init   :: s
  , parser'modify :: (a -> s -> m (Maybe (St s)))
  , parser'get    :: s -> m (Maybe b)
  }

runParser :: Frp m => Parser m a b -> Evt m a -> m (Maybe b)
runParser (Parser init modify get) evt = do
  ref <- proxyNewRef evt (Cont init)

  waitAsync $ do
    runEvt evt $ \x -> do
      st <- liftIO $ readRef ref
      case st of
        Final s -> stopSelf
        Cont s  -> do
          mS' <- modify x s
          forM_ mS' $ \case
            Cont s  -> liftIO $ writeRef ref (Cont s)
            Final s -> liftIO $ do
              writeRef ref (Final s)
              stopSelf

  st <- liftIO (readRef ref)
  case st of
    Final s -> get s
    _       -> pure Nothing

heads :: Frp m => Evt m a -> m a
heads evt = do
  ref <- proxyNewRef evt Nothing
  waitAsync $ do
    runEvt evt $ \x -> do
      liftIO $ writeRef ref (Just x)
      stopSelf
  fromJust <$> liftIO (readRef ref)

-- | Reads single event
takeP :: Frp m => Parser m a b -> Evt m a -> Evt m b
takeP (Parser init modify get) evt = Evt $ \go -> do
  ref <- proxyNewRef evt init
  waitAsync $ do
    runEvt evt $ \x -> do
      s <- liftIO $ readRef ref
      mS' <- modify x s
      forM_ mS' $ \case
        Cont s'  -> liftIO $ writeRef ref s'
        Final s' -> do
          mapM_ go =<< get s'
          stopSelf

cycleP :: Frp m => Parser m a b -> Evt m a -> Evt m b
cycleP (Parser init modify get) evt = Evt $ \go -> do
  ref <- proxyNewRef evt init
  waitAsync $ do
    runEvt evt $ \x -> do
      s <- liftIO $ readRef ref
      mS' <- modify x s
      forM_ mS' $ \case
        Cont s'  -> liftIO $ writeRef ref s'
        Final s' -> do
          mapM_ go =<< get s'
          liftIO $ writeRef ref init

-- | Takes first element of the event stream and shuts the stream down.
headP :: Frp m => Parser m a a
headP = Parser init modify get
  where
    init = Nothing
    modify a _ = pure $ Just (Final (Just a))
    get = pure

maybeP :: Frp m => (a -> Maybe b) -> Parser m a b
maybeP f = Parser init modify get
  where
    init = Nothing
    modify a _ = pure $ fmap (Final . Just) $ f a
    get = pure

instance Frp m => Functor (Parser m a) where
  fmap f (Parser init modify get) = Parser init modify (fmap (fmap f) . get)

instance Frp m => Applicative (Parser m a) where
  pure a = Parser () (\_ _ -> pure (Just (Final ()))) (const $ pure $ Just a)
  (Parser initF modifyF getF) <*> (Parser initA modifyA getA) = Parser initRes modifyRes getRes
    where
      initRes = (Cont initF, Cont initA)

      modifyRes inp (sf, sa) = case sf of
        Cont f -> do
          mF' <- modifyF inp f
          pure $ fmap (Cont . (, sa)) mF'
        Final f ->
          case sa of
            Cont a -> do
              mA' <- modifyA inp a
              pure $ flip fmap mA' $ \case
                  Cont a'  -> Cont (Final f, Cont a')
                  Final a' -> Final (Final f, Final a')
            Final a -> pure (Just (Final (sf, sa)))

      getRes = \case
        (Final f, Final a) -> do
          mf <- getF f
          ma <- getA a
          pure (mf <*> ma)
        _                  -> pure Nothing


-- | Create a new Event and a function that will cause the Event to fire
newTriggerEvt :: (Frp m, MonadIO io) => m (Evt m a, a -> io ())
newTriggerEvt = do
  chan <- liftIO U.newChan
  pure (uchanEvt (fst chan), liftIO . U.writeChan (fst chan))

