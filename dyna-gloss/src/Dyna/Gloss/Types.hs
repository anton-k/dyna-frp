-- | Types of the gloss application
module Dyna.Gloss.Types(
  Run(..),
  Evt(..),
  Dyn(..),
  Env(..),
  newEnv,
  Click(..),
) where

import Prelude hiding ((<*))
import Control.Applicative (liftA2, liftA3)
import Control.Concurrent.Chan.Unagi
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Random.Class
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.IORef
import Data.String
import Data.Boolean
import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Basis
import Data.Cross
import Temporal.Class

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Dyna qualified as D
import Dyna.Gloss.Data.Vec

-- | Monad that drives the application
newtype Run a = Run { unRun :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadBase IO, MonadRandom)

newtype StMRun a = StMRun { unStMRun :: StM (ReaderT Env IO) a }

instance MonadBaseControl IO Run where
    type StM Run a = StMRun a
    liftBaseWith f = Run $ liftBaseWith $ \q -> f (fmap StMRun . q . unRun)
    restoreM = Run . restoreM . unStMRun

instance D.Frp Run where
  type Ref Run = IORef

-------------------------------------------------------------------------------
-- FRP type wrappers

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
newtype Evt a = Evt { unEvt :: D.Evt Run a }
  deriving (Functor, Semigroup, Monoid, Applicative, Monad, Melody, Harmony, Compose, Loop)

type instance DurOf (Evt a) = Float

instance Limit (Evt a) where
  lim t (Evt evt) = Evt $ lim (realToFrac t) evt

-- | Dynamic step-wise continuous process
newtype Dyn a = Dyn { unDyn :: D.Dyn Run a }
  deriving (Functor, Applicative, Num, Fractional, Semigroup, Monoid, IsString,
            Boolean, AdditiveGroup, VectorSpace, HasNormal, HasCross2, HasCross3, AffineSpace, HasBasis)

type instance BooleanOf (Dyn a) = Dyn (BooleanOf a)

instance (IfB a) => IfB (Dyn a) where
  ifB = liftA3 ifB

instance (EqB a) => EqB (Dyn a) where
  (==*) = liftA2 (==*)

instance (OrdB a) => OrdB (Dyn a) where
  (<*) = liftA2 (<*)
  (>*) = liftA2 (>*)
  (<=*) = liftA2 (<=*)
  (>=*) = liftA2 (>=*)

-------------------------------------------------------------------------------

-- | All sorts of clicks
data Click = Click Key KeyState Modifiers Vec

-- | Applicaition environment
data Env = Env
  { env'frameChan   :: D.UChan Float
  , env'eventChan   :: D.UChan Event
  , env'resizeChan  :: D.UChan (Int, Int)
  , env'keyChan     :: D.UChan Click
  , env'mousePos    :: IORef Vec
  , env'mouseDif1   :: IORef Vec
  , env'mouseDif2   :: IORef Vec
  }

-- | Create new environment
newEnv :: IO Env
newEnv = do
  env'frameChan <- newChan
  env'eventChan <- newChan
  env'resizeChan <- newChan
  env'keyChan <- newChan
  env'mousePos <- newIORef (Vec 0 0)
  env'mouseDif1 <- newIORef (Vec 0 0)
  env'mouseDif2 <- newIORef (Vec 0 0)
  pure Env{..}

