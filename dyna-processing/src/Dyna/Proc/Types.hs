module Dyna.Proc.Types where

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

import qualified Dyna as D
import Graphics.Proc hiding ((<*))

-- | Monad that drives the application
newtype Run a = Run { unRun :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadReader Env,
            MonadIO, MonadBase IO, MonadRandom)

newtype StMRun a = StMRun { unStMRun :: StM (ReaderT Env IO) a }

instance MonadBaseControl IO Run where
    type StM Run a = StMRun a
    liftBaseWith f = Run $ liftBaseWith $ \q -> f (fmap StMRun . q . unRun)
    restoreM = Run . restoreM . unStMRun

instance D.Frp Run where
  type Ref Run = IORef

runRun :: Run a -> Env -> IO a
runRun (Run act) env = runReaderT act env

------------------------------------------------------------------------------------

-- | Event streams
newtype Evt a = Evt { unEvt :: D.Evt Run a }
  deriving (Functor, Semigroup, Monoid, Applicative, Monad,
            Melody, Harmony, Compose, Loop)

type instance DurOf (Evt a) = Float

instance Limit (Evt a) where
  lim t (Evt evt) = Evt $ lim (realToFrac t) evt

-- | Dynamic values (step-wise continuous process)
newtype Dyn a = Dyn { unDyn :: D.Dyn Run a }
  deriving (Functor, Applicative, Num, Fractional, Semigroup, Monoid, IsString,
            Boolean, AdditiveGroup, VectorSpace, HasNormal, HasCross2, HasCross3,
            AffineSpace, HasBasis)

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

------------------------------------------------------------------------------------
-- environment

-- | All sorts of clicks
data Click = Click (Either Key MouseButton) KeyState Modifiers P2

data KeyState = Up | Down
  deriving (Show, Eq)

-- | Applicaition environment
data Env = Env
  { env'resizeChan  :: D.UChan (Int, Int)
  , env'keyChan     :: D.UChan Key
  , env'relMousePos :: IORef P2
  , env'mousePos    :: IORef P2
  , env'mouseDif1   :: IORef P2
  , env'mouseDif2   :: IORef P2
  , env'time        :: IORef Float
  , env'clicks      :: D.UChan Click
  }

-- | Create new environment
newEnv :: IO Env
newEnv = do
  env'resizeChan <- newChan
  env'clicks <- newChan
  env'keyChan <- newChan
  env'time <- newIORef 0
  env'relMousePos <- newIORef (P2 0 0)
  env'mousePos <- newIORef (P2 0 0)
  env'mouseDif1 <- newIORef (P2 0 0)
  env'mouseDif2 <- newIORef (P2 0 0)
  pure Env{..}

