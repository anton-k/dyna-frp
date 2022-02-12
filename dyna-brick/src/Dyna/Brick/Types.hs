-- | Brick application types
module Dyna.Brick.Types(
  -- * Brick helper types
  Box,
  BoxId(..),
  Win(..),
  Act(..),
  MouseUpEvent(..),
  MouseDownEvent(..),
  -- * Brick application monad
  Run(..),
  evalRun,
  -- * FRP types
  Evt(..),
  Dyn(..),
  -- * Internal types
  Env(..),
  newEnv,
  InternalEvent(..),
) where

import Prelude hiding ((<*))
import Control.Applicative (liftA2, liftA3)
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Random.Class
import Control.Concurrent.Chan.Unagi
import Data.Text (Text)
import Data.Boolean
import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Basis
import Data.Cross
import Data.String
import Temporal.Class

import Data.IORef
import Dyna qualified as D
import Brick
import qualified Graphics.Vty as Vty
import Graphics.Vty (Key(..), Modifier, Button)

----------------------------------------------------------------------------------
-- brick helper types

-- | Synonym to fix the Widget parameter
type Box = Widget BoxId

-- | Box identifier
newtype BoxId = BoxId { getBoxId :: Text }
  deriving (Show, Eq, Ord, Read)

-- | Actions for Brick rendering engine
data Act
  = Quit -- ^ Quit the app

-- | Window of the application
data Win = Win
  { win'widgets :: Dyn [Box]   -- ^ window view
  , win'acts    :: Evt Act     -- ^ brick app actions
  }

-- | Mouse down events only
data MouseDownEvent = MouseDownEvent BoxId Button [Modifier] Location

-- | Mouse up events only
data MouseUpEvent = MouseUpEvent BoxId (Maybe Button) Location

----------------------------------------------------------------------------------
-- Brick application monad

newtype Run a = Run { unRun :: ReaderT Env IO a }
  deriving ( Functor, Applicative, Monad, MonadReader Env, MonadIO
           , MonadBase IO, MonadRandom)

evalRun :: Run a -> Env -> IO a
evalRun (Run act) env = runReaderT act env

newtype StMRun a =
  StMRun { unStMRun :: StM (ReaderT Env IO) a }

instance MonadBaseControl IO Run where
    type StM Run a = StMRun a
    liftBaseWith f =
      Run $ liftBaseWith $ \q -> f (fmap StMRun . q . unRun)
    restoreM = Run . restoreM . unStMRun

instance D.Frp Run where
  type Ref Run = IORef

----------------------------------------------------------------------------------
-- FRP types

-- | Event stream
newtype Evt a = Evt { unEvt :: D.Evt Run a }
  deriving (Functor, Semigroup, Monoid, Applicative, Monad, Melody, Harmony, Compose, Loop)

type instance DurOf (Evt a) = Float

instance Limit (Evt a) where
  lim t (Evt evt) = Evt $ lim (realToFrac t) evt


-- | Dynamic value that continuously changes over time
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

----------------------------------------------------------------------------------
-- internal types

-- | Environment for execution of Brick and routing events to sensor streams
data Env = Env
  { env'eventChan      :: D.UChan Vty.Event
  , env'mouseDownChan  :: D.UChan MouseDownEvent
  , env'mouseUpChan    :: D.UChan MouseUpEvent
  }

-- | Init new environment
newEnv :: IO Env
newEnv = Env <$> newChan <*> newChan <*> newChan

-- | Internal events to interact with brock application
data InternalEvent
  = UpdateWidgets [Box]
     -- ^ update the view
  | BrickAct Act
      -- ^ send command to brick

