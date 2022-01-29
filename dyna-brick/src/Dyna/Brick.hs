module Dyna.Brick(
  Spec(..),
  defSpec,
  Win(..),
  emptyAttrMap,
  Act(..),
  runApp,
  Evt,
  Dyn,
  vtyEvents,
  keyEvents,
  readChars,
  onChar,
  onKey,
  mouseDown,
  mouseUp,
  module X,
  Key(..),
  Modifier(..),
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar qualified as M
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.Chan.Unagi
import Control.Exception.Lifted

import Data.IORef
import Dyna qualified as D
import Brick as X
import Brick.Main (continueWithoutRedraw)
import Brick.BChan qualified as Brick
import qualified Graphics.Vty as Vty
import Graphics.Vty (Key(..), Modifier, Button)


type Chan a = (InChan a, OutChan a)

data Env n = Env
  { env'eventChan      :: Chan Vty.Event
  , env'mouseDownChan  :: Chan (MouseDownEvent n)
  , env'mouseUpChan    :: Chan (MouseUpEvent n)
  }

data MouseDownEvent n = MouseDownEvent n Button [Modifier] Location
data MouseUpEvent n = MouseUpEvent n (Maybe Button) Location

data Ev n
  = UpdateWidgets [Widget n]
  | BrickAct Act

newEnv :: IO (Env n)
newEnv = Env <$> newChan <*> newChan <*> newChan

newtype BrickM n a = BrickM { unBrickM :: ReaderT (Env n) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (Env n), MonadIO, MonadBase IO)

runBrickM :: BrickM n a -> Env n -> IO a
runBrickM (BrickM act) env = runReaderT act env

newtype StMBrickM n a = StMBrickM { unStMBrickM :: StM (ReaderT (Env n) IO) a }

instance MonadBaseControl IO (BrickM n) where
    type StM (BrickM n) a = StMBrickM n a
    liftBaseWith f = BrickM $ liftBaseWith $ \q -> f (fmap StMBrickM . q . unBrickM)
    restoreM = BrickM . restoreM . unStMBrickM

instance D.Frp (BrickM n) where
  type Ref (BrickM n) = IORef

type Evt n a = D.Evt (BrickM n) a
type Dyn n a = D.Dyn (BrickM n) a


data Spec n = Spec
  { spec'attrMap :: AttrMap
  , spec'cursor  :: [CursorLocation n] -> Maybe (CursorLocation n)
  }

defSpec :: AttrMap -> Spec n
defSpec attrs = Spec attrs (const Nothing)

emptyAttrMap :: AttrMap
emptyAttrMap = attrMap Vty.defAttr []

data Act = Quit

data Win n = Win
  { win'widgets :: Dyn n [Widget n]
  , win'acts    :: Evt n Act
  }

runApp :: Ord n => Spec n -> BrickM n (Win n) -> IO ()
runApp Spec{..} dynActs = do
  env <- newEnv
  actChan <- Brick.newBChan 10
  let app = App
        { appDraw         = id
        , appChooseCursor = const spec'cursor
        , appHandleEvent  = handleEvent env
        , appStartEvent   = pure
        , appAttrMap      = const spec'attrMap
        }
  let evs = (\(Win dyn acts) -> (UpdateWidgets <$> D.unhold dyn) <> (BrickAct <$> acts)) <$> dynActs
  tid <- forkIO $ runBrickM ((\e -> D.runEvt e (liftIO . Brick.writeBChan actChan)) =<< evs) env
  runChanMain actChan app
    `finally` killThread tid
  where
    handleEvent env@Env{..} st evt = case evt of
      VtyEvent event -> do
        liftIO $ writeChan (fst env'eventChan) event
        continueWithoutRedraw st
      AppEvent act -> case act of
        UpdateWidgets ws -> continue ws
        BrickAct act     ->
          case act of
            Quit -> halt st
      MouseDown n but mods loc -> do
        liftIO $ writeChan (fst env'mouseDownChan) (MouseDownEvent n but mods loc)
        continueWithoutRedraw st
      MouseUp n mBut loc       -> do
        liftIO $ writeChan (fst env'mouseUpChan) (MouseUpEvent n mBut loc)
        continueWithoutRedraw st

    runChanMain chan app = do
      let buildVty = Vty.mkVty Vty.defaultConfig
      initialVty <- buildVty
      void $ customMain initialVty buildVty (Just chan) app [emptyWidget]

vtyEvents :: Evt n Vty.Event
vtyEvents = D.uchanEvt $ fst <$> asks env'eventChan

mouseDown :: Evt n (MouseDownEvent n)
mouseDown = D.uchanEvt $ fst <$> asks env'mouseDownChan

mouseUp :: Evt n (MouseUpEvent n)
mouseUp = D.uchanEvt $ fst <$> asks env'mouseUpChan

keyEvents :: Evt n (Key, [Modifier])
keyEvents = D.mapMaybe go vtyEvents
  where
    go = \case
      Vty.EvKey key mods -> Just (key, mods)
      _                  -> Nothing

onChar :: Char -> Evt n [Modifier]
onChar ch = onKey (KChar ch)

readChars :: Evt n Char
readChars = D.mapMaybe go keyEvents
  where
    go x = case fst x of
      KChar ch -> Just ch
      _        -> Nothing

onKey :: Key -> Evt n [Modifier]
onKey k = D.mapMaybe (\(x, mods) -> if x == k then Just mods else Nothing) keyEvents

