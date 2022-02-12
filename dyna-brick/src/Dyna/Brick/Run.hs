-- | IO of thebrick application
module Dyna.Brick.Run(
  Spec(..),
  defSpec,
  emptyAttrMap,
  -- * Run application
  Run,
  runApp,
  -- * Sensors
  vtyEvents,
  mouseUp,
  mouseDown,
  keyEvents,
  onChar,
  onKey,
  readChars,
  module X,
  Key(..),
  Modifier(..),
) where


import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar qualified as M
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Concurrent.Chan.Unagi
import Control.Exception.Lifted
import Data.Default
import Data.Text (Text)

import Data.IORef
import Dyna qualified as D
import Brick as X
import Brick.Main (continueWithoutRedraw)
import Brick.BChan qualified as Brick
import qualified Graphics.Vty as Vty
import Graphics.Vty (Key(..), Modifier, Button)

import Dyna.Brick.Types

data Spec = Spec
  { spec'attrMap :: AttrMap
  , spec'cursor  :: [CursorLocation BoxId] -> Maybe (CursorLocation BoxId)
  }

instance Default Spec where
  def = defSpec emptyAttrMap

defSpec :: AttrMap -> Spec
defSpec attrs = Spec attrs (const Nothing)

emptyAttrMap :: AttrMap
emptyAttrMap = attrMap Vty.defAttr []

--------------------------------------------------------------------------------
-- run application

-- | Run application
runApp :: Spec -> Run Win -> IO ()
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
  let evs = (\(Win dyn acts) -> (UpdateWidgets <$> D.unhold (unDyn dyn)) <> (BrickAct <$> unEvt acts)) <$> dynActs
  tid <- forkIO $ evalRun ((\e -> D.runEvt e (liftIO . Brick.writeBChan actChan)) =<< evs) env
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

--------------------------------------------------------------------------------
-- event sensors

vtyEvents :: Evt Vty.Event
vtyEvents = Evt $ D.uchanEvt $ fst <$> asks env'eventChan

mouseDown :: Evt MouseDownEvent
mouseDown = Evt $ D.uchanEvt $ fst <$> asks env'mouseDownChan

mouseUp :: Evt MouseUpEvent
mouseUp = Evt $ D.uchanEvt $ fst <$> asks env'mouseUpChan

keyEvents :: Evt (Key, [Modifier])
keyEvents = Evt $ D.mapMay go (unEvt vtyEvents)
  where
    go = \case
      Vty.EvKey key mods -> Just (key, mods)
      _                  -> Nothing

onChar :: Char -> Evt [Modifier]
onChar ch = onKey (KChar ch)

readChars :: Evt Char
readChars = Evt $ D.mapMay go (unEvt keyEvents)
  where
    go x = case fst x of
      KChar ch -> Just ch
      _        -> Nothing

onKey :: Key -> Evt [Modifier]
onKey k = Evt $
  D.mapMay
    (\(x, mods) -> if x == k then Just mods else Nothing)
    (unEvt keyEvents)

