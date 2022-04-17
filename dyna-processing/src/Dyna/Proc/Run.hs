module Dyna.Proc.Run(
  -- * Run the app
  runApp,
  Spec(..),
  -- * User interaction
  timeInterval,
  getClicks,
  mouse,
  mouseV,
  mouseA,
  relMouse,
  mouseRight,
  mouseLeft,
  mouseMiddle,
  mouseAdditional,
  mouseWheel,
  isDrag,
  drag,
  dragV,
  keyUp,
  keyDown,
  charUp,
  charDown,
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Dyna as D
import Dyna.Proc.Types
import Graphics.Proc (
  runProc, Proc(..), Default(..), Pio, Draw,
  P2, MouseButton(..), Key(..), Modifiers
 )
import qualified Graphics.Proc as P
import Control.Concurrent.Chan.Unagi qualified as U

-- | Initialisation of the application.
-- It's processing setup procedure that is called
-- once at the app start up.
newtype Spec = Spec
  { spec'setup :: Pio ()
  }

runApp :: Spec -> Run (Dyn Draw) -> IO ()
runApp Spec{..} mDyn = do
  env <- newEnv
  ref <- runRun (D.runDyn . unDyn =<< mDyn) env
  runProc def
    { procSetup  = spec'setup >> pure ref
    , procDraw   = draw env
    , procUpdate = pure
    , procUpdateTime    = \t -> through $ updateTime env t
    , procMousePressed  = through $ mouseBy Down env
    , procMouseReleased = through $ mouseBy Up env
    , procKeyPressed    = through $ keyBy Down env
    , procKeyReleased   = through $ keyBy Up env
    }
  where
    through act st = act >> pure st

    draw env ref = join $ liftIO $ runRun (D.readDyn ref) env

    updateTime Env{..} t = do
      -- mouse input
      pos      <- P.mouse
      relPos   <- P.relMouse
      liftIO $ do
        prevPos  <- readIORef env'mousePos
        prevDef1 <- readIORef env'mouseDif1
        let dif1 = pos - prevPos
        writeIORef env'mousePos pos
        writeIORef env'mouseDif1 dif1
        writeIORef env'mouseDif2 (dif1 - prevDef1)
        writeIORef env'relMousePos relPos
        writeIORef env'time t

    mouseBy keySt Env{..} = do
      mBut <- P.mouseButton
      forM_ mBut $ \but -> do
        mods <- P.modifiers
        pos  <- P.mouse
        let ev = Click (Right but) keySt mods pos
        liftIO $ U.writeChan (fst env'clicks) ev

    keyBy keySt Env{..} = do
      k <- P.key
      mods <- P.modifiers
      pos  <- P.mouse
      let ev = Click (Left k) keySt mods pos
      liftIO $ U.writeChan (fst env'clicks) ev

-------------------------------------------------------------------------------------
-- user input

-- | Time that has passed since previous step of simulation
timeInterval :: Dyn Float
timeInterval = Dyn $ D.constDyn (liftIO . readIORef =<< asks env'time)

-- | Mouse position
mouse :: Dyn P2
mouse = Dyn $ D.constDyn (liftIO . readIORef =<< asks env'mousePos)

-- | Relative mouse position (if the whole screen is in the range [0, 1])
relMouse :: Dyn P2
relMouse = Dyn $ D.constDyn (liftIO . readIORef =<< asks env'relMousePos)

-- | Mouse velocity
mouseV :: Dyn P2
mouseV = Dyn $ D.constDyn (liftIO . readIORef =<< asks env'mouseDif1)

-- | Mouse acceleration
mouseA :: Dyn P2
mouseA = Dyn $ D.constDyn (liftIO . readIORef =<< asks env'mouseDif2)

-- | Reads generic click events
getClicks :: Evt Click
getClicks = Evt $ D.Evt $ \go -> do
  clickChan <- asks env'clicks
  D.runEvt (D.uchanEvt $ fst clickChan) go

-- | Generic mouse click event
mouseButton :: MouseButton -> Evt P2
mouseButton btn' = Evt $ D.mapMay go $ unEvt getClicks
  where
    go = \case
      Click (Right btn) Down mods pos | btn == btn' -> Just pos
      _                                             -> Nothing

-- | Event stream of clicks of the mouse right button
mouseRight :: Evt P2
mouseRight = mouseButton RightButton

-- | Event stream of clicks of the mouse left button
mouseLeft :: Evt P2
mouseLeft = mouseButton LeftButton

-- | Event stream of clicks of the mouse middle button
mouseMiddle :: Evt P2
mouseMiddle = mouseButton MiddleButton

-- | Event stream of clicks of the mouse additional button
mouseAdditional :: Int -> Evt P2
mouseAdditional n = mouseButton (AdditionalButton n)

-- | Mouse wheel displacement.
-- If positive then it goes up, if negative then it goes down.
mouseWheel :: MouseButton -> Evt Float
mouseWheel btn' = Evt $ D.mapMay go $ unEvt getClicks
  where
    go = \case
      Click (Right WheelUp)   Down _ _ -> Just 1
      Click (Right WheelDown) Down _ _ -> Just (-1)
      _                                -> Nothing

-- | Event stream of key up actions
keyUp :: Key -> Evt Modifiers
keyUp = keyBy Up

-- | Event stream of key down actions
keyDown :: Key -> Evt Modifiers
keyDown = keyBy Down

-- | Event stream of char press up actions
charUp :: Char -> Evt Modifiers
charUp = keyUp . Char

-- | Event stream of char press down actions
charDown :: Char -> Evt Modifiers
charDown = keyDown . Char

keyBy :: KeyState -> Key -> Evt Modifiers
keyBy st' key' = Evt $ D.mapMay go $ unEvt getClicks
  where
    go (Click (Left key) st mods _)
      | key == key' && st == st' = Just mods
      | otherwise                = Nothing

isDrag :: MouseButton -> Dyn Bool
isDrag btn = Dyn $ D.scanD collect False $ D.mapMay go $ unEvt getClicks
  where
    go = \case
      Click (Right mbtn) st _ _ | mbtn == btn -> Just st
      _                                       -> Nothing

    collect a st = case a of
      Up   -> False
      Down -> True

-- | Displacement on drag, if no drag it becomes zero
dragV :: MouseButton -> Dyn P2
dragV btn = (\x -> if x then id else const 0) <$> isDrag btn <*> mouseV

-- | Position of the mouse during drag, if no drag it becomes zero
drag :: MouseButton -> Dyn P2
drag btn = (\x -> if x then id else const 0) <$> isDrag btn <*> mouse
