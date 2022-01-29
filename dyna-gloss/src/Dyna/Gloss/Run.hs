-- | Run the game application.
module Dyna.Gloss.Run(
  -- * App execution
  Run,
  Spec(..),
  defSpec,
  runApp,
  -- * IO interface
  mouse,
  mouseV,
  isDrag,
  drag,
  dragV,
  mouseA,
  mouseRight,
  mouseLeft,
  mouseWheel,
  getClicks,
  getFrames,
  -- * Re-exports
  Key(..),
  SpecialKey(..),
  MouseButton(..),
  KeyState(..),
  Modifiers(..),
) where

import Control.Exception.Lifted
import Control.Concurrent.Chan.Unagi qualified as U
import Control.Monad.Reader
import Data.IORef

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Dyna qualified as D
import Dyna.Gloss.Types
import Dyna.Gloss.Data.Vec

-- | Initial parameters for the Game.
data Spec = Spec
  { spec'display    :: Display   -- ^ display settings
  , spec'background :: Color     -- ^ background color to clear each frame
  , spec'steps      :: Int       -- ^ number of steps for simulation
  }

-- | Default settings. Runs in ullscreen mode.
defSpec :: Spec
defSpec = Spec FullScreen white 1

-- | Run the aplication. It accepts initial settings
-- and the dynamic value of pictures wrapped in the Run monad.
--
-- Note that to work properly we need to compile to executable with
-- options -O2 and -threaded. The function does not work in ghci or with runhaskell
-- because it requires support for multiple threads.
--
-- Define the application with the Main module. Then compie it:
--
-- > stack exec -- ghc -O2 -threaded dyna-gloss/examples/Ball.hs
--
-- And run the result:
--
-- > 	./dyna-gloss/examples/Ball
--
-- How it works? It runs the dynamic process at the background thread and
-- every time the gloss function requests new frame it takes a snapshot of the current
-- value of the main dynamic process which produces pictures. It's exactly what gloss
-- simulation function needs to render it on the screen.
runApp :: Spec -> Run (Dyn Picture) -> IO ()
runApp Spec{..} dynAct = do
  env <- newEnv
  ref <- runReaderT (unRun $ D.runDyn . unDyn =<< dynAct) env
  playIO spec'display spec'background spec'steps ref (draw env) (onEvents env) (onIterate env)
    `finally` (runReaderT (unRun $ D.cancelDyn ref) env)
  where
    draw env ref = runReaderT (unRun $ D.readDyn ref) env

    onEvents Env{..} evt ref = do
      case evt of
        EventResize sizes      -> U.writeChan (fst env'resizeChan) sizes
        EventMotion (x, y)     -> do
          let pos = Vec x y
          prevPos  <- readIORef env'mousePos
          prevDif1 <- readIORef env'mouseDif1
          let dif1 = pos - prevPos
          writeIORef env'mousePos pos
          writeIORef env'mouseDif1 dif1
          writeIORef env'mouseDif2 (dif1 - prevDif1)
        EventKey k st mods pos -> U.writeChan (fst env'keyChan) (Click k st mods (fromTuple pos))
      pure ref

    onIterate Env{..} time ref = do
      U.writeChan (fst env'frameChan) time
      pure ref

----------------------------------------------------------------

-- | Read mouse positions. It produces dynamic of vectors. @(0, 0)@ is a center of the screen.
mouse :: Dyn Vec
mouse = Dyn $ D.constDyn $ do
  ref <- asks env'mousePos
  liftIO $ readIORef ref

-- | Mouse velocity or displacement
mouseV :: Dyn Vec
mouseV = Dyn $ D.constDyn $ do
  ref <- asks env'mouseDif1
  liftIO $ readIORef ref

-- | Mouse accelartion or speed of displacement
mouseA :: Dyn Vec
mouseA = Dyn $ D.constDyn $ do
  ref <- asks env'mouseDif2
  liftIO $ readIORef ref

isDrag :: MouseButton -> Dyn Bool
isDrag btn = Dyn $ D.foldD collect False $ D.mapMaybe go $ unEvt getClicks
  where
    go (Click key st mods pos) = case key of
      MouseButton mbtn | mbtn == btn -> Just st
      _                              -> Nothing

    collect a st = case a of
      Up   -> False
      Down -> True


-- | Displacement on drag, if no drag it becomes zero
dragV :: MouseButton -> Dyn Vec
dragV btn = (\x -> if x then id else const 0) <$> isDrag btn <*> mouseV

-- | Position of the mouse during drag, if no drag it becomes zero
drag :: MouseButton -> Dyn Vec
drag btn = (\x -> if x then id else const 0) <$> isDrag btn <*> mouse

-- | Event stream of clicks of the mouse right button
mouseRight :: Evt Vec
mouseRight = Evt $ D.mapMaybe go $ unEvt getClicks
  where
    go (Click key st mods pos) = case key of
      MouseButton RightButton | st == Down -> Just pos
      _                                    -> Nothing

-- | Event stream of clicks of the mouse left button
mouseLeft :: Evt Vec
mouseLeft = Evt $ D.mapMaybe go $ unEvt getClicks
  where
    go (Click key st mods pos) = case key of
      MouseButton LeftButton | st == Down -> Just pos
      _                                   -> Nothing

-- | Mouse wheel displacement.
-- If positive then it goes up, if negative then it goes down.
mouseWheel :: Evt Float
mouseWheel = Evt $ D.mapMaybe go $ unEvt getClicks
  where
    go (Click key st mods pos) = case key of
      MouseButton WheelUp   -> Just 1
      MouseButton WheelDown -> Just (-1)
      _                     -> Nothing

-- | Reads generic click events
getClicks :: Evt Click
getClicks = Evt $ D.uchanEvt $ fst <$> asks env'keyChan

-- | Reads frame updates. Value of the event is a time that has passed since the previous frame.
--
-- Note that if we want to use the sort of event stream as a timeline for the game or simulation
-- we can also use time utilities from the FRP library: @clock@, @pulse@, @ticks@, @timer@.
getFrames :: Evt Float
getFrames = Evt $ D.uchanEvt $ fst <$> asks env'frameChan

