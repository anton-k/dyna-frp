-- | Simple point painter
--
-- We can change the color of the pointer with left clicks of the mouse
-- And we can make a trace with right click of the mouse
module Main where

import Dyna.Proc
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Screen specification
spec :: Spec
spec = Spec $
  size (P2 700 700)

-- | Ball: color, position
data Ball = Ball Col P2

-- | Draw a Ball
draw :: Ball -> Draw
draw (Ball col pos) = do
  strokeFill col
  circle 25 pos

-- | Balls are saved to the sequence and redrawn on each frame also we draw the pointer.
-- Note that order of storage affects the drawing. Later pictures go in the foreground.
balls :: Dyn (Seq Ball)
balls =
  liftA2 (Seq.|>)
    (scanD (flip (Seq.|>)) Seq.empty $ snap pointer mouseRight)
    pointer

-- | Ball at the mouse position
pointer :: Dyn Ball
pointer = liftA2 Ball ballCol mouse

-- | Cols alterate on mouse left clicks
ballCol :: Dyn Col
ballCol = hold green $ cycles [red, green] mouseLeft

{-
-- | Count the mouse clicks so far (so many balls we have placed on the screen)
countBalls :: Dyn Picture
countBalls = text . show <$> (hold 0 $ count mouseRight)
-}

-- | Main app
main = runApp spec $ pure $
  (mappend initDraw . foldMap draw <$> balls)  -- <> countBalls
  where
    initDraw = background white

