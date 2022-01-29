-- | Simple point painter
--
-- We can change the color of the pointer with left clicks of the mouse
-- And we can make a trace with right click of the mouse
module Main where

import Dyna.Gloss
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Screen specification
spec :: Spec
spec = defSpec { spec'display = InWindow "Main" (700, 700) (50, 50) }

-- | Ball: color, position
data Ball = Ball Color Vec

-- | Draw a Ball
draw :: Ball -> Picture
draw (Ball col pos) = translate pos $ color col $ circleSolid 25

-- | Balls are saved to the sequence and redrawn on each frame also we draw the pointer.
-- Note that order of storage affects the drawing. Later pictures go in the foreground.
balls :: Dyn (Seq Ball)
balls =
  liftA2 (Seq.|>)
    (foldD (flip (Seq.|>)) Seq.empty $ snap pointer mouseRight)
    pointer

-- | Ball at the mouse position
pointer :: Dyn Ball
pointer = liftA2 Ball ballColor mouse

-- | Colors alterate on mouse left clicks
ballColor :: Dyn Color
ballColor = hold green $ cycleE [red, green] mouseLeft

-- | Count the mouse clicks so far (so many balls we have placed on the screen)
countBalls :: Dyn Picture
countBalls = text . show <$> (hold 0 $ count mouseRight)

-- | Main app
main = runApp spec $ pure $
  (foldMap draw <$> balls) <> countBalls


