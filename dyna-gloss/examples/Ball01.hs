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

-- runApp :: Spec -> Dyn Picture -> IO ()

-- | Ball at the mouse position
pointer :: Dyn Ball
pointer = fmap (Ball green) mouse

-- | Main app
main = runApp spec $ pure $ fmap draw pointer


