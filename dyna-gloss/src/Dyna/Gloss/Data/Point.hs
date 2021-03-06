module Dyna.Gloss.Data.Point
        ( Point, Path
        , pointInBox)
where

import Dyna.Gloss.Data.Vec

type Point = Vec
type Path = [Point]

-- | Test whether a point lies within a rectangular box that is oriented
--   on the x-y plane. The points P1-P2 are opposing points of the box,
--   but need not be in a particular order.
--
-- @
--    P2 +-------+
--       |       |
--       | + P0  |
--       |       |
--       +-------+ P1
-- @
--
pointInBox :: Point -> Point -> Point -> Bool
pointInBox (Vec x0 y0) (Vec x1 y1) (Vec x2 y2)
  =  x0 >= min x1 x2
  && x0 <= max x1 x2
  && y0 >= min y1 y2
  && y0 <= max y1 y2


