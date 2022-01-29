module Dyna.Gloss.Data.Picture
        ( Picture       (..)
        , Point, Vec, Path

        -- * Aliases for Picture constructors
        , blank
        , polygon
        , line
        , circle, thickCircle
        , arc,    thickArc
        , text
        , bitmap
        , bitmapSection
        -- , bitmap
        , color
        , translate, rotate, scale
        , pictures

        -- * Compound shapes
        , lineLoop
        , circleSolid
        , arcSolid
        , sectorWire
        , rectanglePath
        , rectangleWire
        , rectangleSolid
        , rectangleUpperPath
        , rectangleUpperWire
        , rectangleUpperSolid
        )
where

import Data.VectorSpace
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Geometry.Angle
import Dyna.Gloss.Data.Point
import Dyna.Gloss.Data.Vec

-- Constructors ----------------------------------------------------------------
-- NOTE: The docs here should be identical to the ones on the constructors.

-- | A blank picture, with nothing in it.
blank :: Picture
blank   = Blank

-- | A convex polygon filled with a solid color.
polygon :: Path -> Picture
polygon = Polygon . fmap toTuple

-- | A line along an arbitrary path.
line :: Path -> Picture
line = Line . fmap toTuple

-- | A circle with the given radius.
circle  :: Float  -> Picture
circle  = Circle

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `Circle`.
thickCircle  :: Float -> Float -> Picture
thickCircle = ThickCircle

-- | A circular arc drawn counter-clockwise between two angles (in degrees)
--   at the given radius.
arc     :: Float -> Float -> Float -> Picture
arc = Arc

-- | A circular arc drawn counter-clockwise between two angles (in degrees),
--   with the given radius  and thickness.
--   If the thickness is 0 then this is equivalent to `Arc`.
thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = ThickArc

-- | Some text to draw with a vector font.
text :: String -> Picture
text = Text

-- | A bitmap image
bitmap  :: BitmapData -> Picture
bitmap bitmapData = Bitmap bitmapData

-- | a subsection of a bitmap image
--   first argument selects a sub section in the bitmap
--   second argument determines the bitmap data
bitmapSection  :: Rectangle -> BitmapData -> Picture
bitmapSection = BitmapSection

-- | A picture drawn with this color.
color :: Color -> Picture -> Picture
color = Color

-- | A picture translated by the given x and y coordinates.
translate :: Vec -> Picture -> Picture
translate (Vec x y) = Translate x y

-- | A picture rotated clockwise by the given angle (in tau's).
-- 1 Tau is full circle.
rotate  :: Float -> Picture -> Picture
rotate x = Rotate (360 * x)

-- | A picture scaled by the given x and y factors.
scale   :: Vec -> Picture -> Picture
scale (Vec x y) = Scale x y

-- | A picture consisting of several others.
pictures :: [Picture] -> Picture
pictures = Pictures


-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop xs     = line $ case xs of
   []     -> []
   (x:xs) -> (x:xs) ++ [x]


-- Circles and Arcs -----------------------------------------------------------
-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r
        = thickCircle (r/2) r


-- | A solid arc, drawn counter-clockwise between two angles at the given radius.
arcSolid  :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r
        = thickArc a1 a2 (r/2) r


-- | A wireframe sector of a circle.
--   An arc is draw counter-clockwise from the first to the second angle at
--   the given radius. Lines are drawn from the origin to the ends of the arc.
---
--   NOTE: We take the absolute value of the radius incase it's negative.
--   It would also make sense to draw the sector flipped around the
--   origin, but I think taking the absolute value will be less surprising
--   for the user.
--
sectorWire :: Float -> Float -> Float -> Picture
sectorWire a1 a2 r_
 = let r        = abs r_
   in  Pictures
        [ Arc a1 a2 r
        , line [0, r *^ unitVecAtAngle (degToRad a1) ]
        , line [0, r *^ unitVecAtAngle (degToRad a2) ]
        ]

-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

-- | A path representing a rectangle centered about the origin
rectanglePath
        :: Float        -- ^ width of rectangle
        -> Float        -- ^ height of rectangle
        -> Path
rectanglePath sizeX sizeY
 = let  sx      = sizeX / 2
        sy      = sizeY / 2
   in   [Vec (-sx) (-sy), Vec (-sx) sy, Vec sx sy, Vec sx (-sy)]

-- | A wireframe rectangle centered about the origin.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY
        = lineLoop $ rectanglePath sizeX sizeY

-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY
        = lineLoop $ rectangleUpperPath sizeX sizeY

-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy
 = let  sx      = sizeX / 2
   in   [Vec (-sx) 0, Vec (-sx) sy, Vec sx sy, Vec sx 0]

-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
        = Polygon $ rectanglePath' sizeX sizeY
  where
    rectanglePath' sizeX sizeY =
        let  sx      = sizeX / 2
             sy      = sizeY / 2
        in   [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]

-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY = Polygon $ path sizeX sizeY
  where
    path sizeX sy
      = let  sx      = sizeX / 2
        in   [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]

