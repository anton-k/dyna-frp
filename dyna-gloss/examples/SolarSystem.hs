{-# Language RecordWildCards #-}
{-# Language FlexibleContexts #-}
-- | Draw a solar system in motion. We can change scale by mouse wheel
-- and displace it by dragging.
module Main(
  main
) where

import Dyna.Gloss

data Body = Body
  { body'size       :: Float
  , body'color      :: Color
  , body'satelites  :: [Satelite]
  , body'shape      :: Bool
  }

data Satelite = Satelite
  { satelite'radius  :: Float
  , satelite'speed   :: Float
  , satelite'start   :: Float
  , satelite'body    :: Body
  }

--------------------------------------------------------------------------------
-- draw body

type Time = Float

drawBody :: Time -> Body -> Picture
drawBody t Body{..} =
       (color body'color shape)
    <> foldMap (drawSatelite t) body'satelites
  where
    shape
      | body'shape = circleSolid body'size
      | otherwise  = rectangleSolid body'size body'size

drawSatelite :: Time -> Satelite -> Picture
drawSatelite t (Satelite rad speed start body) =
  rotate angle $ translate (Vec rad 0) $ drawBody t body
  where
    angle = 0.12 * t * speed + start

--------------------------------------------------------------------------------

move :: Dyn (Picture -> Picture)
move = liftA2 (.) displace scaleWheel

-- | Scale picture on mouse wheel event
scaleWheel :: Dyn (Picture -> Picture)
scaleWheel = (\n -> scale (Vec n n)) <$> foldD updateScale 1 mouseWheel
  where
    updateScale a s = min maxBound $ max minBound (s + a * step)
      where
        step = 0.05
        minBound = 0.3
        maxBound = 2.3

-- | Displace picture on drag by righ mouse button event
displace :: Dyn (Picture -> Picture)
displace = fmap translate $ foldD const 0 $ whenE (isDrag RightButton) (snap mouse $ pulse 0.01)

--------------------------------------------------------------------------------

main :: IO ()
main = runApp spec $ pure $ move <*> ((\t -> drawBody t solarSystem) <$> timerD 0.005)
  where
    spec = defSpec { spec'display = FullScreen }
    -- spec = defSpec { spec'display = InWindow "Solar system" (900, 900) (25, 25) }

--------------------------------------------------------------------------------

solarSystem :: Body
solarSystem = sun
  where
    sun = Body
      { body'size      = 40
      , body'color     = yellow
      , body'satelites = mconcat [innerPlanets, asteroidBelt, ourterPlanets {-, asteroidBelt2-}]
      , body'shape     = True
      }
    innerPlanets = [mercury, venus, earth, mars]
    ourterPlanets = [jupiter, saturn, uranus, neptune]

    mercury = Satelite
      { satelite'radius = 100
      , satelite'speed  = 2
      , satelite'start  = 0
      , satelite'body   =
          Body
            { body'size      = 7
            , body'color     = blue
            , body'satelites = []
            , body'shape      = True
            }
      }

    venus = Satelite
      { satelite'radius  = 135
      , satelite'speed   = 1
      , satelite'start  = 0.3
      , satelite'body    =
          Body
            { body'size      = 10
            , body'color     = orange
            , body'satelites = []
            , body'shape     = True
            }
      }

    earth = Satelite
      { satelite'radius  = 190
      , satelite'speed   = 1.6
      , satelite'start   = 1.3
      , satelite'body    =
          Body
            { body'size = 12
            , body'color = blue
            , body'satelites = [moon]
            , body'shape      = True
            }
      }
      where
        moon = Satelite
          { satelite'radius = 20
          , satelite'speed  = 7
          , satelite'start  = 0
          , satelite'body   =
              Body
                { body'size  = 3
                , body'color = black
                , body'satelites = []
                , body'shape      = True
                }
          }

    mars = Satelite
      { satelite'radius   = 250
      , satelite'speed    = 1.3
      , satelite'start    = 1.9
      , satelite'body     =
          Body
            { body'size      = 12
            , body'color     = magenta
            , body'satelites = [deimos, phobos]
            , body'shape      = True
            }
      }
      where
        deimos = Satelite
          { satelite'radius = 18
          , satelite'speed  = 4
          , satelite'start    = 0.9
          , satelite'body   =
              Body
                { body'size  = 3
                , body'color = green
                , body'satelites = []
                , body'shape      = True
                }
          }

        phobos = Satelite
          { satelite'radius = 27
          , satelite'speed  = 5
          , satelite'start  = 0.1
          , satelite'body   =
              Body
                { body'size  = 2
                , body'color = magenta
                , body'satelites = []
                , body'shape      = True
                }
          }

    asteroidBelt = asteroidBeltBy 280 45
    asteroidBelt2 = mconcat [asteroidBeltBy 670 7, asteroidBeltBy 680 3, asteroidBeltBy 700 2]

    asteroidBeltBy mainRad size = [asteroid (rad * 3) (start/(size + 35)) | start <- [1..size], rad <- [1..5]]
      where
        asteroid a n = Satelite
            { satelite'radius   = mainRad + a
            , satelite'speed    = 0.2 + (a / 4) + (n / 70)
            , satelite'start    = n
            , satelite'body     =
                Body
                  { body'size  = 3.4
                  , body'color = if a < 5 then black else blue
                  , body'satelites = []
                  , body'shape     = False
                  }
            }

    jupiter = Satelite
      { satelite'radius   = 360
      , satelite'speed    = 1
      , satelite'start    = 0.2
      , satelite'body     =
          Body
            { body'size  = 25
            , body'color = red
            , body'satelites = [ganymede, callisto, io, europa]
            , body'shape      = True
            }
      }
      where
        ganymede = Satelite
          { satelite'radius = 30
          , satelite'speed  = 4
          , satelite'start  = 0
          , satelite'body   = Body 3 green [] True
          }
        callisto = Satelite
          { satelite'radius = 38
          , satelite'speed  = 3
          , satelite'start  = 0.3
          , satelite'body   = Body 3 black [] True
          }
        io = Satelite
          { satelite'radius = 47
          , satelite'speed  = 3
          , satelite'start  = 0.3
          , satelite'body   = Body 3 yellow [] True
          }
        europa = Satelite
          { satelite'radius = 53
          , satelite'speed  = 2.5
          , satelite'start  = 0.8
          , satelite'body   = Body 3 red [] True
          }

    saturn = Satelite
      { satelite'radius   = 440
      , satelite'speed    = 1.2
      , satelite'start    = 0.5
      , satelite'body     =
          Body
            { body'size  = 20
            , body'color = blue
            , body'satelites =
                let xs a = (\n -> Satelite (22 + 1 * n) (6 + 0.1 * n) (a + n / 12) (Body 3.4 black [] False)) <$> [0..12]
                in xs 0 ++ xs 0.25
            , body'shape = True
            }

      }

    uranus = Satelite
      { satelite'radius   = 510
      , satelite'speed    = 1.3
      , satelite'start    = 1.9
      , satelite'body     =
          Body
            { body'size      = 15
            , body'color     = yellow
            , body'satelites = (\n -> Satelite (18 + 0.5 * n) (3 + 0.03 * n) (n / 27) (Body 3.5 blue [] False)) <$> [0..20]
            , body'shape = True
            }
      }

    neptune = Satelite
      { satelite'radius   = 580
      , satelite'speed    = 0.7
      , satelite'start    = 1.9
      , satelite'body     =
          Body
            { body'size      = 15
            , body'color     = black
            , body'satelites = [triton]
            , body'shape = True
            }
      }
      where
        triton = Satelite
          { satelite'radius = 20
          , satelite'speed  = 7
          , satelite'start  = 0
          , satelite'body   =
              Body
                { body'size  = 3
                , body'color = blue
                , body'satelites = []
                , body'shape = True
                }
          }

