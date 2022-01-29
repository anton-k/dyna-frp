module Dyna.Gloss.Data.Vec(
    Vec(..)
  , magV
  , argV
  , dotV
  , detV
  , mulSV
  , rotateV
  , angleVV
  , normalizeV
  , unitVecAtAngle
  , e
  , VecBasis(..)
  , fromTuple
  , toTuple
  ) where

import Graphics.Gloss.Geometry.Angle
import Dyna (BasisArity(..))
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Basis
import Data.Cross
import Data.VectorSpace

-- | Pair of unboxed floats. All operations on vectors are strict
-- which is more suitable for computation intensive domains such as computer graphics.
data Vec = Vec
  { vec'x :: {-# UNPACK #-} !Float
  , vec'y :: {-# UNPACK #-} !Float
  }
  deriving (Show, Eq, Ord)

-- | Converts vector to tuple of Floats
toTuple :: Vec -> (Float, Float)
toTuple (Vec x y) = (x, y)

-- | Converts tuple of floats to vector
fromTuple :: (Float, Float) -> Vec
fromTuple (x, y) = Vec x y

lift0 :: Float -> Vec
lift0 a = Vec a a

lift1 :: (Float -> Float) -> Vec -> Vec
lift1 f (Vec a b) = Vec (f a) (f b)

lift2 :: (Float -> Float -> Float) -> Vec -> Vec -> Vec
lift2 f (Vec a1 b1) (Vec a2 b2) = Vec (f a1 a2) (f b1 b2)

-- numeric instances

instance Num Vec where
  fromInteger = lift0 . fromInteger
  (+) = lift2 (+)
  (*) = lift2 (*)
  (-) = lift2 (-)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum

instance Fractional Vec where
  fromRational = lift0 . fromRational
  recip = lift1 recip
  (/) = lift2 (/)

-- vector space instances

instance AdditiveGroup Vec where
  zeroV = lift0 zeroV
  (^+^) = lift2 (^+^)
  (^-^) = lift2 (^-^)
  negateV = lift1 negateV

instance VectorSpace Vec where
  type Scalar Vec = Float
  (*^) k = lift1 (k *^)

instance AffineSpace Vec where
  type Diff Vec = Vec
  (.-.) = lift2 (.-.)
  (.+^) = lift2 (.+^)

instance BasisArity Vec where
  basisArity _ = 2

data VecBasis = VecX | VecY

instance HasBasis Vec where
  type Basis Vec = VecBasis
  basisValue = \case
    VecX -> Vec 1 0
    VecY -> Vec 0 1

  decompose (Vec x y) = [(VecX, x), (VecY, y)]

  decompose' (Vec x y) = \case
    VecX -> x
    VecY -> y

instance HasNormal Vec where
  normalVec = normalizeV

instance HasCross2 Vec where
  cross2 (Vec x y) = Vec (negate y) x -- or @(y,-x)@?

-------------------------------------------------------------------
-- gloss functions on Vectors

-- | Normalise a vector, so it has a magnitude of 1.
normalizeV :: Vec -> Vec
normalizeV v = lift1 ((1 / magV v) * ) v
{-# INLINE normalizeV #-}

-- | The magnitude of a vector.
magV :: Vec -> Float
magV (Vec x y) = sqrt (x * x + y * y)
{-# INLINE magV #-}

-- | The angle of this vector, relative to the +ve x-axis.
argV :: Vec -> Float
argV (Vec x y) = normalizeAngle $ atan2 y x
{-# INLINE argV #-}

-- | The dot product of two vectors.
dotV :: Vec -> Vec -> Float
dotV (Vec x1 x2) (Vec y1 y2) = x1 * y1 + x2 * y2
{-# INLINE dotV #-}

-- | The determinant of two vectors.
detV :: Vec -> Vec -> Float
detV (Vec x1 y1) (Vec x2 y2) = x1 * y2 - y1 * x2
{-# INLINE detV #-}

-- | Multiply a vector by a scalar.
mulSV :: Float -> Vec -> Vec
mulSV s (Vec x y) = Vec (s * x) (s * y)
{-# INLINE mulSV #-}

-- | Rotate a vector by an angle (in radians). +ve angle is counter-clockwise.
rotateV :: Float -> Vec -> Vec
rotateV r (Vec x y)
 = Vec (x * cos r - y * sin r)
       (x * sin r + y * cos r)
{-# INLINE rotateV #-}

-- | Compute the inner angle (in radians) between two vectors.
angleVV :: Vec -> Vec -> Float
angleVV p1 p2
 = let  m1      = magV p1
        m2      = magV p2
        d       = p1 `dotV` p2
        aDiff   = acos $ d / (m1 * m2)

   in   aDiff
{-# INLINE angleVV #-}

-- | Produce a unit vector at a given angle relative to the +ve x-axis.
--      The provided angle is in radians.
unitVecAtAngle :: Float -> Vec
unitVecAtAngle r = Vec (cos r) (sin r)
{-# INLINE unitVecAtAngle #-}

-- | Shortcut for @unitVecAtAngle@
e :: Float -> Vec
e = unitVecAtAngle
{-# INLINE e #-}

