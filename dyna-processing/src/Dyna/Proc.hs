module Dyna.Proc(
  module X
) where

import Dyna.Proc.Frp   as X
import Dyna.Proc.Run   as X
import Dyna.Proc.Types as X

import Graphics.Proc   as X hiding
  (mouse, relMouse, runProc, Proc, Sum(..), getSum, loop)

import Data.AdditiveGroup as X
import Data.AffineSpace as X
import Data.Basis as X
import Data.Cross as X
import Data.VectorSpace as X
import Temporal.Class as X

instance Semigroup Draw where
  (<>) = (>>)

instance Monoid Draw where
  mempty = pure ()

