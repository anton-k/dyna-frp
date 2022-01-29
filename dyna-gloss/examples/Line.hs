module Main where

import Control.Applicative (liftA2)
import qualified Dyna as D
import Dyna.Gloss
import Graphics.Gloss


spec :: Spec
spec = defSpec { spec'display = InWindow "Main" (600, 800) (50, 50) }

res = fmap (color blue . line) $ D.hold [] $ fmap snd $ D.foldE acc (Nothing, []) $
  D.dropE 2 $ D.snap mouse (D.clock 0.05)
  where
    acc a (mLast, points) = if Just a == mLast then (mLast, points) else (Just a, a : points)

main = runApp spec res2

res2 = fmap (color blue . foldMap line) $ D.hold [] $ fmap (\(_,_,x) -> x) $ D.foldE acc (Nothing, False, []) $
  D.dropE 2 $ D.attachWith (,) mouse (D.snap flag $ D.pulse 0.05)
  where
    acc (a, curFlag) st@(mLast, isPaint, points)
      | not curFlag && not isPaint = st
      | not curFlag && isPaint     = (mLast, False, points)
      | curFlag     && isPaint     = if Just a == mLast then (mLast, isPaint, points) else (Just a, isPaint, (a : head points) : tail points)
      | curFlag     && not isPaint = (Just a, True, [a] : points)

flag :: Dyn Bool
flag = D.hold False $ D.iterateE not True $ mouseRight






