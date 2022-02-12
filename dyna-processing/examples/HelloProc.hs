module Main where

import Dyna.Proc

main :: IO ()
main = runApp spec $ pure pic
  where
    spec = Spec $ do
      size (P2 500 500)

drawCircle :: P2 -> Draw
drawCircle pos = do
  background white
  translate pos
  fill green
  stroke green
  circle 30 0

pic :: Dyn Draw
pic = drawCircle <$> mouse
