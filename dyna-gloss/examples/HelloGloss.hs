module Main where

import Dyna.Gloss

main :: IO ()
main = runApp defSpec $ pure pic

pic :: Dyn Picture
pic = (\pos -> translate pos $ color green $ circleSolid 50) <$> mouse
