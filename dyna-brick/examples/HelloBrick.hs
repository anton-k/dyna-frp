module Main where

import Dyna.Brick

main :: IO ()
main = runApp (defSpec emptyAttrMap) $ pure $ Win pic quit

pic :: Dyn [Box]
pic  = pure [str "Hello Brick"]

quit :: Evt Act
quit = Quit <$ onKey KEnter
