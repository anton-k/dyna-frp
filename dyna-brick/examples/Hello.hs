module Main where

import qualified Dyna as D
import Dyna.Brick

main = runApp (defSpec emptyAttrMap) (pure win)

win :: Win ()
win = Win widgets acts

widgets :: Dyn () [Widget ()]
widgets = fmap (pure . footer) $ D.hold (str "Hello Brick!") chars
  where
    chars = fmap (\ch -> str $ "Pressed: " <> pure ch) readChars
    footer w = vBox [w, str "Type any char", str "Press Esc to exit"]

acts = Quit <$ onKey KEsc


