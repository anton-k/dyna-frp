module Main where

import Dyna.Brick

main = runApp def (pure win)

win :: Win
win = Win widgets acts

widgets :: Dyn [Box]
widgets = fmap (pure . footer) $ hold (str "Hello Brick!") chars
  where
    chars = fmap (\ch -> str $ "Pressed: " <> pure ch) readChars
    footer w = vBox [w, str "Type any char", str "Press Esc or Enter to exit"]

acts = Quit <$ onKey KEsc <> onKey KEnter

