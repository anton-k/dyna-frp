{-# language TypeApplications #-}
module InputForm where

import Data.Char
import Text.Read
import System.IO
import Dyna

-- | Input form
data Form = Form
  { form'name     :: Name
  , form'age      :: Age
  , form'sex      :: Sex
  , form'passport :: Passport
  }
  deriving (Show)

-- | User name
data Name = Name
  { name'first  :: String
  , name'last   :: String
  }
  deriving (Show)

newtype Age = Age Int
  deriving (Show, Read)

data Sex = Male | Female
  deriving (Show, Read)

data Passport = Passport
  { passport'serial :: String
  , passport'number :: Integer
  }
  deriving (Show)

-- | Parses single word that contains only alphabetical chars
readName :: String -> Maybe String
readName str = case words str of
  [name] | all isAlpha name -> Just name
  _ -> Nothing

-- | Passport of format XXX-32547
readPassport :: String -> Maybe Passport
readPassport x = case x of
  a:b:c:'-':rest | all isAlpha [a,b,c], Just n <- readMaybe rest ->
    Just (Passport (fmap toUpper [a,b,c]) n)
  _ -> Nothing

-- | Asks repeatedly for the input until it fits the parsing function
-- It uses @takeP@ function that can collect single event from several
-- sub events that are taken as arguments to the parser.
--
-- In this case parser @maybeP@ takes in input until the function returns @Just result@.
inputBy :: String -> (String -> Maybe a) -> IO a
inputBy field f = headE $ takeP (maybeP f) $
  foreverE (once $ putStr (field <> ": ") >> getLine)

-- | Read the whole form from parts
inputForm =
  Form
    <$> inputName
    <*> inputAge
    <*> inputSex
    <*> inputPassport

-- | Read the name
inputName =
  Name <$> inputBy "First name" readName
       <*> inputBy "Last name" readName

inputAge = inputBy "Age" (fmap Age . readMaybe)
inputSex = inputBy "Sex (Male / Female)" readMaybe

inputPassport = inputBy "Passport (format: XXX-32454)" readPassport

-- | Main loop
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to FooBar registration form!"
  inp <- inputForm
  putStrLn $ replicate 40 '-'
  putStrLn "Thank you for registration."
  putStrLn "Your input is:"
  print inp


