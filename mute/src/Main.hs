module Main where


--import Parsec
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
--import Text.Parsec.String (oneOf, char, digit, satisfy)
import qualified Text.ParserCombinators.Parsec.Token as Token
--import FunctionsAndTypesForParsing

import Control.Monad.ST
import Data.STRef



data IOPinType = AnalogIn | AnalogOut | DigitalIn | DigitalOut deriving (Show)

data IOPin = IOPin { pinType :: IOPinType
                   , pinNum :: Int
                   } deriving (Show)

data Var = Var { varType :: [Char], location :: Int } deriving (Show)


eol :: GenParser Char st Char
eol = char '\n'


num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

mnum = do
  nums <- many1 num
  eol
  return nums

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

whitespace = void $ many $ oneOf " \n\t"
testParsingFromFile fn = parseFromFile  (sepBy1 pindef whitespace) fn

handleErrors eparseList =
  case eparseList of
    Left err -> Left err
    Right pl ->
      Right $ firstPass pl

firstPass parseList = runST $ do
  varList <- newSTRef []
  forM_ parseList $ \v -> do
    modifySTRef varList $ ((:) . (pinNum)) v
  readSTRef varList

main :: IO ()
main = do
  parseResult <- testParsingFromFile "testpins.mute"
  putStrLn $ show $ parseResult

ain :: GenParser Char st IOPinType
ain = do
  pin <- string "AI"
  return AnalogIn

aout = do
  pin <- string "AO"
  return AnalogOut

analog = do
  pin <- try aout <|> ain
  return pin

din :: GenParser Char st IOPinType
din = do
  pin <- string "DI"
  return DigitalIn

dout = do
  pin <- string "DO"
  return DigitalOut

digital = do
  pin <- try din <|> dout
  return pin

pindef = do
  pinType <- digital <|> analog
  pinNum <- num
  --return (pintype, pinnum)
  return (IOPin pinType (fromIntegral pinNum))
