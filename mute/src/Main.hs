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



data Func = Func { fname :: [Char], args :: [Var] , asm :: [Char]} deriving (Show)
data Var = Var { vname :: [Char], varType :: [Char], location :: Int } deriving (Show)
data CompilerContext = CompilerContext { vars :: [Var], functions :: [Func], contextAsm :: [Char] } | NullContext deriving (Show)
data CompilerMetaData = CompilerMetaData { ip :: Int  , hp :: Int, globalContext :: CompilerContext } deriving (Show)

initCC = CompilerContext [] [] ""
initCMD = CompilerMetaData 0 0 NullContext
addVarToCC v cc =
  case cc of
    NullContext -> addVarToCC v initCC
    cc -> cc { vars = v : (vars cc) }


swapCC incc cmd = cmd { globalContext = incc }





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
      Right $ firstPassCC pl


firstPass parseList = runST $ do
  varList <- newSTRef []
  forM_ parseList $ \v -> do
    modifySTRef varList $ ((:) . (pinNum)) v
  readSTRef varList


firstPassCC parseList = runST $ do
  cmd <- newSTRef initCMD
  --
  forM_ parseList $ \v -> do
    cmd' <- readSTRef cmd
    let newvar = Var ((show (pinType v)) ++ (show (pinNum v))) "PIN" 0
        newcc = globalContext cmd'
        newcc' = addVarToCC newvar newcc
    modifySTRef cmd $ (swapCC newcc') . (\cmd -> cmd {ip = (ip cmd) + 2})
  readSTRef cmd

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
