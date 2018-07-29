module Mute (pindefs, firstPassCC, IOPin(..), Var(..), IOPinType(..), emit, parseProgram, num, mnum, aout, ain, din, dout, varName) where


--import Parsec
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char
--import Text.Parsec.Prim
--import Text.Parsec.String (oneOf, char, digit, satisfy)
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad.ST
import Data.STRef


class Emitter a where
  emit :: a -> String

instance Emitter Var where
  emit v = "BIND" ++ (varType v) ++ " " ++ (vname v) ++ " " ++ (show (location v)) ++ "\n"



data IOPinType = AnalogIn | AnalogOut | DigitalIn | DigitalOut deriving (Show)

data IOPin = IOPin { pinType :: IOPinType
                   , pinNum :: Int
                   } deriving (Show)



data Func = Func { fname :: [Char], args :: [Var] , asm :: [Char]} deriving (Show)
data Var = Var { vname :: [Char], varType :: [Char], location :: Int } deriving (Show)
data CompilerContext = CompilerContext { vars :: [Var], subContexts :: [CompilerContext], contextAsm :: [Char] } | NullContext deriving (Show)
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
--testParsingFromFile fn = parseFromFile  (sepBy pindef whitespace) fn

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
        newcc' = addVarToCC newvar $
          case newcc of
            NullContext -> addVarToCC newvar (initCC) { contextAsm = emit newvar }
            globalCC -> globalCC { contextAsm = (contextAsm globalCC) ++ emit newvar}
--        newcc' = addVarToCC newvar newcc
    modifySTRef cmd $ (swapCC newcc') . (\cmd -> cmd { ip = (ip cmd) + 2} )
  readSTRef cmd



ain :: GenParser Char st IOPinType
ain = do
  pin <- string "AI"
  return AnalogIn

aout :: GenParser Char st IOPinType
aout = do
  pin <- string "AO"
  return AnalogOut

{-
aout2 :: Text.Parsec.Prim.ParsecT
         [Char] u Data.Functor.Identity.Identity IOPinType
aout2 = do
  pin <- string "AO"
  return AnalogOut
-}

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

pindefs = do
  endBy pindef whitespace

varName :: GenParser Char st [Char]
varName = do
  sc <- lower
  nextLetters <- many ( alphaNum <|> char '_' )
  return $ sc : nextLetters



parseProgram = do
  pd <- pindefs
  return pd
