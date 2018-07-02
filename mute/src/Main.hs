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

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

main :: IO ()
main = do
  putStrLn "hello world"
