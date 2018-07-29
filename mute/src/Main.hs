module Main where

import Mute

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Control.Monad

testParsingFromFile :: String -> IO (Either ParseError [Mute.IOPin])
testParsingFromFile fn = parseFromFile pindefs fn

handleErrors eparseList =
  case eparseList of
    Left err -> Left err
    Right pl ->
      Right $ firstPassCC pl



main :: IO ()
main = do
  parseResult <- liftM handleErrors (parseFromFile parseProgram "testpins.mute") 
  putStrLn $ show $ parseResult
  --parseResult <- liftM handleErrors (parseFromFile varName "testpins.mute") 
--  putStrLn $ show $ parseResult
