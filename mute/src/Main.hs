module Main where

import Mute

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

testParsingFromFile :: String -> IO (Either ParseError [Mute.IOPin])
testParsingFromFile fn = parseFromFile pindefs fn

handleErrors eparseList =
  case eparseList of
    Left err -> Left err
    Right pl ->
      Right $ firstPassCC pl



main :: IO ()
main = do
  parseResult <- testParsingFromFile "testpins.mute"
  putStrLn $ show $ parseResult
