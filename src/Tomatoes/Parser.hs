{-# LANGUAGE OverloadedStrings #-}

module Tomatoes.Parser (
  commandParser
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (takeTill)
import Data.Attoparsec.ByteString.Char8 (Parser, string, space, isEndOfLine)

import Tomatoes.Types (Command(Exit, Help, GithubAuth))


commandParser :: Parser Command
commandParser = exitParser <|> helpParser <|> authParser


exitParser :: Parser Command
exitParser = (string "exit" <|> string "quit") >> return Exit


helpParser :: Parser Command
helpParser = string "help" >> return Help


authParser :: Parser Command
authParser = string "auth" >> space >> githubAuthParser


githubAuthParser :: Parser Command
githubAuthParser = string "github" >> space >> GithubAuth <$> takeTill isEndOfLine
