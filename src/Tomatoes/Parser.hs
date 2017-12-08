{-# LANGUAGE OverloadedStrings #-}

module Tomatoes.Parser (
  commandParser
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, string, space)

import Tomatoes.Types (Command(Exit, Help, Auth, StartPomodoro),
  Provider(Github))


commandParser :: Parser Command
commandParser =
      exitParser
  <|> helpParser
  <|> authParser
  <|> startPomodoroParser


exitParser :: Parser Command
exitParser = (string "exit" <|> string "quit") >> return Exit


helpParser :: Parser Command
helpParser = (string "help" <|> string "?") >> return Help


authParser :: Parser Command
authParser = Auth <$> (string "auth" >> space >> githubAuthParser)


githubAuthParser :: Parser Provider
githubAuthParser = string "github" >> return Github


startPomodoroParser :: Parser Command
startPomodoroParser = (string "pomodoro" <|> string "p") >> return StartPomodoro
