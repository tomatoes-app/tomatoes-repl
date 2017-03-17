{-# LANGUAGE OverloadedStrings #-}

module Tomatoes.Parser (
  commandParser
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, string, space)

import Tomatoes.Types (Command(Exit, Help, GithubAuth, StartPomodoro))


commandParser :: Parser Command
commandParser = exitParser <|> helpParser <|> helpShortcutParser <|> authParser
  <|> startPomodoroParser <|> startPomodoroShortcutParser


exitParser :: Parser Command
exitParser = (string "exit" <|> string "quit") >> return Exit


helpParser :: Parser Command
helpParser = string "help" >> return Help


helpShortcutParser :: Parser Command
helpShortcutParser = char 'h' >> return Help


authParser :: Parser Command
authParser = string "auth" >> space >> githubAuthParser


githubAuthParser :: Parser Command
githubAuthParser = string "github" >> return GithubAuth


startPomodoroParser :: Parser Command
startPomodoroParser = string "pomodoro" >> return StartPomodoro


startPomodoroShortcutParser :: Parser Command
startPomodoroShortcutParser = char 'p' >> return StartPomodoro
