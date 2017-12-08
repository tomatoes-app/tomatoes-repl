module Tomatoes.Types (
  Command(..),
  Provider(..),
  availableCommands
) where

import Data.List (intercalate)


data Command = Exit | Help | Auth Provider | StartPomodoro | StartPause

instance Show Command where
  show Exit = "exit (quit)"
  show Help = "help (h, ?)"
  show (Auth provider) = "auth " ++ show provider
  show StartPomodoro = "pomodoro (p)"
  show StartPause = "pause"


data Provider = Github

instance Show Provider where
  show Github = "github"


-- | Return a string with all available commands
availableCommands :: String
availableCommands = intercalate ", " . map show $ commands
  where
    commands = [
        Exit,
        Help,
        Auth Github,
        StartPomodoro
      ]
