module Tomatoes.Types (
  Command(..),
  availableCommands
) where

import Data.List (intercalate)


data Command = Exit | Help | GithubAuth | StartPomodoro
  deriving (Enum, Bounded)

instance Show Command where
  show Exit = "exit (quit)"
  show Help = "help (h, ?)"
  show GithubAuth = "github"
  show StartPomodoro = "pomodoro (p)"


-- | Return a string with all available commands
availableCommands :: String
availableCommands = intercalate ", " . map show $ commands
  where
    commands :: [Command]
    commands = [minBound..]
