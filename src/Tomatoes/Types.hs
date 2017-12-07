module Tomatoes.Types (
  Command(..),
  availableCommands
) where

import Data.List (intercalate)


data Command = Exit | Help | GithubAuth | StartPomodoro | StartPause
  deriving (Enum, Bounded)

instance Show Command where
  show Exit = "exit (quit)"
  show Help = "help (h, ?)"
  show GithubAuth = "auth github"
  show StartPomodoro = "pomodoro (p)"
  show StartPause = "pause"


-- | Return a string with all available commands
availableCommands :: String
availableCommands = intercalate ", " . map show $ commands
  where
    commands :: [Command]
    -- Note: commands after `StartPomodoro` are no available from the CLI
    commands = [minBound..StartPomodoro]
