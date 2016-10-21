module Tomatoes.Types (
  Command(..)
) where

import Data.ByteString (ByteString)


data Command = Exit | Help | GithubAuth ByteString
