module Types where

import           Data.Text (Text)

newtype ShortLink =
  ShortLink { unShortLink :: Text }
  deriving (Eq, Show, Ord)

newtype URL =
  URL { unURL :: Text }
  deriving (Eq, Show)
