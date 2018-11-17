module Dobby.ChangeLog.Version where

import ClassyPrelude
import Prelude (read)
import Data.Attoparsec.Text

data Version
  = Version
  { versionMajor :: !Integer
  , versionMinor :: !Integer
  , versionPatch :: !Integer
  } deriving Eq

instance Show Version where
  show Version{..} = concat
    [ "v"
    , show versionMajor
    , "."
    , show versionMinor
    , "."
    , show versionPatch
    ]

parseVersion :: Parser Version
parseVersion = do
  char 'v'
  versionMajor <- read <$> many1 digit
  char '.'
  versionMinor <- read <$> many1 digit
  char '.'
  versionPatch <- read <$> many1 digit
  pure Version{..}

bumpPatch :: Version -> Version
bumpPatch version = version
  { versionPatch = versionPatch version + 1
  }

versionHeader :: Text -> Version -> Text
versionHeader today version = "## [" ++ tshow version ++ "] - " ++ today
