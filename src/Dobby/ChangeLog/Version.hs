module Dobby.ChangeLog.Version where

import ClassyPrelude
import Prelude (read)
import Data.Attoparsec.Text

data Version
  = Unreleased
  | SemanticVersion
    { versionMajor :: !Integer
    , versionMinor :: !Integer
    , versionPatch :: !Integer
    } deriving (Eq, Show)

prettyVersion :: Version -> Text
prettyVersion Unreleased = "Unreleased"
prettyVersion SemanticVersion{..} = pack $ concat
  [ "v"
  , show versionMajor
  , "."
  , show versionMinor
  , "."
  , show versionPatch
  ]

prettyVersionLink :: Version -> Text
prettyVersionLink Unreleased = "HEAD"
prettyVersionLink v = prettyVersion v

parseVersion :: Parser Version
parseVersion = parseUnreleased <|> parseSemanticVersion
 where
  parseUnreleased = string "Unreleased" *> pure Unreleased
  parseSemanticVersion = do
    char 'v'
    versionMajor <- read <$> many1 digit
    char '.'
    versionMinor <- read <$> many1 digit
    char '.'
    versionPatch <- read <$> many1 digit
    pure SemanticVersion{..}

bumpPatch :: Version -> Version
bumpPatch Unreleased = Unreleased
bumpPatch version = version { versionPatch = versionPatch version + 1 }

versionHeader :: Maybe Text -> Version -> Text
versionHeader Nothing version     = "## [" ++ prettyVersion version ++ "]"
versionHeader (Just date) version = "## [" ++ prettyVersion version ++ "] - " ++ date
