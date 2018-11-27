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
    } deriving (Eq, Show, Generic)

instance Hashable Version

instance Ord Version where
  compare Unreleased Unreleased = EQ
  compare Unreleased _ = LT
  compare _ Unreleased = GT
  compare (SemanticVersion ma1 mi1 pa1) (SemanticVersion ma2 mi2 pa2)
    | ma1 == ma2 && mi1 == mi2 = compare pa1 pa2
    | ma1 == ma2 = compare mi1 mi2
    | otherwise = compare ma1 ma2

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
bumpPatch Unreleased = SemanticVersion 0 0 1
bumpPatch version = version { versionPatch = versionPatch version + 1 }

versionHeader :: Maybe Text -> Version -> Text
versionHeader date version = "## [" ++ prettyVersion version ++ "]" ++ maybe "" (" - " ++) date
