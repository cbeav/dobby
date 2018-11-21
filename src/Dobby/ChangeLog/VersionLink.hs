module Dobby.ChangeLog.VersionLink where

import ClassyPrelude
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

import Dobby.ChangeLog.Version

data VersionLink
  = VersionLink
  { versionLinkVersion :: !Version
  , versionLinkCompareUrl :: !Text
  , versionLinkPreviousVersion :: !Version
  } deriving (Eq, Show)

parseVersionLink :: Parser VersionLink
parseVersionLink =
  VersionLink
    <$> parseStart
    <*> parseUrlUntil parseEnd
    <*> parseEnd
 where
  parseStart    = char '[' *> parseVersion <* string "]: "
  parseUrlUntil = map pack . manyTill anyChar . lookAhead
  parseEnd      = parseVersion <* string "..." <* parseVersion <* endOfInput

bumpPatchVersionLink :: VersionLink -> VersionLink
bumpPatchVersionLink versionLink = versionLink
  { versionLinkPreviousVersion = versionLinkVersion versionLink
  , versionLinkVersion = bumpPatch $ versionLinkVersion versionLink
  }

previousLink :: VersionLink -> Text
previousLink VersionLink{..} = concat
  [ "["
  , tshow versionLinkVersion
  , "]: "
  , versionLinkCompareUrl
  , tshow versionLinkPreviousVersion
  , "..."
  , tshow versionLinkVersion
  ]

unreleasedLink :: VersionLink -> Text
unreleasedLink VersionLink{..} = concat
  [ "[Unreleased]: "
  , versionLinkCompareUrl
  , tshow versionLinkVersion
  , "...HEAD"
  ]
