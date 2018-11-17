module Dobby.ChangeLog.VersionLink where

import ClassyPrelude
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

import Dobby.ChangeLog.Version

data VersionLink
  = VersionLink
  { versionLinkVersion :: !Version
  , versionLinkPreviousVersion :: !Version
  , versionLinkCompareUrl :: !Text
  } deriving (Eq, Show)

parseVersionLink :: Parser VersionLink
parseVersionLink = do
  versionLinkVersion <- char '[' *> parseVersion <* string "]: "
  versionLinkCompareUrl <- pack <$> manyTill anyChar (lookAhead parsePrevFromEnd)
  versionLinkPreviousVersion <- parsePrevFromEnd
  pure VersionLink{..}
 where
  parsePrevFromEnd = parseVersion <* string "..." <* parseVersion <* endOfInput

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
