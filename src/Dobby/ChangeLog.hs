module Dobby.ChangeLog
  ( patchVersion
  ) where

import ClassyPrelude hiding (readFile, writeFile)
import Data.Attoparsec.Text
import Data.Time.Clock
import Safe (fromJustNote)
import Data.Text.IO (readFile, writeFile)

import Dobby.ChangeLog.Version
import Dobby.ChangeLog.VersionLink

changeLogFile :: FilePath
changeLogFile = "CHANGELOG.md"

patchVersion :: IO ()
patchVersion = do
  changeLog <- lines <$> readFile changeLogFile
  today     <- tshow . utctDay <$> getCurrentTime
  let
    findLinkMaybe = listToMaybe . rights . map (parseOnly parseVersionLink)
    findLink      = fromJustNote "Unable to find previous version link." . findLinkMaybe
    newLink       = bumpPatchVersionLink $ findLink changeLog
    newChangeLog  = updateChangeLog newLink today changeLog
  writeFile changeLogFile newChangeLog

updateChangeLog :: VersionLink -> Text -> [Text] -> Text
updateChangeLog link today cl = intercalate "\n" $ updateChangeLogRec link today cl []

updateChangeLogRec :: VersionLink -> Text -> [Text] -> [Text] -> [Text]
updateChangeLogRec _ _ [] acc = acc
updateChangeLogRec link today (next:rest) acc = case unpack next of
  "## [Unreleased]" -> updateChangeLogRec link today rest $ acc ++
    [ "## [Unreleased]", ""
    , versionHeader today $ versionLinkVersion link
    ]
  ('[':'U':'n':'r':_) -> updateChangeLogRec link today rest $ acc ++
    [ unreleasedLink link
    , previousLink link
    ]
  _ -> updateChangeLogRec link today rest (acc ++ [next])
