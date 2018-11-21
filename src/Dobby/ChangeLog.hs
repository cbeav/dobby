module Dobby.ChangeLog
  ( patchVersion
  ) where

import ClassyPrelude hiding (readFile, takeWhile, try, writeFile)
import Prelude (read)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Text.IO (readFile, writeFile)
import Data.Time.Clock
import Safe (fromJustNote)

import Dobby.Scripts.Git
import Dobby.ChangeLog.Version
import Dobby.ChangeLog.VersionLink

data ChangeLog
  = ChangeLog
  { changeLogIntro :: !Text
  , changeLogEntries :: ![ChangeLogEntry]
  } deriving (Eq, Show)

data ChangeLogEntry
  = ChangeLogEntry
  { changeLogEntryVersion    :: !Version
  , changeLogEntryDate       :: !(Maybe Text)
  , changeLogEntrySections   :: ![ChangeLogEntrySection]
  } deriving (Eq, Show)

data ChangeLogEntrySection
  = ChangeLogEntrySection
  { changeLogEntrySectionType  :: !ChangeType
  , changeLogEntrySectionLines :: ![Text]
  } deriving (Eq, Show)

data ChangeType
  = Added
  | Changed
  | Deprecated
  | Fixed
  | Removed
  | Security
  deriving (Eq, Show, Read)

changeLogFile :: FilePath
changeLogFile = "CHANGELOG.md"

patchVersion :: IO ()
patchVersion = do
  Right changeLog <- parseOnly parseChangeLog <$> readFile changeLogFile
  today <- tshow . utctDay <$> getCurrentTime
  compareUrl <- gitCompareUrl
  writeFile changeLogFile $ prettyPrintChangeLog compareUrl changeLog

updateChangeLog :: VersionLink -> Text -> [Text] -> Text
updateChangeLog link today cl = unlines $ updateChangeLogRec link today cl []

updateChangeLogRec :: VersionLink -> Text -> [Text] -> [Text] -> [Text]
updateChangeLogRec _ _ [] acc = acc
updateChangeLogRec link today (next:rest) acc = case unpack next of
  "## [Unreleased]" -> updateChangeLogRec link today rest $ acc ++
    [ "## [Unreleased]", ""
    , versionHeader (Just today) $ versionLinkVersion link
    ]
  ('[':'U':'n':'r':_) -> updateChangeLogRec link today rest $ acc ++
    [ unreleasedLink link
    , previousLink link
    ]
  _ -> updateChangeLogRec link today rest (acc ++ [next])

parseChangeLog :: Parser ChangeLog
parseChangeLog = do
  changeLogIntro <- parseTextUntil parseChangeLogEntry
  changeLogEntries <- many parseChangeLogEntry
  pure ChangeLog{..}
 where
  parseTextUntil = map pack . manyTill anyChar . lookAhead

parseChangeLogEntry :: Parser ChangeLogEntry
parseChangeLogEntry = do
  (changeLogEntryVersion, changeLogEntryDate) <- parseChangeLogHeader
  changeLogEntrySections <- many parseChangeLogEntrySection
  pure ChangeLogEntry{..}

parseChangeLogHeader :: Parser (Version, Maybe Text)
parseChangeLogHeader = do
  version <- string "## [" *> parseVersion <* char ']'
  date    <- optional (string " - " *> takeWhile (/= '\n'))
  many endOfLine
  pure (version, date)

parseChangeLogEntrySection :: Parser ChangeLogEntrySection
parseChangeLogEntrySection = do
  changeLogEntrySectionType <- read <$> (string "### " *> many letter <* endOfLine)
  changeLogEntrySectionLines <- option [] (manyTill parseLine parseBreak <* many endOfLine)
  pure ChangeLogEntrySection{..}
 where
  parseLine = string "- " *> takeWhile (/= '\n') <* endOfLine
  parseBreak = void parseVersionLink <|> void endOfLine <|> void endOfInput

prettyPrintChangeLog :: Text -> ChangeLog -> Text
prettyPrintChangeLog compareUrl ChangeLog{..} =
  changeLogIntro ++
  unlines (map prettyPrintChangeLogEntry changeLogEntries) ++
  unlines (buildReleaseLinks compareUrl [] changeLogEntries)

prettyPrintChangeLogEntry :: ChangeLogEntry -> Text
prettyPrintChangeLogEntry ChangeLogEntry{..} =
  prettyPrintHeader changeLogEntryVersion changeLogEntryDate ++
  intercalate "\n" (map prettyPrintSection changeLogEntrySections)
 where
  prettyPrintHeader version date = concat
    [ "## ["
    , prettyVersion version
    , "]"
    , maybe "" (" - " ++) date
    , "\n"
    ]
  prettyPrintSection ChangeLogEntrySection{..} = unlines $
    ("### " ++ tshow changeLogEntrySectionType) :
    map ("- " ++) changeLogEntrySectionLines

buildReleaseLinks :: Text -> [Text] -> [ChangeLogEntry] -> [Text]
buildReleaseLinks compareUrl acc [] = acc
buildReleaseLinks compareUrl acc [_] = acc
buildReleaseLinks compareUrl acc (this:that:rest) =
  let
    thisVersion = changeLogEntryVersion this
    thatVersion = changeLogEntryVersion that
    link this that = concat
      [ "["
      , prettyVersion this
      , "]: "
      , compareUrl
      , prettyVersionLink that
      , "..."
      , prettyVersionLink this
      ]
  in
    buildReleaseLinks compareUrl (acc ++  [link thisVersion thatVersion]) (that:rest)
