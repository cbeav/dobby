module Dobby.ChangeLog
  ( Changes(..)
  , ChangeType(..)
  , commit
  , patchVersion
  ) where

import ClassyPrelude hiding (maximum, maximumBy, readFile, takeWhile, try, writeFile)
import Prelude (maximum, read)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Default
import Data.List (maximumBy)
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

instance Default ChangeLogEntry where
  def = ChangeLogEntry Unreleased Nothing []

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

data Changes
  = Changes
  { changesAdded :: ![Text]
  , changesChanged :: ![Text]
  , changesDeprecated :: ![Text]
  , changesFixed :: ![Text]
  , changesRemoved :: ![Text]
  , changesSecurity :: ![Text]
  } deriving (Eq, Show)

changeLogFile :: FilePath
changeLogFile = "CHANGELOG.md"

readChangeLog :: IO ChangeLog
readChangeLog = do
  Right changeLog <- parseOnly parseChangeLog <$> readFile changeLogFile
  pure changeLog

writeChangeLog :: ChangeLog -> IO ()
writeChangeLog changeLog = do
  compareUrl <- gitCompareUrl
  writeFile changeLogFile $ prettyPrintChangeLog compareUrl changeLog

commit :: Changes -> IO ()
commit Changes{..} = do
  changeLog@ChangeLog{..} <- readChangeLog
  let
    maxVersion = maximum $ map changeLogEntryVersion changeLogEntries
    versionMatches = (== maxVersion) . changeLogEntryVersion
    changeTuples =
      [ (Added, changesAdded)
      , (Changed, changesChanged)
      , (Deprecated, changesDeprecated)
      , (Fixed, changesFixed)
      , (Removed, changesRemoved)
      , (Security, changesSecurity)
      ]
    updateEntry entry = foldr (uncurry addChanges) entry changeTuples
    newEntry = ChangeLogEntry
      { changeLogEntryVersion = Unreleased
      , changeLogEntryDate = Nothing
      , changeLogEntrySections = mapMaybe (uncurry buildSection) changeTuples
      }
    msg = intercalate "\n\n" $ mapMaybe (uncurry commitMessage) changeTuples
  writeChangeLog $ changeLog
    { changeLogEntries = addOrUpdateWhere versionMatches updateEntry newEntry changeLogEntries
    }
  gitCommit msg
 where
  buildSection _ [] = Nothing
  buildSection changeLogEntrySectionType changeLogEntrySectionLines = Just $ ChangeLogEntrySection{..}
  commitMessage _ [] = Nothing
  commitMessage changeType messages = Just $
    intercalate "\n" ((tshow changeType ++ ":") : messages)

addChanges :: ChangeType -> [Text] -> ChangeLogEntry -> ChangeLogEntry
addChanges _ [] entry = entry
addChanges changeType messages entry@ChangeLogEntry{..} =
  let
    sectionMatches = (== changeType) . changeLogEntrySectionType
    updateSection section = section
      { changeLogEntrySectionLines = changeLogEntrySectionLines section ++ messages
      }
    newSection = ChangeLogEntrySection
      { changeLogEntrySectionType = changeType
      , changeLogEntrySectionLines = messages
      }
  in
    entry
      { changeLogEntrySections = addOrUpdateWhere sectionMatches updateSection newSection changeLogEntrySections
      }

addOrUpdateWhere :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
addOrUpdateWhere pred mod def as =
  maybe (as ++ [def]) (pure $ map (\a -> if pred a then mod a else a) as) (find pred as)

patchVersion :: IO ()
patchVersion = do
  changeLog <- readChangeLog
  today <- tshow . utctDay <$> getCurrentTime
  let maxVersion = maximum . map changeLogEntryVersion $ changeLogEntries changeLog
  writeChangeLog $ changeLog
    { changeLogEntries = def : map (promoteUnreleased (bumpPatch maxVersion) today) (changeLogEntries changeLog)
    }
 where
  promoteUnreleased newVersion today entry = case changeLogEntryVersion entry of
    Unreleased -> entry
      { changeLogEntryVersion = newVersion
      , changeLogEntryDate = Just today
      }
    version -> entry

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
