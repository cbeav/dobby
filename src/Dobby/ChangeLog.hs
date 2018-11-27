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
import qualified Data.HashMap.Strict as HM
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
  , changeLogEntries :: !ChangeLogEntries
  } deriving (Eq, Show)

type ChangeLogEntries = HM.HashMap (Version, Maybe Text) Changes

type Changes = HM.HashMap ChangeType [Text]

data ChangeType
  = Added
  | Changed
  | Deprecated
  | Fixed
  | Removed
  | Security
  deriving (Eq, Show, Read, Generic)

instance Hashable ChangeType

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
commit changes = do
  changeLog@ChangeLog{..} <- readChangeLog
  let
    latestVersion = maximumBy (\a b -> compare (snd a) (snd b)) $ HM.keys changeLogEntries
    msg = prettyPrintChanges changes
  writeChangeLog $ changeLog
    { changeLogEntries = HM.insertWith (HM.unionWith (flip (++))) latestVersion changes changeLogEntries
    }
  gitCommit msg

patchVersion :: IO ()
patchVersion = do
  changeLog@ChangeLog{..} <- readChangeLog
  today <- tshow . utctDay <$> getCurrentTime
  let
    urVersion = (Unreleased, Nothing)
    latestVersion = maximum . map fst $ HM.keys changeLogEntries
    newVersion = (bumpPatch latestVersion, Just today)
    newEntry = changeLogEntries HM.! (Unreleased, Nothing)
    cleaned = HM.insert urVersion HM.empty changeLogEntries
  writeChangeLog $ changeLog
    { changeLogEntries = HM.insert newVersion newEntry cleaned
    }

parseChangeLog :: Parser ChangeLog
parseChangeLog = do
  changeLogIntro <- parseTextUntil parseChangeLogEntry
  changeLogEntries <- HM.fromListWith (++) <$> many parseChangeLogEntry
  pure ChangeLog{..}
 where
  parseTextUntil = map pack . manyTill anyChar . lookAhead

parseChangeLogEntry :: Parser ((Version, Maybe Text), Changes)
parseChangeLogEntry = do
  key <- parseChangeLogHeader
  value <- HM.fromListWith (++) <$> many parseChangeLogEntrySection
  pure (key, value)

parseChangeLogHeader :: Parser (Version, Maybe Text)
parseChangeLogHeader = do
  version <- string "## [" *> parseVersion <* char ']'
  date    <- optional (string " - " *> takeWhile (/= '\n'))
  many endOfLine
  pure (version, date)

parseChangeLogEntrySection :: Parser (ChangeType, [Text])
parseChangeLogEntrySection = do
  changeType <- read <$> (string "### " *> many letter <* endOfLine)
  lines <- option [] (manyTill parseLine parseBreak <* many endOfLine)
  pure (changeType, lines)
 where
  parseLine = string "- " *> takeWhile (/= '\n') <* endOfLine
  parseBreak = void parseVersionLink <|> void endOfLine <|> void endOfInput

prettyPrintChangeLog :: Text -> ChangeLog -> Text
prettyPrintChangeLog compareUrl ChangeLog{..} =
  changeLogIntro ++
  unlines (map prettyPrintChangeLogEntry $ HM.toList changeLogEntries) ++
  unlines (buildReleaseLinks compareUrl [] $ (map fst . HM.keys) changeLogEntries)

prettyPrintChangeLogEntry :: ((Version, Maybe Text), Changes) -> Text
prettyPrintChangeLogEntry ((version, mDate), changes) =
  prettyPrintHeader version mDate ++
  prettyPrintChanges changes
 where
  prettyPrintHeader version date = concat
    [ "## ["
    , prettyVersion version
    , "]"
    , maybe "" (" - " ++) date
    , "\n"
    ]

prettyPrintChanges :: Changes -> Text
prettyPrintChanges changes =
  let
    nonEmptySections = HM.toList $ HM.filter ((> 0) . length) changes
    prettyPrintSection (changeType, lines) = unlines $
      ("### " ++ tshow changeType) : map ("- " ++) lines
  in
   intercalate "\n" $ map prettyPrintSection nonEmptySections

buildReleaseLinks :: Text -> [Text] -> [Version] -> [Text]
buildReleaseLinks compareUrl acc [] = acc
buildReleaseLinks compareUrl acc [_] = acc
buildReleaseLinks compareUrl acc (this:that:rest) =
  let
    link = concat
      [ "["
      , prettyVersion this
      , "]: "
      , compareUrl
      , prettyVersionLink that
      , "..."
      , prettyVersionLink this
      ]
  in
    buildReleaseLinks compareUrl (acc ++  [link]) (that:rest)
