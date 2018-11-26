module Dobby.Scripts.Git (gitCompareUrl, gitCommit) where

import ClassyPrelude hiding (stdout)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Shelly

gitProject :: IO Text
gitProject = do
  let
    gitRemote  = run "git" ["remote", "-v"]
    head1      = run "head" ["-1"]
    stripStart = run "sed" ["-e", "s|^origin\tgit@github.com:||"]
    stripEnd   = run "sed" ["-e", "s|\\.git.*||"]
  remote <- shelly . silently $ gitRemote -|- head1
  let
    Right project = parseOnly parseProject remote
  pure project
 where
  parseStart = string "origin\tgit@github.com:"
  parseEnd = string ".git" <* many anyChar
  parseTextUntil = map pack . manyTill anyChar . lookAhead
  parseProject = parseStart *> parseTextUntil parseEnd <* parseEnd

gitCompareUrl :: IO Text
gitCompareUrl =
  ("https://github.com/" ++) . (++ "/compare/") <$> gitProject

gitCommit :: Text -> IO ()
gitCommit message = void . shelly . silently $
  run "git" ["commit", "-am", message]
