module Dobby.Scripts.Git (gitCompareUrl) where

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
  remote <- shelly $ gitRemote -|- head1
  let
    Right test = parseOnly parseProject remote
  pure test
 where
  parseStart = string "origin\tgit@github.com:"
  parseEnd = string ".git" <* many anyChar
  parseTextUntil = map pack . manyTill anyChar . lookAhead
  parseProject = parseStart *> parseTextUntil parseEnd <* parseEnd

gitCompareUrl :: IO Text
gitCompareUrl =
  ("https://github.com/" ++) . (++ "/compare") <$> gitProject
