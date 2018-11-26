module Main where

import ClassyPrelude
import Dobby.ChangeLog
import Options.Applicative
import Options.Applicative.Text

commitOpts :: Parser Changes
commitOpts = Changes
  <$> many (textOption
    ( long "added"
   <> short 'a'
   <> metavar "ADDED" ) )
  <*> many (textOption
    ( long "changed"
   <> short 'c'
   <> metavar "CHANGED" ) )
  <*> many (textOption
    ( long "deprecated"
   <> short 'd'
   <> metavar "DEPRECATED" ) )
  <*> many (textOption
    ( long "fixed"
   <> short 'f'
   <> metavar "FIXED" ) )
  <*> many (textOption
    ( long "Removed"
   <> short 'r'
   <> metavar "REMOVED" ) )
  <*> many (textOption
    ( long "Security"
   <> short 's'
   <> metavar "SECURITY" ) )

main :: IO ()
main =
  join . execParser . flip info idm $ subparser
    ( command "commit" (info (commit <$> commitOpts) idm)
   <> command "patch-release" (info (pure patchVersion) idm) )
