{-# LANGUAGE TupleSections #-}
module Main where

import ClassyPrelude hiding (fromList)
import Data.HashMap.Strict (fromList)
import Dobby.ChangeLog
import Options.Applicative
import Options.Applicative.Text

commitOpts :: Parser Changes
commitOpts = fromList <$> sequenceA
  [ (Added,)      <$> opt "added"      'a' "ADDED"
  , (Changed,)    <$> opt "changed"    'c' "CHANGED"
  , (Deprecated,) <$> opt "deprecated" 'd' "DEPRECATED"
  , (Fixed,)      <$> opt "fixed"      'f' "FIXED"
  , (Removed,)    <$> opt "removed"    'r' "REMOVED"
  , (Security,)   <$> opt "security"   's' "SECURITY"
  ]
 where opt l s d = many (textOption (long l <> short s <> metavar d))

main :: IO ()
main = join . execParser . flip info idm $ subparser
  ( command "commit"        (info (commit <$> commitOpts) idm)
 <> command "patch-release" (info (pure patchVersion)     idm)
  )
