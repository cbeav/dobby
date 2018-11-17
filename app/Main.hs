module Main where

import ClassyPrelude
import Dobby.ChangeLog

main :: IO ()
main = do
  -- TODO: print usage
  Just tool <- listToMaybe . toList <$> getArgs
  case tool of
    "bump-patch-version" -> patchVersion
    _ -> putStrLn $ "Unrecognized tool: " ++ tool
