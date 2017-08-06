module Hanum.CLI
  ( getArgs
  , runCLI
  ) where

import           Control.Applicative ((<*>))
import           Control.Monad (unless)
import           Data.Char (ord)
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, exitFailure)
import qualified Hanum.Arguments as Args

version :: String
version = "0.1.0.0-alpha"

combine :: [String] -> String
combine []         = []
combine (str:more) = str ++ combine more

display :: [String] -> IO ()
display []         = return ()
display (str:more) = putStrLn str >> display more

displayVersion :: IO ()
displayVersion = display ["Hanum CLI v" ++ version ++ " under the MIT License"]

displayBugReport :: IO ()
displayBugReport =
  display ["Report any problem to https://github.com/wisn/hanum/issues"]

displayError :: [String] -> IO ()
displayError str = (putStrLn $ "Error: " ++ combine str) >> exitFailure

humanizeList :: [String] -> String
humanizeList [] = []
humanizeList list = evaluate list (length list)
  where
  evaluate :: [String] -> Int -> String
  evaluate []     _ = []
  evaluate (x:xs) n
    | xs == []  = x
    | n == 2    = x ++ " and " ++ head xs
    | otherwise =
      if (length xs) == 1 && n > 2
        then x ++ ", and " ++ head xs
        else x ++ ", " ++ evaluate xs n

isExistCoupleOptions :: (String, String) -> Args.Arguments -> Bool
isExistCoupleOptions (oa, ob) args =
  (Args.isExistOption oa args) || (Args.isExistOption ob args)

runCLI :: [String] -> IO ()
runCLI input = do
  let args = Args.getArguments input

  if isExistCoupleOptions ("v", "version") args
    then displayVersion >> displayBugReport
    else if Args.isEmptyOption args || isExistCoupleOptions ("h", "help") args
      then displayHelp
      else print args

displayHelp :: IO ()
displayHelp = display
  [ ""
  , "Usage: hanum [options] [file] [arguments...]"
  , ""
  , "Options"
  , "  -v, --version              Display Hanum version on your machine"
  , "  -h, --help                 Display help menu as in your monitor"
  , "  -p, --presets  <string>    Define your linting rule presets path"
  , "  -o, --output   <string>    Save the result as a file"
  , ""
  , "Built with <3 in Indonesia"
  , ""
  ]
