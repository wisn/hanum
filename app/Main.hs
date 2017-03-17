module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs

  if (null args)
    then display help
    else do
      let command = head args

      case command of "help"      -> display help
                      "-h"        -> display help
                      "--help"    -> display help
                      "about"     -> display about
                      "version"   -> display appVersion
                      "-v"        -> display appVersion
                      "--version" -> display appVersion
                      otherwise -> run args
