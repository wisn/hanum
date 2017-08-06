module Main where

import qualified Hanum as Hanum

main :: IO ()
main = Hanum.runCLI =<< Hanum.getArgs
