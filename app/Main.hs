module Main where

import qualified Hanum as Hanum

main :: IO ()
main = Hanum.run =<< Hanum.getArgs
