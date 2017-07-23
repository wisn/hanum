module Main where

import qualified CLI as CLI
import qualified Overpass as Overpass

main :: IO ()
main = (CLI.display. CLI.getMessage) ["app.version"]
