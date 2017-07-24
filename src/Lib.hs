module Lib where

{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

import Data.Aeson (decode, FromJSON, parseJSON, withObject, (.:))
import Data.Char (isLower)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath.Posix (takeExtension, (</>))
import qualified Data.ByteString.Lazy as BL (readFile)
import qualified Data.Map.Strict as Map (Map, toList, lookup)

data Attributes = Attributes {
  tags :: (Map.Map String String)
} deriving (Generic, Show)

instance FromJSON Attributes where
  parseJSON = withObject "attributes" $ \o -> do
    elements <- o .: "elements"
    tags     <- elements !! 0 .: "tags"
    return Attributes{..}

data Presets = Presets {
  presets :: (Map.Map String String)
} deriving (Generic, Show)

instance FromJSON Presets where
  parseJSON = withObject "presets" $ \o -> do
    presets <- o .: "presets"
    return Presets{..}

getFirstPairs :: [(String, String)] -> [String]
getFirstPairs []           = []
getFirstPairs (pairs:next) = [fst pairs] ++ getFirstPairs next

extract :: [(String, String)] -> [String]
extract []           = []
extract (pairs:next) = [(fst pairs) ++ (snd pairs)] ++ extract next

display :: [String] -> IO ()
display message = (putStr . unlines) message

beGreen :: String -> String
beGreen text = "\x1b[32m" ++ text ++ "\x1b[0m"

beBlue :: String -> String
beBlue text = "\x1b[34m" ++ text ++ "\x1b[0m"

beYellow :: String -> String
beYellow text = "\x1b[33m" ++ text ++ "\x1b[0m"

beRed :: String -> String
beRed text = "\x1b[31m" ++ text ++ "\x1b[0m"

showSuccess :: String -> String
showSuccess message =
  let tag = "[" ++ beGreen "SUCCESS" ++ "]"
  in  tag ++ " " ++ message

showInfo :: String -> String
showInfo message =
  let tag = "[" ++ beBlue "INFO" ++ "]"
  in  tag ++ " " ++ message

showWarning :: String -> String
showWarning message =
  let tag = "[" ++ beYellow "WARNING" ++ "]"
  in  tag ++ " " ++ message

showError :: String -> String
showError message =
  let tag = "[" ++ beRed "ERROR" ++ "]"
  in  tag ++ " " ++ message

parseArgs :: [String] -> (String, String)
parseArgs (path':preset') = do
  let extension = takeExtension path'
      path      = if (null extension)
                    then []
                    else path'
      preset   = if (null preset')
                    then []
                    else (preset' !! 0)
  (path, preset)

getTags :: Attributes -> (Map.Map String String)
getTags (Attributes {tags = attributes}) = attributes

getPresets :: Presets -> [(String, String)]
getPresets (Presets {presets = rules}) = Map.toList rules

runLinter :: [(String, String)] -> (Map.Map String String) -> IO ()
runLinter rules attrs = runPresets rules attrs 0
  where runPresets :: [(String, String)] -> (Map.Map String String) -> Int -> IO ()
        runPresets [] _ errors = do
          putStrLn []
          if errors > 0
            then do
              (putStrLn . beRed) ((show errors) ++ " error(s) found!")
              exitFailure
            else do
              (putStrLn . beGreen) "It's all clean!"
              exitSuccess
        runPresets (x:xs) attr err = do
          let ruleKey  = fst x
              ruleFn   = snd x
              ruleFn'  = head (splitOn " " ruleFn)

          if isItLintFn ruleFn'
            then do
              let ruleTest =
                    case ruleFn' of
                      "required"        -> lintRequired ruleKey attr
                      "shouldLowercase" -> lintShouldLowercase ruleKey attr
                      "dontSameWith"    -> lintDontSameWith ruleKey ruleFn attr
                      otherwise         -> (True, [])

                  testResult = fst ruleTest
                  testMsg    = snd ruleTest

              if testResult == True
                then return ()
                else do
                  putStrLn testMsg
                  runPresets xs attr (err + 1)
            else do
              let msg =  "The \"" ++ ruleFn ++ "\" rule function in key \""
                      ++ ruleKey
                      ++ "\" didn't exist!"
              (putStrLn . showWarning) msg
              runPresets xs attr err

lint :: String -> String -> IO ()
lint file presets = do
  if (null presets)
    then do
      (putStrLn . showError) "Default presets currently didn't available!"
      exitFailure
    else do
      let presetsExtension = takeExtension presets
      if (presetsExtension /= ".json")
        then do
          (putStrLn . showError) "Presets must be a JSON!"
          exitFailure
        else do
          json <- BL.readFile file
          let osm = decode json :: Maybe Attributes
          case osm of
            Nothing
              -> do
                (putStrLn . showError) "Can't find out the attributes!"
                exitFailure
            Just attributes
              -> do
                presetExist <- doesFileExist presets

                if presetExist
                  then do
                    let attr = getTags attributes
                    preset <- BL.readFile presets
                    let prst = decode preset :: Maybe Presets
                    case prst of
                      Nothing
                        -> do
                          (putStrLn . showError) "Can't find out the presets!"
                          exitFailure
                      Just rules
                        -> do
                          let linterRule = getPresets rules
                          runLinter linterRule attr
                  else do
                    (putStrLn . showError) "Presets didn't exist!"
                    exitFailure

run :: [String] -> IO ()
run args = do
  let parsedArgs = parseArgs args
      path       = fst parsedArgs
      extension  = takeExtension path
      preset    = snd parsedArgs

  if (null path)
    then (putStrLn . showError) "It's not both a command nor a file!"
    else if (extension /= ".json" && extension /= ".geojson")
            then do
              (putStrLn . showError) "It's not a JSON file!"
              exitFailure
            else do
              pwd <- getCurrentDirectory
              let file = pwd </> path

              fileExist <- doesFileExist file
              if fileExist
                then lint file preset
                else do
                  (putStrLn . showError) "File doesn't exist!"
                  exitFailure

isItLintFn :: String -> Bool
isItLintFn fun = do
  case fun of
    "required"     -> True
    "dontSameWith" -> True
    otherwise      -> False

lintRequired :: String -> (Map.Map String String) -> (Bool, String)
lintRequired key tags =
  let lookupKey = Map.lookup key tags
  in case lookupKey of
    Nothing
      -> ( False
         , showError $ "Required key: " ++ key)
    Just k  -> (True, [])

lintDontSameWith :: String -> String -> (Map.Map String String) -> (Bool, String)
lintDontSameWith key key' tags =
  let lookupKey  = Map.lookup key tags
      getKey     = last (splitOn " " key')
      lookupKey' = Map.lookup getKey tags
  in case lookupKey of
    Nothing
      -> ( False
         , showError $ "Can't find out key \"" ++ key ++ "\"!")
    Just k
      -> do
        case lookupKey' of
          Nothing
            -> ( False
               , showError $ "Can't find out key \"" ++ key' ++ "\"!")
          Just l -> do
            if k /= l
              then (True, [])
              else ( False
              , showError $ "The \"" ++ key ++ "\" is same with \"" ++ getKey ++ "\"!")

lintShouldLowercase :: String -> (Map.Map String String) -> (Bool, String)
lintShouldLowercase key tags =
  let content = Map.lookup key tags
  in case content of
    Nothing -> (True, [])
    Just v  -> if (all isLower v)
      then (True, [])
      else ( False
           , showError $ "The \"" ++ key ++ "\" should be lowercase!")

versionFull :: String
versionFull = "0.0.1-alpha"

versionMajor :: Int
versionMajor = 0

versionMinor :: Int
versionMinor = 0

versionRelease :: Int
versionRelease = 1

versionAttribute :: String
versionAttribute = "alpha"

releaseDate :: String
releaseDate = "-"

commands :: [(String, String)]
commands =
  [ (beGreen "  help", "          Display this help message")
  , (beGreen "  about", "         Display about Hanum")
  , (beGreen "  version", "       Display version of Hanum") ]

help :: [String]
help = [ "Usage: hanum PATH [PRESET]"
       , ""
       , "  Commands"
       , (unlines . extract) commands]

about :: [String]
about = [ "Hanum version " ++ versionFull
        , "Released at " ++ releaseDate ]

appVersion :: [String]
appVersion = ["Hanum " ++ versionFull]
