module Hanum.Arguments (debug) where

import qualified Data.Map.Lazy as Map ( Map
                                      , fromList
                                      , toList
                                      , empty
                                      , insert
                                      , union
                                      )
import           System.Environment (getArgs)

type Options = Map.Map String String

data Status = Error
            | Warning
            | Success
            | Fail
            | Nil
            | Unknown
            deriving (Eq, Show, Read)

data Arguments = ArgNil
               | Arguments
                 { options          :: Options
                 , arguments        :: [String]
                 , unknownOptions   :: [String]
                 , unknownArguments :: [String]
                 }
               deriving (Eq, Show)

getArgumentsStatus :: Arguments -> Status
getArgumentsStatus ArgNil              = Nil
getArgumentsStatus (Arguments _ _ _ _) = Success

specialSingleOptions :: [String]
specialSingleOptions =
                     [ "-v"
                     , "--version"
                     , "-h"
                     , "--help"
                     ]

availableOptions :: [String]
availableOptions = specialSingleOptions ++
                 [ "-o"
                 , "--output"
                 ]

appArguments :: Arguments
appArguments = ArgNil

insertOptionsTo :: Arguments -> Options -> Arguments
insertOptionsTo args opts = case args of
  ArgNil
    -> Arguments
       { options          = Map.union opts Map.empty
       , arguments        = []
       , unknownOptions   = []
       , unknownArguments = []
       }
  ( Arguments { options          = o
              , arguments        = a
              , unknownArguments = uA
              , unknownOptions   = uO
              }
    )
    -> Arguments
       { options          = Map.union opts o
       , arguments        = a
       , unknownArguments = uA
       , unknownOptions   = uO
       }

{-insertAllOptionsTo :: Arguments -> [(String, String)] -> IO ()
insertAllOptionsTo args []         = return ()
insertAllOptionsTo args ((k,v):xs) =
   let chunk = insertOptionsTo args (k, v)
   in insertAllOptionsTo args xs-}

isOptions :: String -> Bool
isOptions (x:xs)
  | x == '-' && xs /= [] = True
  | otherwise            = False

isShortOptions :: String -> Bool
isShortOptions (x:y:_)
  | x == '-' && y /= '-' = True
  | otherwise            = False

isLongOptions :: String -> Bool
isLongOptions (x:y:_)
  | x == '-' && y == '-' = True
  | otherwise            = False

getShortOptions :: String -> Options
getShortOptions []       = Map.empty
getShortOptions (_:opts) = evaluate opts [] Map.empty
  where
  evaluate :: String -> String -> Options -> Options
  evaluate []     _ res = res
  evaluate (x:xs) l res
    | x == '='  = evaluate [] []  $ Map.insert l   xs res
    | otherwise = evaluate xs [x] $ Map.insert [x] [] res

getOptions :: String -> Options
getOptions [] = Map.empty
getOptions x
  | elem x sopts     = Map.fromList [(x, [])]
  | isShortOptions x = getShortOptions x
  | otherwise        = Map.empty
  where
  sopts = specialSingleOptions

{-getUserArguments :: [String] -> Arguments
getUserArguments []     = ArgNil
getUserArguments (x:xs) = do
  if isOptions x
    then getOptions x >> getUserArguments xs
    else getUserArguments xs-}

debug :: IO ()
debug = do
  input <- getArgs

  let opts = getOptions (if input == [] then [] else head input)

  putStrLn $ "Get one Options: " ++ show opts
