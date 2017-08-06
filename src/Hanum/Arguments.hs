module Hanum.Arguments ( Arguments
                       , getArguments
                       , isEmptyOption
                       , isExistOption
                       ) where

import qualified Data.Map.Lazy as Map ( Map
                                      , fromList
                                      , toList
                                      , empty
                                      , insert
                                      , union
                                      , member
                                      , difference
                                      , null
                                      )
import           System.Environment (getArgs)
import qualified System.FilePath.Posix as File (takeExtension)

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
                 , unknownOptions   :: Options
                 , unknownArguments :: [String]
                 }
               deriving (Eq, Show)

getArgumentsStatus :: Arguments -> Status
getArgumentsStatus ArgNil              = Nil
getArgumentsStatus (Arguments _ _ _ _) = Success

specialSingleOptions :: Options
specialSingleOptions = Map.fromList
                     [ ("v", [])
                     , ("version", [])
                     , ("h", [])
                     , ("help", [])
                     ]

availableOptions :: Options
availableOptions = Map.union spcl $
  Map.fromList [ ("o", [])
               , ("output", [])
               , ("p", [])
               , ("persets", [])
               ]
  where
  spcl = specialSingleOptions

insertOptionsTo :: Arguments -> Options -> Arguments
insertOptionsTo args opts = case args of
  ArgNil
    -> Arguments
       { options          = Map.union opts Map.empty
       , arguments        = []
       , unknownOptions   = Map.empty
       , unknownArguments = []
       }
  ( Arguments { options          = o
              , arguments        = a
              , unknownOptions   = uO
              , unknownArguments = uA
              }
    )
    -> Arguments
       { options          = Map.union opts o
       , arguments        = a
       , unknownOptions   = uO
       , unknownArguments = uA
       }

insertUnkownOptionsTo :: Arguments -> Options -> Arguments
insertUnkownOptionsTo args opts = case args of
  ArgNil
    -> Arguments
       { options          = Map.empty
       , arguments        = []
       , unknownOptions   = Map.union opts Map.empty
       , unknownArguments = []
       }
  ( Arguments { options          = o
              , arguments        = a
              , unknownOptions   = uO
              , unknownArguments = uA
              }
    )
    -> Arguments
       { options          = o
       , arguments        = a
       , unknownOptions   = Map.union opts uO
       , unknownArguments = uA
       }

insertArgumentsTo :: Arguments -> [String] -> Arguments
insertArgumentsTo args x = case args of
  ArgNil
    -> Arguments
      { options          = Map.empty
      , arguments        = x
      , unknownOptions   = Map.empty
      , unknownArguments = []
      }
  ( Arguments { options          = o
              , arguments        = a
              , unknownOptions   = uO
              , unknownArguments = uA
              }
   )
    -> Arguments
      { options          = o
      , arguments        = a ++ x
      , unknownOptions   = uO
      , unknownArguments = uA
      }

insertUnknownArgumentsTo :: Arguments -> [String] -> Arguments
insertUnknownArgumentsTo args x = case args of
  ArgNil
    -> Arguments
      { options          = Map.empty
      , arguments        = []
      , unknownOptions   = Map.empty
      , unknownArguments = x
      }
  ( Arguments { options          = o
              , arguments        = a
              , unknownOptions   = uO
              , unknownArguments = uA
              }
   )
    -> Arguments
      { options          = o
      , arguments        = a
      , unknownOptions   = uO
      , unknownArguments = uA ++ x
      }

isEmptyOption :: Arguments -> Bool
isEmptyOption ArgNil                         = True
isEmptyOption (Arguments { options = opts }) = Map.null opts

isExistOption :: String -> Arguments -> Bool
isExistOption _  ArgNil = False
isExistOption [] _      = False
isExistOption k (Arguments { options = opts })
  | Map.member k opts = True
  | otherwise         = False

isOptions :: String -> Bool
isOptions x
  | (length x) < 2                          = False
  | (isShortOptions x) || (isLongOptions x) = True
  | otherwise                               = False

isShortOptions :: String -> Bool
isShortOptions (x:y:_)
  | x == '-' && y /= '-' = True
  | otherwise            = False

isLongOptions :: String -> Bool
isLongOptions (x:y:z)
  | x == '-' && y == '-' && z /= [] = True
  | otherwise                       = False

getShortOptions :: String -> String -> Options
getShortOptions []       _  = Map.empty
getShortOptions (_:opts) [] = evaluate opts [] Map.empty
  where
  evaluate :: String -> String -> Options -> Options
  evaluate []     _ res = res
  evaluate (x:xs) l res
    | x == '='  = evaluate [] []  $ Map.insert l   xs res
    | otherwise = evaluate xs [x] $ Map.insert [x] [] res
getShortOptions (_:opts) v  = evaluate opts [] v Map.empty
  where
  evaluate :: String -> String -> String -> Options -> Options
  evaluate []     k v res = Map.insert k v res
  evaluate (x:xs) k v res
    | xs == []  = evaluate [] [x] v res
    | otherwise = evaluate xs [x] v $ Map.insert [x] [] res

getLongOptions :: String -> String -> Options
getLongOptions []         _  = Map.empty
getLongOptions (_:_:opts) [] = evaluate opts [] []
  where
  evaluate :: String -> String -> String -> Options
  evaluate []     k  v = Map.fromList [(k, v)]
  evaluate (x:xs) k  v
    | x == '='  = evaluate [] k xs
    | otherwise = evaluate xs (k ++ [x]) v
getLongOptions (_:_:opts) v = Map.fromList [(opts, v)]

getOptions :: String -> String -> Options
getOptions [] _ = Map.empty
getOptions k v
  | isShortOptions k   = getShortOptions k v
  | isLongOptions k    = getLongOptions k v
  | otherwise          = Map.empty

getOptionsKey :: String -> String
getOptionsKey [] = []
getOptionsKey input
  | isShortOptions input = evaluate (safeTail input) []
  | isLongOptions  input = evaluate ((safeTail. safeTail) input) []
  where
  safeTail :: String -> String
  safeTail [] = []
  safeTail xs = tail xs
  evaluate :: String -> String -> String
  evaluate []     res = res
  evaluate (x:xs) res
    | x == '='  = evaluate [] res
    | otherwise = evaluate xs (res ++ [x])

safeHead :: [String] -> String
safeHead [] = []
safeHead xs = head xs

safeTail :: [String] -> [String]
safeTail [] = []
safeTail xs = tail xs

getUserArguments :: [String] -> (Options, [String])
getUserArguments []    = (Map.empty, [])
getUserArguments input = evaluate input Map.empty []
  where
  evaluate :: [String] -> Options -> [String] -> (Options, [String])
  evaluate []     opts args = (opts, args)
  evaluate (x:xs) opts args
    | isOptions x =
      let key = getOptionsKey x
      in if not (Map.member key availableOptions)
        then evaluate xs (combineOptions opts x []) args
        else if Map.member key sOpts
          then evaluate xs (Map.union opts (Map.fromList [(key, [])])) args
          else if (isOptions (safeHead xs)) || elem '=' x
            then evaluate xs (combineOptions opts x []) args
            else evaluate (safeTail xs) (combineOptions opts x xs) args
    | otherwise = evaluate xs opts (args ++ [x])
    where
    sOpts = specialSingleOptions
    combineOptions opts k [] = Map.union opts (getOptions k [])
    combineOptions opts k xs = Map.union opts (getOptions k (head xs))

getKnownArguments :: [String] -> ([String], [String])
getKnownArguments [] = ([], [])
getKnownArguments input = evaluate input [] []
  where
  evaluate :: [String] -> [String] -> [String] -> ([String], [String])
  evaluate []     args uArgs = (args, uArgs)
  evaluate (x:xs) args uArgs
    | (File.takeExtension x) == [] = evaluate xs args (uArgs ++ [x])
    | otherwise                    = evaluate xs (args ++ [x]) uArgs

getArguments :: [String] -> Arguments
getArguments input =
  let (pOpts, pArgs) = getUserArguments input
      uOpts          = Map.difference pOpts availableOptions
      opts           = Map.difference pOpts uOpts
      (args, uArgs)  = getKnownArguments pArgs
      inOpts         = insertOptionsTo ArgNil opts
      inUOpts        = insertUnkownOptionsTo inOpts uOpts
      inArgs         = insertArgumentsTo inUOpts args
      inUArgs        = insertUnknownArgumentsTo inArgs uArgs
  in inUArgs
