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

{-|
  Options is a "pair" of key and value.
  The key storing the option name and the value obviously storing it's value.
-}
type Options = Map.Map String String

{-|
  Arguments is a record data type that used for storing user arguments.
  It's have an ArgNil when there is no data and Arguments if there is a data.
  It's storing any options and arguments that passed by the user.

  For example, when user pass
  ```
  hanum -o "checked.json" -p "sample.presets.json" data.geojson --opts args
  ```
  the stored data looks like this.

  Arguments
  { options = fromList [("o", "checked.json"), ("p", "sample.presets.json")]
  , arguments        = ["data.geojson"]
  , unknownOptions   = fromList [("opts", [])]
  , unknownArguments = ["args"]
  }

  Hanum will use this information in the main program.
-}
data Arguments = ArgNil
               | Arguments
                 { options          :: Options
                 , arguments        :: [String]
                 , unknownOptions   :: Options
                 , unknownArguments :: [String]
                 }
               deriving (Eq, Show)

-- |specialSingleOptions provided any options that doesn't need any value.
specialSingleOptions :: Options
specialSingleOptions = Map.fromList
                     [ ("v", [])
                     , ("version", [])
                     , ("h", [])
                     , ("help", [])
                     ]

-- |availableOptions provided all supported options.
availableOptions :: Options
availableOptions = Map.union spcl $
  Map.fromList [ ("o", [])
               , ("output", [])
               , ("p", [])
               , ("presets", [])
               ]
  where
  spcl = specialSingleOptions

-- |insertOptionsTo insert an options data in the Arguments record.
insertOptionsTo :: Arguments -> Options -> Arguments
insertOptionsTo args opts = case args of
  ArgNil
    -> Arguments
       { options          = Map.union opts Map.empty
       , arguments        = []
       , unknownOptions   = Map.empty
       , unknownArguments = []
       }
  ( Arguments { options = o } )
    -> args { options = Map.union opts o }

-- |insertUnkownOptionsTo insert an unknownOptions data in the Arguments.
-- record.
insertUnkownOptionsTo :: Arguments -> Options -> Arguments
insertUnkownOptionsTo args opts = case args of
  ArgNil
    -> Arguments
       { options          = Map.empty
       , arguments        = []
       , unknownOptions   = Map.union opts Map.empty
       , unknownArguments = []
       }
  ( Arguments { unknownOptions = uO } )
    -> args { unknownOptions = Map.union opts uO }

-- |insertArgumentsTo insert an arguments data in the Arguments record.
-- An "arguments" mean a known user arguments which is a file name.
insertArgumentsTo :: Arguments -> [String] -> Arguments
insertArgumentsTo args x = case args of
  ArgNil
    -> Arguments
      { options          = Map.empty
      , arguments        = x
      , unknownOptions   = Map.empty
      , unknownArguments = []
      }
  ( Arguments { arguments = a } )
    -> args { arguments = a ++ x }

-- |insertUnknownArgumentsTo insert an unknownArguments data in the Arguments
-- record.
insertUnknownArgumentsTo :: Arguments -> [String] -> Arguments
insertUnknownArgumentsTo args x = case args of
  ArgNil
    -> Arguments
      { options          = Map.empty
      , arguments        = []
      , unknownOptions   = Map.empty
      , unknownArguments = x
      }
  ( Arguments { unknownArguments = uA } )
    -> args { unknownArguments = uA ++ x }

-- |isEmptyOption checking whether there is any options or not in the Arguments
-- record.
isEmptyOption :: Arguments -> Bool
isEmptyOption ArgNil                         = True
isEmptyOption (Arguments { options = opts }) = Map.null opts

-- |isExistOption checking whether an option exists or not in the Arguments
-- record.
isExistOption :: String -> Arguments -> Bool
isExistOption _  ArgNil = False
isExistOption [] _      = False
isExistOption k (Arguments { options = opts })
  | Map.member k opts = True
  | otherwise         = False

-- |isOptions checking whether it's an option or not.
isOptions :: String -> Bool
isOptions x
  | (length x) < 2                          = False
  | (isShortOptions x) || (isLongOptions x) = True
  | otherwise                               = False

{-|
  isShortOptions checking whether it's a short option or not.
  For example, `hanum -v` have a short option `v`.
-}
isShortOptions :: String -> Bool
isShortOptions (x:y:_)
  | x == '-' && y /= '-' = True
  | otherwise            = False

{-|
  isLongOptions checking whether it's a long option or not.
  For example, `hanum --version` have a long option `version`.
-}
isLongOptions :: String -> Bool
isLongOptions (x:y:z)
  | x == '-' && y == '-' && z /= [] = True
  | otherwise                       = False

{-|
  getShortOptions take any short options from the user arguments.

  For example, `hanum -abcd=5 -p "presets.json"` will produce:

  Arguments
  { options          = fromList [("p", "presets.json")]
  , arguments        = []
  , unknownOptions   = fromList [("a",""),("b",""),("c",""),("d","5")]
  , unknownArguments = []
  }
-}
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

{-|
  getLongOptions take any long options from the user arguments.

  For example, `hanum --output="result.json" --presets "presets.json"` will
  produce:

  Arguments
  { options = fromList [("output", "result.json"), ("presets", "presets.json")]
  , arguments        = []
  , unknownOptions   = fromList []
  , unknownArguments = []
  }
-}
getLongOptions :: String -> String -> Options
getLongOptions []         _  = Map.empty
getLongOptions (_:_:opts) [] = evaluate opts [] []
  where
  evaluate :: String -> String -> String -> Options
  evaluate []     k  v = Map.fromList [(k, v)]
  evaluate (x:xs) k  v
    | x == '='  = evaluate [] k xs
    | otherwise = evaluate xs (k ++ [x]) v
getLongOptions (_:_:opts) v  = Map.fromList [(opts, v)]

-- |getOptions checking whether it's a short option or a long option then run
-- the getShortOptions or getLongOptions.
getOptions :: String -> String -> Options
getOptions [] _ = Map.empty
getOptions k v
  | isShortOptions k = getShortOptions k v
  | isLongOptions k  = getLongOptions k v
  | otherwise        = Map.empty

-- |getOptionsKey take the "key" only from the user arguments.
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

-- |safeHead take the head safely which won't raise an error when .
safeHead :: [String] -> String
safeHead [] = []
safeHead xs = head xs

-- |safeTail take the tail safely which won't raise an error.
safeTail :: [String] -> [String]
safeTail [] = []
safeTail xs = tail xs

-- |getUserArguments evaluates the user arguments from the main program.
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

-- |getKnownArguments takes any files path from the user arguments.
getKnownArguments :: [String] -> ([String], [String])
getKnownArguments [] = ([], [])
getKnownArguments input = evaluate input [] []
  where
  evaluate :: [String] -> [String] -> [String] -> ([String], [String])
  evaluate []     args uArgs = (args, uArgs)
  evaluate (x:xs) args uArgs
    | (File.takeExtension x) == [] = evaluate xs args (uArgs ++ [x])
    | otherwise                    = evaluate xs (args ++ [x]) uArgs

-- TODO: Use mutable state instead
-- So we won't saw something like this again
-- |getArguments evaluates then return the Arguments from user arguments.
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
