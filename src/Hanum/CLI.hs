module Hanum.CLI
  ( getArgs
  , run
  ) where

import           Control.Applicative ((<*>))
import           Control.Monad (unless)
import           Data.Char (ord)
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, exitFailure)

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

data Status = Init | Pass | Done | Warning | Error deriving (Eq)

isShortOptions :: String -> Bool
isShortOptions (c:_)
  | c == '-'  = True
  | otherwise = False

getShortOptions :: String -> (Status, [(String, String)], String)
getShortOptions [] = ( Error, [],
                       combine
                       [ "EmptyOption: "
                       , "Got a single dash sign (-) without any options!"
                       ]
                      )
getShortOptions opts = getOpts Init opts [] []
    where
    getOpts :: Status -> String -> [String] -> String
            -> (Status, [(String, String)], String)
    getOpts Error  _  _    msg  = (Error, [], msg)
    getOpts _      [] opts args = (Done, pack opts, args)
      where
      pack :: [String] -> [(String, String)]
      pack []     = []
      pack (c:cs) = [(c, [])] ++ pack cs
    getOpts status (c:cs) opts args
      | c == '='     = getOpts Done [] opts cs
      | isAlphabet c = getOpts Pass cs (opts ++ [[c]]) args
      | otherwise
        = getOpts Error [] []
          ( combine
          [ "NonAlphabet: "
          , "An option must be written in alphabet!"
          ])

isLongOptions :: String -> Bool
isLongOptions opt
  | length (opt) < 2                       = False
  | (opt !! 0) == '-' && (opt !! 1) == '-' = True
  | otherwise                              = False

parseArgs :: [String] -> (Status, [(String, String)], [String])
parseArgs []         = (Done, [], [])
parseArgs opts = getArgs opts 0 [] [] (Init, [], [])
  where
  getArgs :: [String] -> Int -> String -> String
          -> (Status, [(String, String)], [String])
          -> (Status, [(String, String)], [String])
  getArgs _  _ _ _ (Error, _, msg)    = (Error, [], msg)
  getArgs _  _ _ _ (Done, opts, args) = (Done, opts, args)
  getArgs [] _ k v (_, opts, args)
    | k /= []   = getArgs [] 0 [] [] (Done, opts ++ [(k, v)], args)
    | v /= []   = getArgs [] 0 [] [] (Done, opts, args ++ [v])
    | otherwise = getArgs [] 0 [] [] (Done, opts, args)
  getArgs w@(x:xs) i k v (status, opts, args)
    | i == 1 = getArgs w 0 [] [] (status, opts ++ [(k, v)], args)
    | i == 2 = getArgs w 0 [] [] (status, opts, args ++ [v])
    | (length x) == 0  = getArgs xs i k v (status, opts, args)
    | isShortOptions x =
      let (s, o, a) = getShortOptions (tail x)
      in if s == Error
        then getArgs [] 0 [] [] (Error, [], [a])
        else if (length o) == 1
          then let key = (fst. head) o
                   tl  = if xs == [] then [] else tail xs
                   hd  = if xs == [] then [] else head xs
               in if a == []
                  then getArgs tl 1 key hd (Pass, opts, args)
                  else getArgs xs 1 key a (Pass, opts, args)
          else getArgs xs 0 [] [] (Pass, opts ++ o, args)
    | otherwise = getArgs xs 2 [] x (Pass, opts, args)

between :: Int -> (Int, Int) -> Bool
between n (x, y)
  | x < n && n < y = True
  | otherwise      = False

isAlphabet :: Char -> Bool
isAlphabet c  =
  let ascii = ord c
  in if (ascii `between` (64, 91)) || (ascii `between` (96, 123))
    then True
    else False

isNumber :: Char -> Bool
isNumber c =
  let ascii = ord c
  in if (ascii `between` (47, 58))
    then True
    else False

options :: [String]
options =
  [ "v", "version"
  , "h", "help"
  , "p", "presets"
  , "o", "output"
  ]

getCoupleOptions :: [(String, String)] -> (String, String) -> Bool
getCoupleOptions [] _ = False
getCoupleOptions (x:xs) (a, b) =
  let k = fst x
  in if k == a || k == b
    then True
    else getCoupleOptions xs (a, b)

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

getUnknownParams :: [(String, String)] -> [String] -> [String]
getUnknownParams [] _ = []
getUnknownParams ((p,_):ps) x
  | p `elem` x = getUnknownParams ps x
  | otherwise  = [p] ++ getUnknownParams ps x

run :: [String] -> IO ()
run input = do
  let (status, opts, args) = parseArgs input
  case status of
    Error -> let msg = args
             in displayError msg
    otherwise -> if ((length opts) == 0 && (length args) == 0)
      || getCoupleOptions opts ("h", "help")
      then displayVersion
        >> displayHelp
        >> displayBugReport
      else if getCoupleOptions opts ("v", "version")
        then displayVersion
        else displayVersion
          >> displayHelp
          >> displayBugReport
          >> putStrLn []
          >> putStr "Options:   " >> print opts
          >> putStr "Arguments: " >> print args

  let unknown = getUnknownParams opts options
  unless ((length unknown) == 0) $ do
    display [ ""
            , "Warning: Unknown options passed:"
            , "  " ++ humanizeList unknown
            ]

displayHelp :: IO ()
displayHelp = display
  [ ""
  , "Usage: hanum [options] [arguments] file"
  , ""
  , "Options"
  , "  -v, --version              Display Hanum version on your machine"
  , "  -h, --help                 Display help menu as in your monitor"
  , "  -p, --presets  <string>    Define your linting rule presets path"
  , "  -o, --output   <string>    Save the result as a file"
  , ""
  , "Built with <3 from Indonesia"
  , ""
  ]
