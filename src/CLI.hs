module CLI
  ( display
  , getMessage
  , displayMessage
  ) where

messages :: [(String, String)]
messages =
  [ ( "app.name"
    , "Hanum"
    )
  , ( "app.description"
    , "An OpenStreetMap attributes linter with custom presets."
    )
  , ( "app.version"
    , "0.1.0.0"
    )
  , ( "app.author"
    , "Wisnu Adi Nurcahyo"
    )
  , ( "app.released"
    , "-"
    )
  ]

display :: [String] -> IO ()
display []         = return ()
display (str:more) = putStrLn str >> display more

getMessage :: [String] -> [String]
getMessage []            = []
getMessage (key:moreKey) = getFromMessage messages key []
  where getFromMessage :: [(String, String)] -> String -> [String] -> [String]
        getFromMessage []         _        _ = []
        getFromMessage (msg:more) expected res
          | (fst msg) == expected = (snd msg) : getMessage moreKey
          | otherwise             = getFromMessage more expected res

displayMessage :: String -> IO ()
displayMessage []  = return ()
displayMessage key = fromMessage messages key
  where fromMessage :: [(String, String)] -> String -> IO ()
        fromMessage []         _   = return ()
        fromMessage (msg:more) expected
          | (fst msg) == expected = display [snd msg]
          | otherwise             = fromMessage more expected
