-- # File and Streams
--
-- ## Getting strings from input streams

-- Program that takes some input and prints out only those lines that are shotrer than 10  characters

main = do
  contents <- getContents
  putStrLn $ shortLines contents

shortLines :: String -> String
shortLines = unlines . filter (\line -> length line <= 10) . lines
