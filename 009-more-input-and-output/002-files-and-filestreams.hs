import Data.Char

-- # Files and filestreams
--
-- ## Getting strings from input streams
--
-- - Let’s take a look at an I/O action that makes processing input streams easier
--   by allowing us to treat them as normal strings: getContents . getContents reads
--   everything from the standard input until it encounters an end-of-file charac-
--   ter. Its type is getContents :: IO String . What’s cool about getContents is that
--   it does lazy I/O. This means that when we do foo <- getContents , getContents
--   doesn’t read all of the input at once, store it in memory, and then bind it to
--   foo. No, getContents is lazy! It will say, “Yeah yeah, I’ll read the input from the
--   terminal later as we go along, when you really need it!”
--
-- - When the result of getContents is bound to contents , it’s not represented
--   in memory as a real string, but more like a promise that the string will be
--   produced eventually. When we map toUpper over contents , that’s also a pro-
--   mise to map that function over the eventual contents. Finally, when putStr
--   happens, it says to the previous promise, “Hey, I need a caps-locked line!”
--   It doesn’t have any lines yet, so it says to contents , “How about getting a line
--   from the terminal?” And that’s when getContents actually reads from the ter-
--   minal and gives a line to the code that asked it to produce something tangi-
--   ble. That code then maps toUpper over that line and gives it to putStr , which
--   prints the line. And then putStr says, “Hey, I need the next line—come on!”
--   This repeats until there’s no more input, which is signified by an end-of-file
--   character.

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ map toUpper contents

-- - Running the program gives:
--   $ ./capslocker < haiku.txt
--   I'M A LIL' TEAPOT
--   WHAT'S WITH THAT AIRPLANE FOOD, HUH?
--   IT'S SO SMALL, TASTELESS
