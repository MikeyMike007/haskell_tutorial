-- # Files and streams
--
-- ## Input redirection
--
-- - Create a file called haiku.txt:
--
--    I'm a lil' teapot
--    What's with that airplane food, huh?
--    It's so small, tasteless

import Control.Monad
import Data.Char

main = forever $ do
  input <- getLine
  putStrLn $ map toUpper input

-- - Now, run:
--   $ ghc --make capslocker
--   [1 of 1] Compiling Main ( capslocker.hs, capslocker.o )
--   Linking capslocker ...
--   $ ./capslocker < haiku.txt
--   I'M A LIL' TEAPOT
--   WHAT'S WITH THAT AIRPLANE FOOD, HUH?
--   IT'S SO SMALL, TASTELESS
--   capslocker <stdin>: hGetLine: end of file
