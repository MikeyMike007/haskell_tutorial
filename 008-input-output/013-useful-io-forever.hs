import Control.Monad
import Data.Char

-- - The forever function takes an I/O action and returns an I/O action that just
--   repeats the I/O action it got forever.

main = forever $ do
  putStrLn "Give me input"
  input <- getLine
  putStrLn $ map toUpper input
