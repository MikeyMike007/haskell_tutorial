module Main where

import Control.Monad (unless, when)
import System.Random

-- It’s very similar to the previous version, but instead of making a function
-- that takes a generator and then calls itself recursively with the new updated
-- generator, we do all the work in main . After telling the user whether he was
-- correct in his guess, we update the global generator and then call main again.
-- Both approaches are valid, but I like the first one more since it does less
-- stuff in main and also provides a function I can reuse easily.

main :: IO ()
main = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am i thinking off?"
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main
