module Main where

import Control.Monad (when)
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am i thinking of?"
  numberString <- getLine
  -- Can also use: unless (null numberString)
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry it was " ++ show randNumber

    -- We perform askForNumber recursively, but this time with the new generator
    -- that we got. This gives us an I/O action that’s just like the one we
    -- performed, except that it depends on a different generator.
    askForNumber newGen

-- -
