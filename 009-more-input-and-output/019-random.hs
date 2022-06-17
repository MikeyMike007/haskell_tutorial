-- # Random

-- - But you need to be careful. Just performing getStdGen twice will ask the system
--   for the same global generator twice. Suppose we do this:

import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen2 <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen2)

--   We will get the same string printed out twice!
