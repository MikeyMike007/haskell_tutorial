-- # Random
--
-- - The best way to get two different strings is to use the newStdGen action,
--   which splits our current random generator into two generators. It updates the
--   global random generator with one of them and yields the other as its result.

import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen' <- newStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen')

-- - Not only do we get a new random generator when we bind newStdGen to something,
--   but the global one gets updated as well. This means that if we do getStdGen
--   again and bind it to something, we’ll get a generator that’s not the same as
--   gen.
