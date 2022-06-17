-- # Randomness and I/O

-- - You may be wondering what this section has to do with I/O. We haven’t done
--   anything concerning I/O so far. We’ve always made our random num- ber generator
--   manually by creating it with some arbitrary integer. The prob- lem is that if
--   we do that in our real programs, they will always return the same random
--   numbers, which is no good for us. That’s why System.Random offers the getStdGen
--   I/O action, which has a type of IO StdGen . It asks the system for some initial
--   data and uses it to jump-start the global generator. getStdGen fetches that
--   global random generator when you bind it to something.

import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)

-- - Running the program gives:
--   $ ./random_string
--   pybphhzzhuepknbykxhe
--   $ ./random_string
--   eiqgcxykivpudlsvvjpg
--   $ ./random_string
--   nzdceoconysdgcyqjruo
--   $ ./random_string
--   bakzhnnuzrkgvesqplrx
