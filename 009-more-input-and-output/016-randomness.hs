import System.Random

-- # Randomness
--
-- - In the System.Random module. random's type signature is
--
--   random :: (RandomGen g, Random a) => g -> (a, g)
--
--   Two typeclasses i.e.
--
--   RandomGen: Types that can act as sources of randomness
--   Random: Types whose values can be random
--
-- - To use our random function, we need a random genrator. System.Random
--   exports a type called StdGen which is an instance of the RandomGen type
--   class.
--
-- - ghci> random (mkStdGen 100)
--   <interactive>:1:0:
--   Ambiguous type variable `a' in the constraint:
--   `Random a' arising from a use of `random' at <interactive>:1:0-20
--   Probable fix: add a type signature that fixes these type variable(s)
--
--   This means that the random function can return a value of any type
--   that’s part of the Random type class, so we need to inform Haskell which type
--   we want.
--
-- - ghci> random (mkStdGen 100) :: (Int, StdGen)
--   (-1352021624,651872571 1655838864)
--
--   Finally, a number that looks kind of random! The first component of
--   the tuple is our number, and the second component is a textual representa-
--   tion of our new random generator.
--
-- ## Tossing a coin
--
-- - Let’s make a function that simulates tossing a coin three times. If random
--   didn’t return a new generator along with a random value, we would need to make
--   this function take three random generators as a parameter and re- turn coin
--   tosses for each of them. But if one generator can make a random value of type
--   Int (which can take on a load of different values), it should be able to make
--   three coin tosses (which can have only eight different end re- sults). So this
--   is where random returning a new generator along with a value comes in handy.
--
-- - We call random with the generator we got as a parameter to get a coin
--   and a new generator. Then we call it again, only this time with our new
--   generator, to get the second coin. We do the same for the third coin.
--   Had we called it with the same generator every time, all the coins would
--   have had the same value, so we would get only (False, False, False) or
--   (True, True, True) as a result.

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gren =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

-- - Running the program gives:
--
--   ghci> threeCoins (mkStdGen 21)
--   (True,True,True)
--
--   ghci> threeCoins (mkStdGen 22)
--   (True,False,True)
--
--   ghci> threeCoins (mkStdGen 943)
--   (True,False,True)
--
--   ghci> threeCoins (mkStdGen 944)
--   (True,True,True)
