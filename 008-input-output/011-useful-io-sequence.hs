-- - The sequence function takes a list of I/O actions and returns an I/O action
--   that will perform those actions one after the other. The result that this I/O
--   action yields will be a list of the results of all the I/O actions that were per-
--   formed.

main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- - The code above is exactly similar to the following code
--     main = do
--     a <- getLine
--     b <- getLine
--     c <- getLine
--     print [a,b,c]

--  - The results of both these versions are exactly the same. sequence
--    [getLine, getLine, getLine] makes an I/O action that will perform getLine
--    three times. If we bind that action to a name, the result is a list of all the re-
--    sults. So in this case, the result would be a list of three things that the user
--    entered at the prompt
--
-- - A common pattern with sequence is when we map functions like print or
--   putStrLn over lists. Executing map print [1,2,3,4] won’t create an I/O action,
--   but instead will create a list of I/O actions. Effectively, this is the same as
--   writing this:
--
--   [print 1, print 2, print 3, print 4]
--
-- - If we want to transform that list of I/O actions into an I/O action, we
--   must sequence it:
--
--   ghci> sequence $ map print [1,2,3,4,5]
--   1
--   2
--   3
--   4
--   5
--   [(),(),(),(),()]
--
--   But what’s with the [(),(),(),(),()] at the end of the output? Well,
--   when we evaluate an I/O action in GHCi, that action is performed, and
--   then its result is printed out, unless that result is (). That’s why evaluating
--   putStrLn "hehe" in GHCi just prints out hehe —putStrLn "hehe" yields (). But
--   when we enter getLine in GHCi, the result of that I/O action is printed out,
--   because getLine has a type of IO String
