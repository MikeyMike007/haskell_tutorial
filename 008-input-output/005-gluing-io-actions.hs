-- # Gluing I/O Actions
--
-- - We can use return in combination with <- to bind stuff to names:

main = do
  a <- return "hell"
  b <- return "yeah"
  putStr $ a ++ " " ++ b

-- - So you see, return is sort of the opposite of <- . While return takes a value
--   and wraps it up in a box, <- takes a box (and performs it) and takes the value
--   out of it, binding it to a name. But doing this is kind of redundant, especially
--   since you can use let in do blocks to bind to names, like so:
--
--   main = do
--   let a = "hell"
--   b = "yeah"
--   putStrLn $ a ++ " " ++ b
--
-- -  When dealing with I/O do blocks, we mostly use return either because we
--    need to create an I/O action that doesn’t do anything or because we don’t
--    want the I/O action that’s made up from a do block to have the result value
--    of its last action. When we want it to have a different result value, we use
--    return to make an I/O action that always yields our desired result, and we
--    put it at the end.
