-- # Gluing I/O Actions
--

main :: IO ()
main = do
  putStrLn "Please write something: "
  sentence <- getLine
  if null sentence
    then return ()
    else do
      putStrLn $ reverseWords sentence
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- - First, we get a line from the terminal by performing
--   getLine and call that line line . Next we have a conditional expression. Re-
--   member that in Haskell, every if must have a corresponding else , because
--   every expression must have some sort of value. Our if says that when a con-
--   dition is true (in our case, the line that we entered is blank), we perform one
--   I/O action; when it isn’t true, the I/O action under the else is performed.
--   Because we need to have exactly one I/O action after the else , we use a
--   do block to glue together two I/O actions into one. We could also write that
--   part as follows:
--
--   else (do
--    putStrLn $ reverseWords line
--    main)
--
-- - This makes it clearer that the do block can be viewed as one I/O action,
--   but it’s uglier. Inside the do block, we apply reverseWords to the line that we got from
--   getLine and then print that to the terminal. After that, we just perform main .
--   It’s performed recursively, and that’s okay, because main is itself an I/O ac-
--   tion. So in a sense, we go back to the start of the program.
--   If null line is True , the code after the then is executed: return () . You
--   might have used a return keyword in other languages to return from a sub-
--   routine or function. But return in Haskell is nothing like the return in most
--   other languages
--
-- - In Haskell (and in I/O actions specifically), return makes an I/O action
--   out of a pure value. Returning to the box analogy for I/O actions, return
--   takes a value and wraps it up in a box. The resulting I/O action doesn’t ac-
--   tually do anything; it just yields that value as its result. So in an I/O context,
--   return "haha" will have a type of IO String .
--   What’s the point of just transforming a pure value into an I/O action
--   that doesn’t do anything? Well, we needed some I/O action to carry out in
--   the case of an empty input line. That’s why we made a bogus I/O action that
--   doesn’t do anything by writing return () .
--   Unlike in other languages, using return doesn’t cause the I/O do block
--   to end in execution. For instance, this program will quite happily continue
--   all the way to the last line:
--
--    main = do
--     return ()
--     return "HAHAHA"
--     line <- getLine
--     return "BLAH BLAH BLAH"
--     return 4
--     putStrLn line
--
-- - Again, all these uses of return do is make I/O actions that yield a result,
--   which is then thrown away because it isn’t bound to a name.
