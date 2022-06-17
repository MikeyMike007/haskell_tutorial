-- # Hello World

-- - You can compile following program with ghc --make ./002-hello-world.hs
main = putStrLn "Hello World"

-- - Please note the following examples
--
--   ghci> :t putStrLn
--   putStrLn :: String -> IO ()
--   ghci> :t putStrLn "hello, world"
--   putStrLn "hello, world" :: IO ()
--
-- - We can read the type putStrLn like this: putStrLn takes a string and returns
--   an I/O action that has the result type of (), that is, the empty tuple, also
--   known as unit
--
-- - An I/O action is something that, when performed, will carry out an ac-
--   tion with a side effect (such as reading input or printing stuff to the screen
--   or a file) and will also present some result. We say that an I/O action yields
--   this result. Printing a string to the terminal doesn’t really have any kind of
--   meaningful return value, so a dummy value of () is used.
--
-- - So when will an I/O action be performed? Well, this is where main comes
--   in. An I/O action will be performed when we give it a name of main and then
--   run our program
