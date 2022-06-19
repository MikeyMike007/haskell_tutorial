import Control.Monad

-- - forM (located in Control.Monad ) is like mapM , but its parameters are switched
--   around. The first parameter is the list, and the second is the function to map
--   over that list, which is then sequenced. Why is that useful? Well, with some
--   creative use of lambdas and do notation, we can do stuff like this:

main :: IO [()]
main = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          color <- getLine
          return color
      )

  putStrLn "The colors that you associate with 1,2,3 and 4 are:"
  mapM putStrLn colors

-- - The (\a -> do ... ) lambda is a function that takes a number and re- turns an
--   I/O action. Notice that we call return color in the inside do block. We do that
--   so that the I/O action that the do block defines yields the string that
--   represents our color of choice. We actually did not have to do that though,
--   since getLine already yields our chosen color, and it’s the last line in the do
--   block. Doing color <- getLine and then return color is just unpack- ing the
--   result from getLine and then repacking it—it’s the same as just call- ing
--   getLine.
--
--   The forM function (called with its two parameters) produces an I/O action,
--   whose result we bind to colors . colors is just a normal list that holds
--   strings. At the end, we print out all those colors by calling mapM putStrLn
--   colors.
--
-- - Following code would be exactly the same as above:

--   main = do
--     colors <-
--       forM
--         [1, 2, 3, 4]
--         ( \a -> do
--             putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--             getLine
--         )
--
--     putStrLn "The colors that you associate with 1,2,3 and 4 are:"
--     mapM putStrLn colors
