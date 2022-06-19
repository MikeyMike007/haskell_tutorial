import Control.Monad

-- when takes a Bool and an I/O action, and if that Bool value is True , it re-
-- turns the same I/O action that we supplied to it. However, if it’s False , it
-- returns the return () action, which doesn’t do anything.

main :: IO ()
main = do
  input <- getLine
  when (input == "SWORDFISH") $ do putStrLn input

-- You can also use if / else but the code is not as straight forward as with when
-- main' = do
--   sentence <- getLine
--   if sentence == "SWORDFISH"
--     then putStrLn sentence
--     else return ()
