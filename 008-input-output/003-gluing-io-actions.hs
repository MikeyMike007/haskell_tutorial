import Data.Char

-- # Gluing I/O actions together
--
-- - Having your whole program be just one I/O action seems kind of limiting.
--   That’s why we can use do syntax to glue together several I/O actions into
--   one. Take a look at the following example:

--   main = do
--     putStrLn "Hello, Whats your name?"
--     name <- getLine
--     putStrLn
--       ("Hey " ++ name ++ ", you rock!")

-- - Notice that we said do and then we laid out a series of steps, as we would
--   in an imperative program. Each of these steps is an I/O action. By putting
--   them together with do syntax, we glued them into one I/O action. The ac-
--   tion that we got has a type of IO () , as that’s the type of the last I/O action
--   inside. Because of that, main always has a type signature of main :: IO something ,
--   where something is some concrete type. We don’t usually specify a type decla-
--   ration for main.
--
-- - See following examples
--
--   ghci> :t getLine
--   getLine :: IO String
--
-- - We see that getLine is an I/O action that
--   yields a String . That makes sense, because it will wait for the user to input
--   something at the terminal, and then that something will be represented as a
--   string. So what’s up with name <- getLine then? You can read that piece of code
--   like this: perform the I/O action getLine , and then bind its result value to
--   name . getLine has a type of IO String , so name will have a type of String
--
-- - You can think of an I/O action as a box with little feet that will go out
--   into the real world and do something there
--   (like write some graffiti on a wall) and maybe bring back some data. Once it
--   has fetched that data for you, the only way to open the box and get the data
--   inside it is to use the <- construct. And if we’re taking data out of an I/O
--   action, we can take it out only when we’re inside another I/O action. This
--   is how Haskell manages to neatly separate the pure and impure parts of our
--   code. getLine is impure, because its result value is not guaranteed to be the
--   same when performed twice.
--
-- - This code is working:
--
--   main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn $ "Zis is your future: " ++ tellFortune name
--
-- - This code is not working:
--
--   nameTag = "Hello, my name is " ++ getLine
--
--   This doesn’t work because ++ requires both its parameters to be lists over the
--   same type. The left parameter has a type of String (or [Char], if you will),
--   while getLine has a type of IO String . Remember that you can’t concatenate a
--   string and an I/O action. First, you need to get the result out of the I/O
--   action to get a value of type String , and the only way to do that is to do
--   something like name <- getLine inside some other I/O action. If we want to
--   deal with impure data, we must do it in an impure environ- ment. The taint of
--   impurity spreads around much like the undead scourge, and it’s in our best
--   interest to keep the I/O parts of our code as small as possible.
--
-- -  Every I/O action that is performed yields a result. That’s why our
--    previous example could also have been written like this:
--
--    main = do
--     foo <- putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ ", you rock!")
--
--     However, foo would just have a value of (), so doing that would be kind
--     of moot. Notice that we didn’t bind the last putStrLn to anything. That’s be-
--     cause in a do block, the last action cannot be bound to a name as the first
--     two were. You’ll see exactly why that is so when we venture off into the world
--     of monads, starting in Chapter 13. For now, the important point is that the
--     do block automatically extracts the value from the last action and yields that
--     as its own result.
--
-- - Except for the last line, every line in a do block that doesn’t bind can
--   also be written with a bind. So putStrLn "BLAH" can be written as _ <- putStrLn
--   "BLAH". But that’s useless, so we leave out the <- for I/O actions that don’t
--   yield an important result, like putStrLn
--
-- - What do you think will happen when we do something like the following?
--
--   myLine = getLine
--
--   Do you think it will read from the input and then bind the value of that
--   to name? Well, it won’t. All this does is give the getLine I/O action a differ-
--   ent name called myLine . Remember that to get the value out of an I/O ac-
--   tion, you must perform it inside another I/O action by binding it to a name
--   with <-.
--
-- - I/O actions will be performed when they are given a name of main or
--   when they’re inside a bigger I/O action that we composed with a do block.
--   We can also use a do block to glue together a few I/O actions, and then we
--   can use that I/O action in another do block, and so on. They will be per-
--   formed if they eventually fall into main .
--
-- ## Using let inside I/O actions
--
-- - When using do syntax to glue together I/O actions, we can use let syntax
--   to bind pure values to names. Whereas <- is used to perform I/O actions
--   and bind their results to names, let is used when we just want to give names
--   to normal values inside I/O actions. It’s similar to the let syntax in list
--   comprehensions.
--
-- - See how the I/O actions in the do block are lined up? Also notice how
--   the let is lined up with the I/O actions, and the names of the let are lined
--   up with each other? That’s good practice, because indentation is important
--   in Haskell.
--
-- - You may be wondering when to use <- and when to use let bindings.
--   <- is for performing I/O actions and binding their results to names.
--   map toUpper firstName, however, isn’t an I/O action—it’s a pure expression
--   in Haskell. So you can use <- when you want to bind the results of I/O ac-
--   tions to names, and you can use let bindings to bind pure expressions to
--   names. Had we done something like let firstName = getLine , we would have
--   just called the getLine I/O action a different name, and we would still need
--   to run it through a <- to perform it and bind its result.

main = do
  putStrLn "Whats your first name?"
  firstName <- getLine
  putStrLn "Whats your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $
    "Hey " ++ bigFirstName ++ " "
      ++ bigLastName
      ++ " "
      ++ ", how are you?"
