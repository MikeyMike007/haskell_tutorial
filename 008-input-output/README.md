# Input and Output (IO)

## Separating the Pure from the impure

- Instead of giving the computer a series of steps to execute, you give it def-
  initions of what certain things are

- A function isn’t allowed to have side effects. A function can give us back only
  some result based on the parameters we supplied to it. If a function is called
  two times with the same parameters, it must return the same result.

- Haskell has a really clever system for dealing with functions that have side
  effects. It neatly separates the part of our program that is pure and the part
  of our program that is impure, which does all the dirty work like talking to the
  keyboard and the screen. With those two parts separated, we can still reason
  about our pure program and take advantage of all the things that purity
  offers—like laziness, robustness, and composability—while easily communicating
  with the outside world.

## Hello World

Following program can be compiled with,

```bash
 ghc --make ./001-hello-world.hs
```

```haskell
main :: IO ()
main = putStrLn "Hello World"
```

```haskell
ghci> :t putStrLn
putStrLn :: String -> IO ()

ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
```

- We can read the type `putStrLn` like this: `putStrLn` takes a string and returns
  an I/O action that has the result type of `()`, that is, the empty tuple, also
  known as unit

- An I/O action is something that, when performed, will carry out an ac-
  tion with a side effect (such as reading input or printing stuff to the screen
  or a file) and will also present some result. We say that an I/O action yields
  this result. Printing a string to the terminal doesn’t really have any kind of
  meaningful return value, so a dummy value of () is used.

- So when will an I/O action be performed? Well, this is where main comes
  in. An I/O action will be performed when we give it a name of main and then
  run our program

## Gluing IO actions together

`import Data.Char`

- Having your whole program be just one I/O action seems kind of limiting.
  That’s why we can use do syntax to glue together several I/O actions into
  one. Take a look at the following example:

```haskell
main = do
  putStrLn "Hello, Whats your name?"
  name <- getLine
  putStrLn
    ("Hey " ++ name ++ ", you rock!")
```

- Notice that we said `do` and then we laid out a series of steps, as we would
  in an imperative program. Each of these steps is an I/O action. By putting
  them together with `do` syntax, we glued them into one I/O action. The ac-
  tion that we got has a type of `IO ()` , as that’s the type of the last I/O action
  inside. Because of that, main always has a type signature of `main :: IO something` ,
  where something is some concrete type. We don’t usually specify a type decla-
  ration for main.

See following example

```haskell
 ghci> :t getLine
 getLine :: IO String
```

- We see that getLine is an I/O action that yields a `String` . That makes
  sense, because it will wait for the user to input something at the terminal, and
  then that something will be represented as a string. So what’s up with `name <- getLine` then? You can read that piece of code like this: perform the I/O action
  `getLine` , and then bind its result value to `name`. `getLine` has a type of
  `IO String` , so name will have a type of `String`

- You can think of an I/O action as a box with little feet that will go out into
  the real world and do something there (like write some graffiti on a wall) and
  maybe bring back some data. Once it has fetched that data for you, the only way
  to open the box and get the data inside it is to use the `<-` construct. And if
  we’re taking data out of an I/O action, we can take it out only when we’re
  inside another I/O action. This is how Haskell manages to neatly separate the
  pure and impure parts of our code. `getLine` is impure, because its result value
  is not guaranteed to be the same when performed twice.

- This code is working:

```haskell
 main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Zis is your future: " ++ tellFortune name
```

- This code is not working:

```haskell
  nameTag = "Hello, my name is " ++ getLine
```

- This doesn’t work because `++` requires both its parameters to be lists over the
  same type. The left parameter has a type of `String` (or `[Char]`, if you will),
  while `getLine` has a type of `IO String` . Remember that you can’t concatenate a
  `String` and an I/O action. First, you need to get the result out of the I/O
  action to get a value of type `String` , and the only way to do that is to do
  something like `name <- getLine` inside some other I/O action. If we want to
  deal with impure data, we must do it in an impure environment. The taint of
  impurity spreads around much like the undead scourge, and it’s in our best
  interest to keep the I/O parts of our code as small as possible.

- Every I/O action that is performed yields a result. That’s why our
  previous example could also have been written like this:

```haskell
 main = do
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
```

- However, `foo` would just have a value of `()`, so doing that would be kind
  of moot. Notice that we didn’t bind the last `putStrLn` to anything. That’s be-
  cause in a `do` block, the last action cannot be bound to a name as the first
  two were. You’ll see exactly why that is so when we venture off into the world
  of `monads`, starting in Chapter 13. For now, the important point is that the
  `do` block automatically extracts the value from the last action and yields that
  as its own result.

- Except for the last line, every line in a `do` block that doesn’t bind can
  also be written with a bind. So `putStrLn "BLAH"` can be written as `_ <- putStrLn "BLAH"`. But that’s useless, so we leave out the `<-` for I/O actions that don’t
  yield an important result, like `putStrLn`

- What do you think will happen when we do something like the following?

```haskell
myLine = getLine
```

- Do you think it will read from the input and then bind the value of that
  to name? Well, it won’t. All this does is give the `getLine` I/O action a differ-
  ent name called `myLine`. Remember that to get the value out of an I/O ac-
  tion, you must perform it inside another I/O action by binding it to a name
  with `<-`.

- I/O actions will be performed when they are given a name of `main` or
  when they’re inside a bigger I/O action that we composed with a `do` block.
  We can also use a `do` block to glue together a few I/O actions, and then we
  can use that I/O action in another `do` block, and so on. They will be per-
  formed if they eventually fall into main .

## Using let inside I/O actions

- When using do syntax to glue together I/O actions, we can use `let` syntax
  to bind pure values to names. Whereas `<-` is used to perform I/O actions
  and bind their results to names, let is used when we just want to give names
  to normal values inside I/O actions. It’s similar to the let syntax in list
  comprehensions.

- See how the I/O actions in the `do` block are lined up? Also notice how
  the `let` is lined up with the I/O actions, and the names of the `let` are lined
  up with each other? That’s good practice, because indentation is important
  in Haskell.

- You may be wondering when to use `<-` and when to use `let` bindings.
  `<-` is for performing I/O actions and binding their results to names.
  `map toUpper firstName`, however, isn’t an I/O action — it’s a pure expression
  in Haskell. So you can use `<-` when you want to bind the results of I/O ac-
  tions to names, and you can use `let` bindings to bind pure expressions to
  names. Had we done something like `let firstName = getLine` , we would have
  just called the `getLine` I/O action a different name, and we would still need
  to run it through `a <-` to perform it and bind its result.

```haskell
main :: IO ()
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
```

## Example with writing out input sentences as reversed sentences

Example,

```haskell
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
```

- First, we get a line from the terminal by performing
  `getLine` and call that line `sentence` . Next we have a conditional expression. Re-
  member that in Haskell, every `if` must have a corresponding `else` , because
  every expression must have some sort of value. Our if says that when a con-
  dition is `true` (in our case, the line that we entered is blank), we perform one
  I/O action; when it isn’t true, the I/O action under the `else` is performed.
  Because we need to have exactly one I/O action after the else , we use a
  `do` block to glue together two I/O actions into one. We could also write that
  part as follows:

```haskell
else (do
  putStrLn $ reverseWords line
  main)
```

- This makes it clearer that the `do` block can be viewed as one I/O action,
  but it’s uglier. Inside the `do` block, we apply `reverseWords` to the line that we got from
  `getLine` and then print that to the terminal. After that, we just perform `main`.
  It’s performed recursively, and that’s okay, because `main` is itself an I/O ac-
  tion. So in a sense, we go back to the start of the program.
  If `null sentence` is `True`, the code after the then is executed: `return ()` . You
  might have used a `return` keyword in other languages to return from a sub-
  routine or function. But `return` in Haskell is nothing like the return in most
  other languages

- In Haskell (and in I/O actions specifically), `return` makes an I/O action
  out of a pure value. Returning to the box analogy for I/O actions, `return`
  takes a value and wraps it up in a box. The resulting I/O action doesn’t ac-
  tually do anything; it just yields that value as its result. So in an I/O context,
  `return "haha"` will have a type of `IO String`.
  What’s the point of just transforming a pure value into an I/O action
  that doesn’t do anything? Well, we needed some I/O action to carry out in
  the case of an empty input line. That’s why we made a bogus I/O action that
  doesn’t do anything by writing return () .
  Unlike in other languages, using `return` doesn’t cause the I/O `do` block
  to end in execution. For instance, this program will quite happily continue
  all the way to the last line:

```haskell
  main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line
```

- Again, all these uses of `return` do is make I/O actions that yield a result,
  which is then thrown away because it isn’t bound to a name.

- We can use `return` in combination with `<-` to bind stuff to names:

```haskell
main = do
  a <- return "hell"
  b <- return "yeah"
  putStr $ a ++ " " ++ b
```

- So you see, `return` is sort of the opposite of `<-` . While `return` takes a value
  and wraps it up in a box, `<-` takes a box (and performs it) and takes the value
  out of it, binding it to a name. But doing this is kind of redundant, especially
  since you can use `let` in do blocks to bind to names, like so:

```haskell
  main = do
    let a = "hell"
    b = "yeah"
    putStrLn $ a ++ " " ++ b
```

- When dealing with I/O `do` blocks, we mostly use `return` either because we
  need to create an I/O action that doesn’t do anything or because we don’t
  want the I/O action that’s made up from a do block to have the result value
  of its last action. When we want it to have a different result value, we use
  `return` to make an I/O action that always yields our desired result, and we
  put it at the end.

## Useful IO functions

### `putStr`

```haskell
main :: IO ()
main = do
  putStr "Hey, "
  putStr "a am "
  putStr "Andy!"
```

### `putChar`

```haskell
main :: IO ()
main = do
  putChar 'A'
  putChar 'B'
  putChar 'C'
```

Implementation of `putStr` with the help of `putChar`,

```haskell
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs
```

### `print`

`print` takes a value of any type that’s an instance of `Show` (meaning that we know how to represent it as a `string`), applies show to that value to “stringify” it, and then outputs that `string` to the terminal. Basically, it’s just `putStrLn . show`. It first runs show on a value, and then feeds that to `putStrLn`, which returns an I/O action that will print out our value.

```haskell
main :: IO ()
main = do
  print True
  print 2
  print "haha"
  print 3.2
  print [3, 4, 3]
```

### `when`

`when` takes a `Bool` and an I/O action, and if that `Bool` value is `True` , it returns the same I/O action that we supplied to it. However, if it’s `False` , it returns the `return ()` action, which doesn’t do anything.

```haskell
import Control.Monad

main :: IO ()
main = do
  input <- getLine
  when (input == "SWORDFISH") $ do putStrLn input
```

### `sequence`

- The sequence function takes a list of I/O actions and returns an I/O action
  that will perform those actions one after the other. The result that this I/O
  action yields will be a list of the results of all the I/O actions that were per-
  formed.

```haskell
main :: IO ()
main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs
```

The code below is excactly similar to the code above,

```haskell
main = do
  a <- getLine
  b <- getLine
  c <- getLine
  print [a,b,c]
```

- The results of both these versions are exactly the same. sequence
  `[getLine, getLine, getLine]` makes an I/O action that will perform `getLine`
  three times. If we bind that action to a name, the result is a list of all the re-
  sults. So in this case, the result would be a list of three things that the user
  entered at the prompt

- A common pattern with `sequence` is when we map functions like print or
  `putStrLn` over lists. Executing `map print [1,2,3,4]` won’t create an I/O action,
  but instead will create a list of I/O actions. Effectively, this is the same as
  writing this: `[print 1, print 2, print 3, print 4]` If we want to transform that list of I/O actions into an I/O action, we must sequence it:

```haskell
ghci> sequence $ map print [1,2,3,4,5]
1
2
3
4
5
[(),(),(),(),()]
```

- But what’s with the `[(),(),(),(),()]` at the end of the output? Well,
  when we evaluate an I/O action in `GHCi`, that action is performed, and
  then its result is printed out, unless that result is `()`. That’s why evaluating
  `putStrLn "hehe"` in `GHCi` just prints out "hehe" - `putStrLn "hehe"` yields (). But when we enter `getLine`in`GHCi`, the result of that I/O action is printed out, because `getLine` has a type of `IO String`

### `mapM`

- Because mapping a function that returns an I/O action over a list and then
  sequencing it is so common, the utility functions `mapM` and `mapM_` were intro-
  duced. `mapM` takes a function and a list, maps the function over the list, and
  then sequences it. `mapM_` does the same thing, but it throws away the result
  later. We usually use `mapM_` when we don’t care what result our sequenced
  I/O actions have. Here’s an example of mapM

```haskell
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
```

But when we don’t care about the list of three units at the end, so it’s better to
use this,

```haskell
ghci> mapM_ print [1,2,3]
1
2
3
```

### `forever`

The forever function takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.

```haskell
import Control.Monad ( forever )
import Data.Char ( toUpper )

main :: IO ()
main = forever $ do
  putStrLn "Give me input"
  input <- getLine
  putStrLn $ map toUpper input

```

### `forM`

`forM` (located in `Control.Monad` ) is like `mapM` , but its parameters are switched around. The first parameter is the list, and the second is the function to map over that list, which is then sequenced. Why is that useful? Well, with some creative use of lambdas and `do` notation, we can do stuff like this:

```haskell
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
```

The `(\a -> do ... )` lambda is a function that takes a number and returns an I/O action. Notice that we call `return` color in the inside `do` block. We do that so that the I/O action that the `do` block defines yields the string that represents our color of choice. We actually did not have to do that though, since `getLine` already yields our chosen color, and it’s the last line in the `do` block. Doing `color <- getLine` and then return color is just unpacking the result from `getLine` and then repacking it — it’s the same as just calling `getLine`

The `forM` function (called with its two parameters) produces an I/O action, whose result we bind to `colors`. `colors` is just a normal list that holds `strings`. At the end, we print out all those colors by calling `mapM putStrLn colors`

Following code would be exactly the same as above:

```haskell
main = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
      putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
      getLine
      )

  putStrLn "The colors that you associate with 1,2,3 and 4 are:"
  mapM putStrLn colors
```
