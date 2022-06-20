# File and filestreams

## Input redirection

Create a file and named it `haiku.txt`,

```text
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
```

Now create following program called `capslocker.hs`,

```haskell
main = forever $ do
  input <- getLine
  putStrLn $ map toUpper input
```

```bash
$ ghc --make capslocker
[1 of 1] Compiling Main ( capslocker.hs, capslocker.o )
Linking capslocker ...

$ ./capslocker < haiku.txt
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file
```

## Getting strings from input streams

- Let’s take a look at an I/O action that makes processing input streams easier
  by allowing us to treat them as normal strings: `getContents`. `getContents` reads
  everything from the standard input until it encounters an end-of-file charac-
  ter. Its type is `getContents :: IO String`. What’s cool about `getContents` is that
  it does lazy I/O. This means that when we do `foo <- getContents`, `getContents`
  doesn’t read all of the input at once, store it in memory, and then bind it to
  foo. No, `getContents` is lazy! It will say, “Yeah yeah, I’ll read the input from the
  terminal later as we go along, when you really need it!”

- When the result of `getContents` is bound to `contents` , it’s not represented
  in memory as a real string, but more like a promise that the string will be
  produced eventually. When we `map` `toUpper` over `contents` , that’s also a pro-
  mise to map that function over the eventual contents. Finally, when `putStr`
  happens, it says to the previous promise, “Hey, I need a caps-locked line!”
  It doesn’t have any lines yet, so it says to `contents` , “How about getting a line
  from the terminal?” And that’s when `getContents` actually reads from the ter-
  minal and gives a line to the code that asked it to produce something tangi-
  ble. That code then `maps` `toUpper` over that line and gives it to `putStr` , which
  prints the line. And then `putStr` says, “Hey, I need the next line—come on!”
  This repeats until there’s no more input, which is signified by an end-of-file
  character.

```haskell
main :: IO ()
main = do
  contents <- getContents
  putStrLn $ map toUpper contents
```

```bash
$ ./capslocker < haiku.txt
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
```

## Example using `getContents`

Program that takes some input and prints out only those lines that are shorter than 10 characters,

```haskell
main = do
  contents <- getContents
  putStrLn $ shortLines contents

shortLines :: String -> String
shortLines = unlines . filter (\line -> length line <= 10) . lines
```

## Same example as above but with `interact`

`interact` takes a function of type `String -> String` as a parameter and returns an I/O action that will take some input, run that function on it, and then print out the function’s result.

```haskell
main :: IO ()
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line <= 10) . lines
```

## Another example with `interact` - Palindrome

```haskell
main :: IO ()
main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs then "Is palindrome" else "is not palindrome") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs
```

## Reading and writing files

See example below where the programs reads the contents of the file `girlfriend.txt` and writes that to the terminal.

```haskell
import System.IO

main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
```

Note the type signature of `openFile`

```haskell
openFile :: FilePath -> IOMode -> IO Handle
type FilePath = String
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMod
```

- `openFile` returns an I/O action that will open the specified file in the speci-
  fied mode. If we bind that action’s result to something, we get a Handle ,
  which represents where our file is. We’ll use that handle so we know which file
  to read from.

- `hGetContents` takes a Handle , so it knows which file to get the contents
  from, and returns an `IO String` — an I/O action that holds contents of the file
  as its result. This function is pretty much like `getContents`. The only
  difference is that `getContents` will automatically read from the standard in-
  put (that is, from the terminal), whereas `hGetContents` takes a file handle that
  tells it which file to read from. In all other respects, they work the same.

- A handle just points to our current position in the file. The contents are
  what’s actually in the file. If you imagine your whole filesystem as a really
  big book, the handle is like a bookmark that shows where you’re currently
  reading (or writing)

- With `putStr contents`, we print the contents out to the standard output,
  and then we do `hClose` , which takes a handle and returns an I/O action that
  closes the file. You need to close the file yourself after opening it with
  `openFile`! Your program may terminate if you try to open a file whose handle
  hasn’t been closed.

## Using the `withFile` function

Type signature:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
```

`withFile` takes a path to a file, an `IOMode` , and a function that takes a handle and returns some I/O action. Then it returns an I/O action that will open that file, do something with the file, and close it. Furthermore, if anything goes wrong while we’re operating on our file, `withFile` makes sure that the file handle gets closed.

Example

```haskell
import Control.Exception (handle)
import System.IO ( hGetContents, withFile, IOMode(ReadMode) )

main = do
  withFile
  "girlfriend.txt"
  ReadMode
  ( \handle -> do
  contents <- hGetContents handle
  putStrLn contents
  )
```

`(\handle -> ...)` is the function that takes a handle and returns an I/O action, and it’s usually done like this, with a lambda. It needs to take a function that returns an I/O action, rather than just taking an I/O action to do and then closing the file, because the I/O action that we would pass to it wouldn’t know on which file to operate. This way, `withFile` opens the file and then passes the handle to the function we gave it. It gets an I/O action back from that function and then makes an I/O action that’s just like the original action, but it also makes sure that the file handle gets closed, even if something goes awry.

## Raising exceptions

`Control.Exception` module offers the bracket function. It has the following type signature:

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
```

Its first parameter is an I/O action that acquires a resource, such as a file handle. Its second parameter is a function that releases that resource. This function gets called even if an exception has been raised. The third parameter is a function that also takes that resource and does something with it. The third parameter is where the main stuff happens, like reading from a file or
writing to it.

Because `bracket` is all about acquiring a resource, doing something with it, and making sure it gets released, implementing withFile is really easy:

The first parameter that we pass to bracket opens the file, and its result is a file handle. The second parameter takes that handle and closes it. `bracket` makes sure that this happens even if an exception is raised. Finally, the third parameter to `bracket` takes a handle and applies the function `f` to it, which takes a file handle and does stuff with that handle, like reading from or writing to the corresponding file,

```haskell
import Control.Exception ( bracket )
import System.IO
    ( Handle, hClose, openFile, hGetContents, IOMode(ReadMode) )

main = do
  withFile'
    "girlfriend.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStrLn contents
    )

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f =
  bracket
    (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)
```

## Example with `readFile`

The `readFile` function has a type signature of `readFile :: FilePath -> IO String`. (Remember that `FilePath` is just a fancy name for String .) `readFile` takes a path to a file and returns an I/O action that will read that file (lazily, of course) and bind its contents to something as a string. It’s usually more handy than calling `openFile` and then calling `hGetContents` with the resulting handle.

```haskell
import System.IO ()
main :: IO ()
main = do
  contents <- readFile "girlfriend.txt"
  putStr contents
```

## Example with `writeFile`

```haskell
import Data.Char ( toUpper )
import System.IO ()

main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" $ map toUpper contents
```

## Example of `appendFile`

```haskell
import System.IO

main :: IO ()
main = do
  todoItem <- getLine
  appendFile "todo.txt" $ todoItem ++ "\n"
```

## Example with Todolist

```haskell
import Data.List ( delete )
import System.Directory ( removeFile, renameFile )
import System.IO ( hClose, hPutStr, openTempFile )

main = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      -- todoTasks !! number returns the value of the index number in todoTasks
      -- delete "A" "ABCABC" gives "BCABC"
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTodoItems
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
```

## Another Todolist example using `bracketOnError`

```haskell
import Control.Exception ( bracketOnError )
import Data.List ( delete )
import System.Directory ( removeFile, renameFile )
import System.IO ( hClose, hPutStr, openTempFile )

main :: IO ()
main = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks

  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt"
    )
```

## Handling arguments in Haskell

```haskell
import Data.List ()
import System.Environment ( getArgs, getProgName )

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
```

## Complete Todolist program

```haskell
import Data.List ( delete )
import System.Directory ( removeFile, renameFile )
import System.Environment ( getArgs )
import System.IO ( hClose, hPutStr, openTempFile )

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

main = do
  (command : argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt"
    )
```

## Randomness

In the System.Random module. random's type signature is

```haskell
random :: (RandomGen g, Random a) => g -> (a, g)
```

Two typeclasses i.e.

`RandomGen`: Types that can act as sources of randomness
`Random`: Types whose values can be random

To use our `random` function, we need a random `generator`. `System.Random` exports a type called `StdGen` which is an instance of the `RandomGen` type class.

Lets run an example that fails,

```haskell
ghci> random (mkStdGen 100)
  <interactive>:1:0:
  Ambiguous type variable `a' in the constraint: `Random a' arising from a use of `random' at <interactive>:1:0-20
  Probable fix: add a type signature that fixes these type variable(s)
```

The error message means that the random function can return a value of any type that’s part of the `Random` type class, so we need to inform Haskell which type we want.

```haskell
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

Finally, a number that looks kind of `random`! The first component of the tuple is our number, and the second component is a textual representation of our new random generator.

## Tossing a coin

Let’s make a function that simulates tossing a coin three times. If `random` didn’t return a new generator along with a random value, we would need to make this function take three random generators as a parameter and return coin tosses for each of them. But if one generator can make a random value of type `Int` (which can take on a load of different values), it should be able to make three coin tosses (which can have only eight different end results). So this is where random returning a new generator along with a value comes in handy.

We call random with the generator we got as a parameter to get a coin and a new generator. Then we call it again, only this time with our new generator, to get the second coin. We do the same for the third coin. Had we called it with the same generator every time, all the coins would have had the same value, so we would get only `(False, False, False)` or `(True, True, True)` as a result.

```haskell
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen)   = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)
```

Running the program gives,

```haskell
ghci> threeCoins (mkStdGen 21)
(True,True,True)

ghci> threeCoins (mkStdGen 22)
(True,False,True)

ghci> threeCoins (mkStdGen 943)
(True,False,True)

ghci> threeCoins (mkStdGen 944)
(True,True,True)
```

## The function `randoms`

The function `randoms` takes a generator and returns an infinite sequence of values based on that generator,

```haskell
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]

ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]

ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
```

Why doesn’t `randoms` return a new generator as well as a list? We could implement the `randoms` function very easily like this:

1. Recursion

```haskell
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen
```

2. Other

```haskell
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
let (value, newGen) = random gen
    (restOfList, finalGen) = finiteRandoms (n-1) newGen
in (value:restOfList, finalGen)
```

Again, this is a recursive definition. We say that if we want zero numbers, we just return an empty list and the generator that was given to us. For any other number of random values, we first get one random number and a new generator. That will be the head. Then we say that the tail will be `n - 1` numbers generated with the new generator. Then we return the head and the rest of the list joined and the final generator that we got from getting the `n - 1` random numbers.

## Generate random values in some sort of range using `randomR`

```haskell
randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)
```

This means that the function `randomR` it’s kind of like the `random` function , but it takes as its first parameter a pair of values that set the lower and upper bounds, and the final value produced will be within those bounds.

```haskell
ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)

ghci> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)
```

There’s also `randomRs`, which produces a stream of random values within our defined ranges. Check this out:

```haskell
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
```

## Randomness and IO

You may be wondering what this section has to do with I/O. We haven’t done anything concerning I/O so far. We’ve always made our random number generator manually by creating it with some arbitrary integer. The problem is that if we do that in our real programs, they will always return the same random numbers, which is no good for us. That’s why `System.Random` offers the `getStdGen` I/O action, which has a type of IO `StdGen`. It asks the system for some initial data and uses it to jump-start the global generator. `getStdGen` fetches that global random generator when you bind it to something.

```haskell

import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
```

Running the program gives,

```haskell
$ ./random_string
pybphhzzhuepknbykxhe

$ ./random_string
eiqgcxykivpudlsvvjpg

$ ./random_string
nzdceoconysdgcyqjruo

$ ./random_string
bakzhnnuzrkgvesqplrx
```

But you need to be careful. Just performing getStdGen twice will ask the system for the same global generator twice. Suppose we do this,

```haskell
import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen2 <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen2)
```

This will mean that we wil get the same string printed out twice.

The best way to get two different strings is to use the newStdGen action, which splits our current random generator into two generators. It updates the global random generator with one of them and yields the other as its result.

```haskell
import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen' <- newStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen')
```

Not only do we get a new random generator when we bind `newStdGen` to something, but the global one gets updated as well. This means that if we do `getStdGen` again and bind it to something, we’ll get a generator that’s not the same as `gen`.

## Example of program "Guess the number"

```haskell
module Main where

import Control.Monad (when)
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am i thinking of?"
  numberString <- getLine
  -- Can also use: unless (null numberString)
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry it was " ++ show randNumber

    -- We perform askForNumber recursively, but this time with the new generator
    -- that we got. This gives us an I/O action that’s just like the one we
    -- performed, except that it depends on a different generator.
    askForNumber newGen
```

## Example of program "Guess the number" version 2

It’s very similar to the previous version, but instead of making a function that takes a generator and then calls itself recursively with the new updated generator, we do all the work in `main` . After telling the user whether he was correct in his guess, we update the global generator and then call `main` again. Both approaches are valid, but I like the first one more since it does less stuff in `main` and also provides a function I can reuse easily.

```haskell
main :: IO ()
main = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am i thinking off?"
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main
```

## Bytestrings

- Processing files as strings has one drawback: It tends to be slow. Lists are
  really lazy. Remember that a list like `[1,2,3,4]` is syntactic sugar for
  `1:2:3:4:[]`. When the first element of the list is forcibly evaluated (say by
  printing it), the rest of the list `2:3:4:[]` is still just a promise of a list,
  and so on. We call that promise a **thunk**.

- A **thunk** is basically a deferred computation. Haskell achieves its lazi-
  ness by using thunks and computing them only when it must, instead of computing
  everything up front. So you can think of lists as promises that the next
  element will be delivered once it really has to be, and along with it, the
  promise of the element after it. It doesn’t take a big mental leap to conclude
  that processing a simple list of numbers as a series of thunks might not be the
  most efficient technique in the world. That overhead doesn’t bother us most of
  the time, but it turns out to be a liability when reading big files and
  manipulating them. That’s why Haskell has bytestrings. Bytestrings are sort of
  like lists, only each element is one byte (or 8 bits) in size. The way they
  handle laziness is also different.

### Strict and lazy bytestrings

- Bytestrings come in two flavors: strict and lazy. Strict bytestrings reside in
  `Data.ByteString`, and they do away with the laziness completely. There are no
  thunks involved. A strict bytestring represents a series of bytes in an array.
  You can’t have things like infinite strict bytestrings. If you evaluate the
  first byte of a strict bytestring, you must evaluate the whole thing

- The other variety of bytestrings resides in `Data.ByteString.Lazy`. They’re
  lazy, but not quite as lazy as lists. Since there are as many thunks in a list
  as there are elements, they are kind of slow for some purposes. Lazy
  bytestrings take a different approach. They are stored in chunks (not to be
  confused with thunks!), and each chunk has a size of 64KB. So if you evaluate a
  byte in a lazy bytestring (by printing it, for example), the first 64KB will be
  evalu- ated. After that, it’s just a promise for the rest of the chunks. Lazy
  bytestrings are kind of like lists of strict bytestrings, with a size of 64KB.
  When you pro- cess a file with lazy bytestrings, it will be read chunk by
  chunk. This is cool because it won’t cause the memory usage to skyrocket, and
  the 64KB proba- bly fits neatly into your CPU’s L2 cache.

- If you look through the documentation for `Data.ByteString.Lazy` , you
  will see that it has a lot of functions with the same names as the ones from
  `Data.List`, but the type signatures have `ByteString` instead of `[a]` and `Word8`
  instead of `a`. These functions are similar to the ones that work on lists. Be-
  cause the names are the same, we’re going to do a qualified import in a script
  and then load that script into `GHCi` to play with bytestrings:

  ```haskell
  import qualified Data.ByteString.Lazy as B
  import qualified Data.ByteString as S
  ```

  `B` has lazy bytestring types and functions, whereas `S` has strict ones. We’ll
  mostly be using the lazy versions

- The `pack` function has the type signature `pack :: [Word8] -> ByteString` .
  This means that it takes a list of bytes of type `Word8` and returns a `ByteString`
  . You can think of it as taking a list, which is lazy, and making it less lazy,
  so that it’s lazy only at 64KB intervals.

- The `Word8` type is like `Int` , but it represents an unsigned 8-bit number.
  This means that it has a much smaller range of only 0 to 255. And just like
  `Int`, it’s in the `Num` type class. For instance, we know that the value 5 is
  polymorphic in that it can act like any numeric type, including `Word8`

- Here’s how we pack lists of numbers into bytestrings:

  ```haskell
  ghci> B.pack [99,97,110]
  Chunk "can" Empty

  ghci> B.pack [98..120]
  Chunk "bcdefghijklmnopqrstuvwx" Empty
  ```

  We packed only a handful of values into a bytestring, so they fit inside
  one chunk. `Empty` is like `[]` for lists—they both represent an empty sequence. As
  you can see, you don’t need to specify that your numbers are of type `Word8`,
  because the type system can make numbers choose that type. If you try to use a
  big number like `336` as a `Word8` , it will just wrap around to `80`.

- When we need to examine a bytestring byte by byte, we need to unpack
  it. The unpack function is the inverse of `pack`. It takes a bytestring and
  turns it into a list of bytes. Here’s an example:

  ```haskell
  ghci> let by = B.pack [98,111,114,116]
  ghci> by
  Chunk "bort" Empty

  ghci> B.unpack by
  [98,111,114,116]
  ```

- You can also go back and forth between strict and lazy bytestrings. The
  `toChunks` function takes a lazy bytestring and converts it to a list of strict
  ones. The `fromChunks` function takes a list of strict bytestrings and converts
  it to a lazy bytestring:

  ```haskell
  ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
  Chunk "()\*" (Chunk "+,-" (Chunk "./0" Empty))
  ```

  This is good if you have a lot of small strict bytestrings and you want to
  process them efficiently without joining them into one big strict bytestring in
  memory first.

- The bytestring version of `:` is called `cons`. It takes a byte and a bytestring
  and puts the byte at the beginning.

  ```haskell
  ghci> B.cons 85 $ B.pack [80,81,82,84]
  Chunk "U" (Chunk "PQRT" Empty)
  ```

- The bytestring modules have a load of functions that are analogous to
  those in Data.List, including, but not limited to, head , tail , init , null ,
  length , map, reverse, foldl, foldr , concat , takeWhile , filter , and so on.
  For a complete listing of bytestring functions, check out the documentation for
  the byte- string package at http://hackage.haskell.org/package/bytestring/. The
  bytestring modules also have functions that have the same name and behave the
  same as some functions found in System.IO , but Strings are replaced with
  ByteStrings . For instance, the `readFile` function in `System.IO` has this type:

  ```haskell
  readFile :: FilePath -> IO String
  ```

  The `readFile` function from the bytestring modules has the following type:

  ```haskell
    readFile :: FilePath -> IO ByteString
  ```

- If you’re using strict bytestrings and you attempt to read a file, all of
  that file will be read into memory at once! With lazy bytestrings, the file
  will be read in neat chunks

### Copying files with bytestrings

- An example of a program that takes two filenames as command line arguments and copies
  the first file into the second file,

```haskell
import Control.Exception ( bracketOnError )
import qualified Data.ByteString as B
import System.Directory ( removeFile, renameFile )
import System.Environment ( getArgs )
import System.IO ( hClose, openTempFile )

main = do
  (filename1 : filename2 : _) <- getArgs
  copy filename1 filename2

  copy source dest = do
    contents <- B.readFile source
    bracketOnError
      (openTempFile "." "temp")
      ( \(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName
      )
      ( \(tempName, tempHandle) -> do
      B.hPutStr tempHandle contents
      hClose tempHandle
      renameFile tempName dest
      )
```

Whenever you need better performance in a program that reads a lot of data into strings, give bytestrings a try. Chances are you’ll get some good performance boosts with very little effort on your part. I usually write pro- grams using normal strings and then convert them to use bytestrings if the performance is not satisfactory.
