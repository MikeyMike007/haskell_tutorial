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
