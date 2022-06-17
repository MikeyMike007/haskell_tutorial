-- # Reading and writing files
import System.IO

main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

type signature of openFile:
openFile :: FilePath -> IOMode -> IO Handle
type FilePath = String
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMod

-- - openFile returns an I/O action that will open the specified file in the speci-
--   fied mode. If we bind that action’s result to something, we get a Handle ,
--   which represents where our file is. We’ll use that handle so we know which file
--   to read from.
--
--
-- - hGetContents . It takes a Handle , so it knows which file to get the contents
--   from, and returns an IO String —an I/O action that holds contents of the file
--   as its result. This function is pretty much like getContents . The only
--   difference is that getContents will automatically read from the standard in-
--   put (that is, from the terminal), whereas hGetContents takes a file handle that
--   tells it which file to read from. In all other re- spects, they work the same.
--
-- - A handle just points to our current position in the file. The contents are
--   what’s actually in the file. If you imagine your whole filesystem as a really
--   big book, the handle is like a bookmark that shows where you’re currently
--   reading (or writing)
--
-- - With putStr contents, we print the contents out to the standard output,
--   and then we do hClose , which takes a handle and returns an I/O action that
--   closes the file. You need to close the file yourself after opening it with
--   openFile! Your program may terminate if you try to open a file whose handle
--   hasn’t been closed.
