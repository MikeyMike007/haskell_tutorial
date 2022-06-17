-- # Using the withFile function

-- - Type signature:

--   withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

-- - It takes a path to a file, an IOMode , and a function that takes a handle and
--   returns some I/O action. Then it returns an I/O action that will open that
--   file, do something with the file, and close it. Furthermore, if anything goes
--   wrong while we’re operating on our file, withFile makes sure that the file
--   handle gets closed.

import Control.Exception (handle)
import System.IO

main = do
  withFile
    "girlfriend.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStrLn contents
    )

-- - (\handle -> ...) is the function that takes a handle and returns an I/O action,
--   and it’s usually done like this, with a lambda. It needs to take a func- tion
--   that returns an I/O action, rather than just taking an I/O action to do and
--   then closing the file, because the I/O action that we would pass to it wouldn’t
--   know on which file to operate. This way, withFile opens the file and then
--   passes the handle to the function we gave it. It gets an I/O action back from
--   that function and then makes an I/O action that’s just like the original
--   action, but it also makes sure that the file handle gets closed, even if
--   something goes awry
