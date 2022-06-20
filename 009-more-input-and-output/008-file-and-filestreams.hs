-- # Raising exceptions
--
-- - Control.Exception module offers the bracket function. It has the following type signature:

--   bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

-- - Its first parameter is an I/O action that acquires a resource, such as a file
--   handle. Its second parameter is a function that releases that resource. This
--   function gets called even if an exception has been raised. The third parameter
--   is a function that also takes that resource and does something with it. The
--   third parameter is where the main stuff happens, like reading from a file or
--   writing to it.

-- - Because bracket is all about acquiring a resource, doing something with
--   it, and making sure it gets released, implementing withFile is really easy:

-- - The first parameter that we pass to bracket opens the file, and its result is a
--   file handle. The second parameter takes that handle and closes it. bracket
--   makes sure that this happens even if an exception is raised. Finally, the third
--   parameter to bracket takes a handle and applies the function f to it, which
--   takes a file handle and does stuff with that handle, like reading from or writ-
--   ing to the corresponding file

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
