-- # Bytestrings

-- - Processing files as strings has one drawback: It tends to be slow. Lists are
--   really lazy. Remember that a list like [1,2,3,4] is syntactic sugar for
--   1:2:3:4:[]. When the first element of the list is forcibly evaluated (say by
--   printing it), the rest of the list 2:3:4:[] is still just a promise of a list,
--   and so on. We call that promise a thunk.
--
-- - A thunk is basically a deferred computation. Haskell achieves its lazi-
--   ness by using thunks and computing them only when it must, instead of computing
--   everything up front. So you can think of lists as promises that the next
--   element will be delivered once it really has to be, and along with it, the
--   promise of the element after it. It doesn’t take a big mental leap to conclude
--   that processing a simple list of numbers as a series of thunks might not be the
--   most efficient technique in the world. That overhead doesn’t bother us most of
--   the time, but it turns out to be a liability when reading big files and
--   manipulating them. That’s why Haskell has bytestrings. Bytestrings are sort of
--   like lists, only each element is one byte (or 8 bits) in size. The way they
--   handle laziness is also different.
--
-- ## Strict and lazy bytestrings
--
-- - Bytestrings come in two flavors: strict and lazy. Strict bytestrings reside in
--   Data.ByteString, and they do away with the laziness completely. There are no
--   thunks involved. A strict bytestring represents a series of bytes in an array.
--   You can’t have things like infinite strict bytestrings. If you evaluate the
--   first byte of a strict bytestring, you must evaluate the whole thing
--
-- - The other variety of bytestrings resides in Data.ByteString.Lazy . They’re
--   lazy, but not quite as lazy as lists. Since there are as many thunks in a list
--   as there are elements, they are kind of slow for some purposes. Lazy
--   bytestrings take a different approach. They are stored in chunks (not to be
--   confused with thunks!), and each chunk has a size of 64KB. So if you evaluate a
--   byte in a lazy bytestring (by printing it, for example), the first 64KB will be
--   evalu- ated. After that, it’s just a promise for the rest of the chunks. Lazy
--   bytestrings are kind of like lists of strict bytestrings, with a size of 64KB.
--   When you pro- cess a file with lazy bytestrings, it will be read chunk by
--   chunk. This is cool because it won’t cause the memory usage to skyrocket, and
--   the 64KB proba- bly fits neatly into your CPU’s L2 cache.
--
-- - If you look through the documentation for Data.ByteString.Lazy , you
--   will see that it has a lot of functions with the same names as the ones from
--   Data.List, but the type signatures have ByteString instead of [a] and Word8
--   instead of a. These functions are similar to the ones that work on lists. Be-
--   cause the names are the same, we’re going to do a qualified import in a script
--   and then load that script into GHCi to play with bytestrings:
--
--   import qualified Data.ByteString.Lazy as B
--   import qualified Data.ByteString as S
--
--   B has lazy bytestring types and functions, whereas S has strict ones. We’ll
--   mostly be using the lazy versions
--
-- - The pack function has the type signature pack :: [Word8] -> ByteString .
--   This means that it takes a list of bytes of type Word8 and returns a ByteString
--   . You can think of it as taking a list, which is lazy, and making it less lazy,
--   so that it’s lazy only at 64KB intervals.
--
-- - The Word8 type is like Int , but it represents an unsigned 8-bit number.
--   This means that it has a much smaller range of only 0 to 255. And just like
--   Int, it’s in the Num type class. For instance, we know that the value 5 is
--   poly- morphic in that it can act like any numeric type, including Word8
--
-- - Here’s how we pack lists of numbers into bytestrings:
--   ghci> B.pack [99,97,110]
--   Chunk "can" Empty
--
--   ghci> B.pack [98..120]
--   Chunk "bcdefghijklmnopqrstuvwx" Empty
--
--   We packed only a handful of values into a bytestring, so they fit inside
--   one chunk. Empty is like [] for lists—they both represent an empty sequence. As
--   you can see, you don’t need to specify that your numbers are of type Word8,
--   because the type system can make numbers choose that type. If you try to use a
--   big number like 336 as a Word8 , it will just wrap around to 80.
--
-- - When we need to examine a bytestring byte by byte, we need to unpack
--   it. The unpack function is the inverse of pack . It takes a bytestring and
--   turns it into a list of bytes. Here’s an example:
--
--   ghci> let by = B.pack [98,111,114,116]
--   ghci> by
--   Chunk "bort" Empty
--   ghci> B.unpack by
--   [98,111,114,116]
--
-- - You can also go back and forth between strict and lazy bytestrings. The
--   toChunks function takes a lazy bytestring and converts it to a list of strict
--   ones. The fromChunks function takes a list of strict bytestrings and converts
--   it to a lazy bytestring:
--
--   ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
--   Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
--
--   This is good if you have a lot of small strict bytestrings and you want to
--   process them efficiently without joining them into one big strict bytestring in
--   memory first.
--
-- - The bytestring version of : is called cons . It takes a byte and a bytestring
--   and puts the byte at the beginning.
--
--   ghci> B.cons 85 $ B.pack [80,81,82,84]
--   Chunk "U" (Chunk "PQRT" Empty)
--
-- - The bytestring modules have a load of functions that are analogous to
--   those in Data.List, including, but not limited to, head , tail , init , null ,
--   length , map, reverse, foldl, foldr , concat , takeWhile , filter , and so on.
--   For a complete listing of bytestring functions, check out the documentation for
--   the byte- string package at http://hackage.haskell.org/package/bytestring/. The
--   bytestring modules also have functions that have the same name and behave the
--   same as some functions found in System.IO , but Strings are replaced with
--   ByteStrings . For instance, the readFile function in System.IO has this type:
--
--   readFile :: FilePath -> IO String
--
--   The readFile function from the bytestring modules has the following type:
--
--   readFile :: FilePath -> IO ByteString
--
-- - if you’re using strict bytestrings and you attempt to read a file, all of
--   that file will be read into memory at once! With lazy bytestrings, the file
--   will be read in neat chunks
--
-- ## Copying files with bytestrings
--
--  - A program that takes two filenamea as command line arguments and copies
--  the first file into the second file
--

import Control.Exception
import qualified Data.ByteString as B
import System.Directory
import System.Environment
import System.IO

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

-- - Whenever you need better performance in a program that reads a lot of data into
--   strings, give bytestrings a try. Chances are you’ll get some good performance
--   boosts with very little effort on your part. I usually write pro- grams using
--   normal strings and then convert them to use bytestrings if the performance is
--   not satisfactory.
