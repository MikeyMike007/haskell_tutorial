import Data.Char
import System.IO

main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" $ map toUpper contents
