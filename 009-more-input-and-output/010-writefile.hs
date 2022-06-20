import Data.Char ( toUpper )
import System.IO ()

main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" $ map toUpper contents
