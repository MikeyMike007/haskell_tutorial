import Data.Char ( toUpper )
import Data.List ( intersperse )

main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line
