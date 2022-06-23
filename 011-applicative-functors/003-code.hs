import Data.Char ( toUpper )
import Data.List ( intersperse )

main = do
  line <- fmap (\xs -> intersperse '-' (reverse (map toUpper xs))) getLine
  putStrLn line
