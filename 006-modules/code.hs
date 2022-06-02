import Data.Char
import Data.IntMap (fromListWith)
import Data.List
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode shift = map (\c -> chr $ ord c - shift)

decode' :: Int -> String -> String
decode' shift = encode (negate shift)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- Gets the first key/value-pair that matches
-- If we dont find the key, it will return the head of an empty vector which throws a runtime error
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k, v) -> key == k)

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

-- Enter Data.Map
-- Data.Map module offers association lists that are much faster
-- Data.Map.fromList takes an association list and returns a map witht the same associations
-- If there are duplicates keys in the orignal association list, the duplicates are discarded
-- Signature of `fromList`
-- Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
-- It says it takes a list of pairs of type k and v and returns a map that maps from keys of type k to values of type v.
--
phoneBook' :: Map.Map String String
phoneBook' =
  Map.fromList $
    [ ("betty", "555-2938"),
      ("bonnie", "452-2928"),
      ("patsy", "493-2928"),
      ("lucille", "205-2928"),
      ("wendy", "939-8282"),
      ("penny", "853-2492")
    ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBook'' =
  [ ("betty", "555-2938"),
    ("betty", "342-2492"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("patsy", "943-2929"),
    ("patsy", "827-9162"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492"),
    ("penny", "555-2111")
  ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
  where
    add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

-- Making your own modules
