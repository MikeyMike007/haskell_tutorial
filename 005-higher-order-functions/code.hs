multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

compareWith100 :: Int -> Ordering
compareWith100 x = compare 100 x

compareWith100' :: Int -> Ordering
compareWith100' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- First argument: A function that takes two parameters a and b and returns c
-- Second argument: A list a
-- Third argument: A list b
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip0' :: (a -> b -> c) -> (b -> a -> c)
flip0' f = g
  where
    g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- The functional programmers toolbox
--
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x : xs) =
  let smallerOrEqual = filter' (>= x) xs
      largerThan = filter' (< x) xs
   in quickSort' smallerOrEqual ++ [x] ++ quickSort' largerThan

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- If we account that functions are curried

sum0' :: (Num a) => [a] -> a
sum0' = foldl (+) 0

-- Creating the map function with foldr
-- map2 :: (a -> b) -> [a] -> [b]
-- map2 f xs = foldr (\acc x -> f x : acc) [] xs
--
-- -- Creating the map function with foldl
-- map1 :: (a -> b) -> [a] -> [b]
-- map1 f xs = foldl (\x acc -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- maximum' :: [a] -> a
-- maximum' = foldl1 max
--
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter1 :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter1 p = foldl (\acc x -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums' :: Int
sqrtSums' = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (< 10000) . filter odd $ map (^ 2) [1 ..]
