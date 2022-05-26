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

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

flip0 :: (a -> b -> c) -> b -> a -> c
flip0 f y x = f x y
