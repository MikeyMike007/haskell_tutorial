lucky :: Int -> String
lucky 7 = "Lucky Number Seven"
lucky x = "Sorry, you are out of luck!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "Cant call head on empty list"
head' (x : _) = x

-- Please note that [1,2,3] is synthatic sugar for 1:2:3:[].
-- A Pattern like x:xs will bind the head of the list to x and the rest of it to xs

tell :: (Show a) => [a] -> String
tell [] = "An empty list"
tell (x : []) = "This list has only one element:" ++ show x
tell (x : y : []) = "This list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list has many elements, the first one is" ++ show x ++ " and the second is " ++ show y

-- We could also re-write this as

tell' :: (Show a) => [a] -> String
tell' [] = "An empty list"
tell' [x] = "This list has only one element:" ++ show x
tell' [x, y] = "This list has two elements: " ++ show x ++ " and " ++ show y
tell' (x : y : _) = "This list has many elements, the first one is" ++ show x ++ " and the second is " ++ show y

addTriplett :: (Num a) => [a] -> a
addTriplett (x : y : z : []) = x + y + z

-- Could also write as
addTriplett' :: (Num a) => [a] -> a
addTriplett' [x, y, z] = x + y + z

firstLetter :: String -> String
firstLetter "" = "Empty String, whoops!"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ show x

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You are underweight"
  | bmi <= 25.0 = "You are normal"
  | bmi <= 30.0 = " You are fat"
  -- otherwise catchcall guard
  | otherwise = "You are a whale"

bmiTell1 :: Double -> Double -> String
bmiTell1 weight height
  | weight / height ^ 2 <= 18.5 = "You are underweight"
  | weight / height ^ 2 <= 25.0 = "You are normal"
  | weight / height ^ 2 <= 30.0 = "You are fat"
  | otherwise = "You are a whale"

bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
  | bmi <= 18.5 = "You are normal"
  | bmi <= 25.0 = "You are normal"
  | bmi <= 30.0 = "You are fat"
  | otherwise = "You are a whale"
  where
    bmi = weight / height ^ 2

bmiTell3 :: Double -> Double -> String
bmiTell3 weight height
  | bmi <= skinny = "You are underweight"
  | bmi <= normal = "You are normal"
  | bmi <= fat = "You are fat"
  | otherwise = "You are a whale"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

bmiTell4 :: Double -> Double -> String
bmiTell4 weight height
  | bmi <= skinny = "You are underweight"
  | bmi <= normal = "You are normal"
  | bmi <= fat = "You are fat"
  | otherwise = "You are a whale"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
  let topArea = pi * r ^ 2
      sideArea = 2 * pi * r * h
   in 2 * topArea + sideArea

calcBmis1 :: [(Double, Double)] -> [Double]
calcBmis1 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

head0 :: [a] -> a
head0 [] = error "No head for empty list"
head0 (x : _) = x

-- Same but with case expressions

head1 :: [a] -> a
head1 xs = case xs of
  [] -> error "No head for empty list"
  (x : _) -> x

describeList :: [a] -> String
describeList ls =
  "The list is " ++ case ls of
    [] -> "empty."
    (x : []) -> "a singleton list"
    (x : y : []) -> "A list with two items"
    (x : y : z) -> "A list with many items"

describeList' :: [a] -> String
describeList' ls =
  "The list is " ++ case ls of
    [] -> "empty."
    [x] -> "a singleton list"
    [x, y] -> "a list with two items"
    xs -> "a list with many items"
