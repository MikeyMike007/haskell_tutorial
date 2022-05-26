# Syntax in functions

## Examples of pattern matching

```haskell
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

```

## Pattern matching with tuples

```haskell
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
```

## Pattern matching with lists and list comprehension

```haskell
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

```

## As-patterns

- Allows you to break up an item according to a pattern, while still keeping a reference to the entire orginal item.

```haskell
firstLetter :: String -> String
firstLetter "" = "Empty String, whoops!"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ show x
```

## Guards

```haskell
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

```

## where

```haskell
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
```

## where's scope

- The variables we define in the `where` section of a function are only visible only to that function.
- If we want to use variables over fifferent functions, we need to define them globally
- `where` bindings are not shared across fucntion bodies of different patterns

This wont work,

```haskell
greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name
where niceGreeting = "Hello! So very nice to see you,"
badGreeting = "Oh! Pfft. It's you."
```

while this works

```haskell
badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name
```

## Pattern matching with `where`

```haskell
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
```

## Functions in `where` blocks

```haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

```

## `let` expressions

- `where` allows you to bind to variables at the end of a functions
- `let` expressions allows you to bind variables anywhere and are expressions themselves.
- `let` expressions are local and dont span across guards
- `let` expressions can be used in pattern matching
- Since `let` expressions are expressions, and are fairly local in their scope, they cant be used across guards.
- Some prefer `where` bindings because their variables are defined after the function they are being used in, rather than before. This allows the function body to be closer to its name and type declaration, which can make for more readable code.

Some examples

```haskell
cylinder :: Double -> Double -> Double
cylinder r h =
  let topArea = pi * r ^ 2
      sideArea = 2 * pi * r * h
   in 2 * topArea + sideArea
```

```haskell
ghci> 4 * (let a = 9 in a + 1) + 2
42

ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
(6000000,"Hey there!")

ghci> (let (a, b, c) = (1, 2, 3) in a+b+c) * 100
600
```

## `let` in list comprehensions

```haskell
calcBmis1 :: [(Double, Double)] -> [Double]
calcBmis1 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]
```

## `let` in GHCI

```haskell
ghci> let zoot x y z = x * y + z
ghci> zoot 3 9 2
29

ghci> let boot x y z = x * y + z in boot 3 4 2
14

ghci> boot
<interactive>:1:0: Not in scope: `boot'
```

## Case expressions

Syntax:

```haskell
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
                  ...
```

Examples

```haskell
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
```
