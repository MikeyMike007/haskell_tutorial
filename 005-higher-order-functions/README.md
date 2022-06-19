# Higher-order functions

Higher-order functions are functions that can take functions as parameters and return functions as return values.

## Curried-functions

- Every function in Haskell takes only one parameter
- Curried function: A function that takes only one parameter
- When the function is called with that parameter, it returns a function that takes the next parameter
- if we call a function with too few parameters, we get back a partially applied function, which is a function that takes as many parameters as we left out

Example,

```haskell
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
```

```haskell
ghci> let mult1 = multThree 3
ghci> let mult2 = mult1 3
ghci> let mult3 = mult2 3
ghci> mult3
27
```

These functions are basically the same thing

```haskell
compareWith100 :: Int -> Ordering
compareWith100 x = compare 100 x

compareWith100' :: Int -> Ordering
compareWith100' = compare 100
```

## Sections

Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and supply a parameter on only one side. That creates a function that takes one parameter and then applies it to the side that’s missing an operand.

An example that divides a number with ten using `section` syntax. Note that calling `divideByTen` 200 is equivalent to calling `200 / 10` or `(/10) 200`.

```haskell
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)
```

Another example that takes a character as input and checks whether is written in uppercase letters,

```haskell
isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A' .. 'Z'])
```

## Some higher-orderism

Examples

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

```haskell
ghci> applyTwice (+3) 10
16

ghci> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"

ghci> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"

ghci> applyTwice (multThree 2 2) 9
144

ghci> applyTwice (3:) [1]
[3,3,1]
```

## Implementation of `zipWith`

`zipWith'` takes a function and two lists as parameters, and then joins the two lists by applying the function between corresponding elements.

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
```

```haskell
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]

ghci> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]

ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters","bar hoppers","baz aldrin"]

ghci> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]

ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
```

## Implementation of `flip`

The `flip` function takes a function and returns a function that is like our original function, but with the first two arguments flipped.

```haskell
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x
```

You can see from the type declaration that `flip'` takes a function that takes `a`
and `b` types, and returns a function that takes `b` and `a` types. But because
functions are curried by default, the second pair of parentheses actually is not
necessary.

In this new version of `flip'` , we take advantage of the fact that functions
are curried. When we call `flip' f` without the parameters `y` and `x`, it will re-
turn an `f` that takes those two parameters but calls them flipped.
Even though flipped functions are usually passed to other functions,
we can take advantage of currying when making higher-order functions by
thinking ahead and writing what their end result would be if they were fully
applied.

```haskell
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
-- This could also be written as: flip' f x y = f y x
```

Lets see some examples with using the `flip'` function.

```haskell
ghci> zip [1,2,3,4,5] "hello"
[(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]

ghci> flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]

ghci> zipWith div [2,2..] [10,8,6,4,2]
[0,0,0,0,1]

ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]
```

## The functional programmers toolbox - Implementations

### `map`

The map function takes a function and a list, and applies that function to every element in the list, producing a new list

```haskell
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs
```

Lets see some examples of `map` in action,

```haskell
ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]

ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]

ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]

ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
```

### `filter`

The filter function takes a predicate and a list, and returns the list of ele-
ments that satisfy that predicate. (Remember that a predicate is a function
that tells whether something is true or false; that is, a function that returns
a Boolean value.)

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs -- i.e. p x == True = ...
  | otherwise = filter' p xs
```

Some examples with `filter` in action,

```haskell
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]

ghci> filter (==3) [1,2,3,4,5]
[3]

ghci> filter even [1..10]
[2,4,6,8,10]

ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
[[1,2,3],[3,4,5],[2,2]]

ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"

ghci> filter (`elem` ['A'..'Z']) "i LAuGh at you bEcause u R all the same"
"LAGER"
```

### More examples with `map` and `filter`

Example of implementation of the quicksort algorithm with `filter`,

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger
```

Another example of a function that finds the largest number under 100,000 that’s divisible by 3,829,

```haskell
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0
```

Some other examples,

```haskell
ghci> takeWhile (/=' ') "elephants know how to party"
"elephants"

ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650

ghci> sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])
166650
```

Example of Collatz sequence,

```haskell
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n: chain (n `div` 2)
  | odd n = n: chain (n*3 + 1)
```

```haskell
ghci> chain 10
[10,5,16,8,4,2,1]

ghci> chain 1
[1]

ghci> chain 30
[30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
```

```haskell
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15
```

```haskell
ghci> let listOfFuns = map (*) [0..]

ghci> (listOfFuns !! 4) 5
20
```

## Lambdas

Lambdas are anonymous functions that we use when we need a function only once.
Normally, we make a lambda with the sole purpose of passing it to a
higher-order function. To declare a lambda, we write a \ (because it kind of looks like the Greek letter lambda
if you squint hard enough), and then we write the function’s parameters, sep-
arated by spaces. After that comes a -> , and then the function body. We usually surround lambdas with parentheses.

Some examples of using labmdas,

```haskell
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
```

```haskell
ghci> map (+3) [1,6,3,2]
[4,9,6,5]
ghci> map (\x -> x + 3) [1,6,3,2]
[4,9,6,5]

```

```haskell
ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]
```

```haskell
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
[3,8,9,8,7]
```

## Fold

Folds can be used to implement any function where you traverse a list
once, element by element, and then return something based on that. When-
ever you want to traverse a list to return something, chances are you want
a fold. A fold takes a binary function (one that takes two parameters, such as `+` or
`div`), a starting value (often called the accumulator ), and a list to fold up.

### Left fold

Creating the `sum` function with `foldl`.

```haskell
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
```

Following code is exactly the same as above since functions are curied,

```haskell
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
```

An example of the implementation of the `map` function using `foldl`.

```haskell
map :: (a -> b) -> [a] -> [b]
map f xs = foldl (\x acc -> acc ++ [f x]) [] xs
```

### Right fold

An example that implements the `map` function using `foldr`,

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\acc x -> f x : acc) [] xs
```

An example that implements the `elem` function with `foldr`,

```haskell
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys
```

### `foldl1` and `foldr1`

The `foldl1` and `foldr1` functions work much like `foldl` and `foldr` , except that
you don’t need to provide them with an explicit starting accumulator. They
assume the first (or last) element of the list to be the starting accumulator,
and then start the fold with the element next to it

An example that implements the `maximum` function with `foldl1`,

```haskell
maximum' :: (Ord a) -> [a] -> a
maximum' = foldl1 max
```

### Some more fold examples

```haskell
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter1 :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter1 p = foldl (\acc x -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
```

## Scans

The `scanl` and `scanr` functions are like `foldl` and `foldr` , except they report
all the intermediate accumulator states in the form of a list. The `scanl1` and
`scanr1` functions are analogous to `foldl1` and `foldr1`.

```haskell
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]

ghci> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]

ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]

ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]
```

Scans are used to monitor the progress of a function that can be imple-
mented as a `fold`. As an exercise in using scans, let’s try answering this ques-
tion: How many elements does it take for the sum of the square roots of all
natural numbers to exceed 1,000?

```haskell
sqrtSums' :: Int
sqrtSums' = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1
```

## Function application with `$`

`$` is called the **function application operator**

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

Function application with a space is left-associative (so`f a b c` is the same as `((f a) b) c )`, while function application with $ is right-associative.

When a `$` is encountered, the expression on its right is applied as the parameter to the function on its left.

The `$` function is right-associative, meaning that something like `f $ g $ x` is equivalent to `f $ (g $ x)` .

These two examples are the same,

```haskell
ghci> sum (map sqrt [1..130])

ghci> sum $ map sqrt [1..130]
```

Some more examples,

```haskell
ghci> sqrt 3 + 4 + 9 -- 9 + 4 + 9 = 22
ghci> sqrt (3 + 4 + 9) -- sqrt 16 = 4
ghci sqrt $ 3 + 4 + 9 -- Same as above i.e. sqrt 16 = 4
```

More examples,

```haskell
ghci> sum (filter (> 10) (map (*2) [2..10]))
80

ghci> sum $ filter (> 10) (map (*2) [2..10])
80

ghci> sum $ filter (> 10) $ map (*2) [2..10]
80
```

Apart from getting rid of parentheses, $ lets us treat function application like just another function. This allows us to, for instance, map function application over a list of functions, like this example,

```haskell
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]
[7.0, 30.0, 9.0, 1.7320508075688772]
```

Here, the function `($ 3)` gets mapped over the list. If you think about what the `($ 3)` function does, you’ll see that it takes a function and then applies that function to `3`.

## Function composition

In mathematics, function composition is defined like this: `(f o g)(x) = f (g(x))`. This means that composing two functions is the equivalent of calling one function with some value and then calling another function with the result of the first function.

In Haskell, function composition is pretty much the same thing. We do function composition with the `.` function, which is defined like this:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

Some examples

```haskell
ghci> map (\x -> negate (abs x))  [5, -3, -6, 7]
[-5,-3, -6, -7]

ghci> map (negate . abs) [5, -3, -6, 7]
[-5, -3, -6, -7]

ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
[-14,-15,-27]

ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
[-14,-15,-27]

```

## Function composition with multiple parameters

Following examples means the same thing

```haskell
sum (replicate 5 (max 6.7 8.9))
(sum . replicate 5) max 6.7 8.9
sum . replicate 5 $ max 6.7 8.9
```

Following examples means the same thing

```haskell
replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
```

Following examples means the same thing

```haskell
fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50
```

Following examples means the same thing

```haskell
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (< 10000) . filter odd $ map (^ 2) [1 ..]
```
