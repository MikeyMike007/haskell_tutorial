# Higher-order functions

- Higher-order functions: Functions can taker functions as parameters and return functions as return values

## Curried-functions

- Every function in Haskell takes only one parameter
- Curried function: A function that takes only one parameter.
- When the function is called with that parameter, it returns a function that takes the next parameter
- if we call a function with too few parameters, we get back a partially applied function, which is a function that takes as many parameters as we left out

Example

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

Examples

- Calling divideByTen 200 is equivalent
  to calling 200 / 10 or (/10) 200

```haskell
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)
```

```haskell
isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A' .. 'Z'])
```

## Some higher-orderism is in order

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

## Implementing `zipWith'`

`zipWith'` takes a function and two lists as parameters, and then joins the two
lists by applying the function between corresponding elements.

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

## Implementing `flip`

The `flip` function takes a function and returns a function that is like our
original function, but with the first two arguments flipped.

```haskell
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x
```

- You can see from the type declaration that flip' takes a function that
  takes a and b types, and returns a function that takes b and a types. But
  because functions are curried by default, the second pair of parentheses
  actually is not necessary.

- In this new version of flip' , we take advantage of the fact that functions
  are curried. When we call flip' f without the parameters y and x, it will re-
  turn an f that takes those two parameters but calls them flipped.
  Even though flipped functions are usually passed to other functions,
  we can take advantage of currying when making higher-order functions by
  thinking ahead and writing what their end result would be if they were fully
  applied.

```haskell
flip0 :: (a -> b -> c) -> b -> a -> c
flip0 f y x = f x y
```
