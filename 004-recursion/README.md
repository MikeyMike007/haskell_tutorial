# Recursion

Lets re-create some well known functions by using recursion.

## `maximum`

The `maximum` function takes a list of things that can be put in order (i.e.,
instances of the Ord type class) and returns the largest of them.

```haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)
```

## `replicate`

`replicate` takes an `Int` and a value, and returns a list that has several repetitions of that value

```haskell
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x
```

## `take`

The `take` function returns a specified number of elements from a specified list.

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs
```

## `reverse`

The `reverse` function takes a list and returns a list with the same elements, but in the reverse order.

```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]
```

## `repeat`

The `repeat` function takes an element and returns an infinite list composed of that element.

```haskell
repeat' :: a -> [a]
repeat' x = x : repeat' x
```

## `zip`

zip takes two lists and zips them together.

```haskell
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
```

## `elem`

This function takes a value and a list, and checks whether the value is a member of the list.

```haskell
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = a `elem'` xs
```

## `quicksort`

The `quicksort` algorithm implemented in haskell.

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger
```
