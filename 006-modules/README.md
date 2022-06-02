# Modules

## Importing modules

### Import a whole module

Import all functions in `Data.List`. In the example below, the function `nub` is used that takes any list and weeds out any duplicates.

```haskell
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

You can also get access to functions of modules directly in `GHCi` by

```haskell
ghci> :m + Data.List
```

or several modules

```haskell
ghci> :m + Data.List Data.Map Data.Set
```

### Import selected functions from module

This imports only the functions `nub` and `sort` from `Data.List`

```haskell
import Data.List (nub, sort)
```

### Import all functions from a module except a few selected ones

Imports al functions except `nub`

```haskell
import Data.List hiding (nub)
```

### Qualified imports

Use the filter function in `Data.Map` instead of the one defined in the prelude.

```haskell
import qualified Data.Map
```

If you want to call the `filter` function, you need to type `Data.Map.filter`.

You can also create an alias for it

```haskell
import qualified Data.Map as M
```

Now you can call the filter function with `M.filter`.

## Solving problems with module functions

### Counting words

Suppose we have a string that contains a bunch of words, and we want to
know how many times each word appears in the string.

```haskell
wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words
```

Function `words` examples

```haskell
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]

ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
```

Function `group` examples

```haskell
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

ghci> group ["boom","bip","bip","boom","boom"]
[["boom"],["bip","bip"],["boom","boom"]]
```

Function `sort` examples

```haskell
ghci> sort [5,4,3,7,2,1]
[1,2,3,4,5,7]

ghci> sort ["boom","bip","bip","boom","boom"]
["bip","bip","boom","boom","boom"]
```

### Needle in a haystack

A function that takes two lists and tells us if the first list is wholly contained anywhere in the second list.

```haskell
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
```

Function `tails` examples

```haskell
ghci> tails "party"
["party","arty","rty","ty","y",""]

ghci> tails [1,2,3]
[[1,2,3],[2,3],[3],[]]
```

Function `isPrefixOf` examples

```haskell
ghci> "hawaii" `isPrefixOf` "hawaii joe"
True

ghci> "haha" `isPrefixOf` "ha"
False

ghci> "ha" `isPrefixOf` "ha"
True
```

Function `any` examples

```haskell
ghci> any (> 4) [1,2,3]
False

ghci> any (=='F') "Frank Sobotka"
True

ghci> any (\x -> x > 5 && x < 10) [1,4,11]
False
```

This function is also already defined in `Data.List` as `isInfixOf`.

### Caesar Cipher Salad

The Caesar cipher is a primitive method of
encoding messages by shifting each character
by a fixed number of positions in the alphabet.
We can easily create a sort of Caesar cipher of
our own, and we won’t constrict ourselves to the
alphabet—we’ll use the whole range of Unicode
characters.

```haskell
import Data.Char

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode shift = map (\c -> chr $ ord c - shift)

decode' :: Int -> String -> String
decode' shift = encode (negate shift)
```

Examples of function `ord` and `chr`.

```haskell
ghci> ord 'a'
97
ghci> chr 97
'a'
ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
```

```haskell
ghci> encode 3 "hey mark"
"kh|#pdun"
ghci> encode 5 "please instruct your men"
"uqjfxj%nsxywzhy%~tzw%rjs"
ghci> encode 1 "to party hard"
"up!qbsuz!ibse"
```

```haskell
ghci> decode 3 "kh|#pdun"
"hey mark"
ghci> decode 5 "uqjfxj%nsxywzhy%~tzw%rjs"
"please instruct your men"
ghci> decode 1 "up!qbsuz!ibse"
"to party hard"
```

### On strict left folds

Using foldl can sometimes lead to so-called stack overflow errors, which occur
when your program uses too much space in a specific part of your computer’s
memory.

This can happen due to that Haskell is lazy. And because Haskell is lazy, it
defers actual computation of values for as long as possible. When we use foldl ,
Haskell doesn’t compute (that is, evaluate) the actual accumulator on every
step. Instead, it defers its evaluation. In the next step, it again doesn’t
evaluate the accu- mulator, but defers the evaluation. It also keeps the old
deferred computation in memory, be- cause the new one often refers to its
result. Going forward, it builds up a bunch of deferred computations, each
taking a not insignificant amount of memory. Eventually, this can cause a stack
overflow error.

Example:

```haskell
ghci> foldl (+) 0 (replicate 1000000 1)
*** Exception: stack overflow
```

Heres how Haskell evaluates `foldl (+) 0 [1,2,3]`

```haskell
foldl (+) 0 [1,2,3] =
foldl (+) (0 + 1) [2,3] =
foldl (+) ((0 + 1) + 2) [3] =
foldl (+) (((0 + 1) + 2) + 3) [] =
((0 + 1) + 2) + 3 =
(1 + 2) + 3 =
3 + 3 =
```

i.e. it first builds up a big stack of deferred computations. Then, once it
reaches the empty list, it goes about actually evaluating those deferred
computations. This isn’t a problem for small lists, but for large lists that
contain upward of a million elements, you get a stack overflow, because
evaluating all these deferred computations is done recursively.

There is a function in `Data.List` called `foldl'` that solves this problem that evaluates the same expression `foldl' (+) 0 [1,2,3]` as

```haskell
foldl' (+) 0 [1,2,3] =
foldl' (+) 1 [2,3] =
foldl' (+) 3 [3] =
foldl' (+) 6 [] =
6
```

### Some cool numbers

what’s the first natural number such that the sum of its digits equals `n`?”

```haskell
firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]
```

```haskell
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
```

```haskell
ghci> show 100
"100"

ghci> digitToInt '2'
2
```

```haskell
ghci> :t find
find :: (a -> Bool) -> [a] -> Maybe a
```

What about the return value in function `find`? It says Maybe a . That’s a type
you haven’t met before. A value with a type of Maybe a is sort of like a list of
type [a]. Whereas a list can have zero, one, or many elements, a Maybe a typed
value can have ei- ther zero elements or just one element. We use it when we
want to represent possible fail- ure. To make a value that holds nothing, we
just use Nothing. Some examples,

```haskell
ghci> Nothing
Nothing

ghci> Just "hey"
Just "hey"

ghci> Just 3
Just 3

ghci> :t Just "hey"
Just "hey" :: Maybe [Char]

ghci> :t Just True
Just True :: Maybe Bool
```

```haskell
ghci> find (> 4) [3,4,5,6,7]
Just 5

ghci> find odd [2,4,6,8,9]
Just 9

ghci> find (=='z') "mjolnir"
Nothing
```

## Mapping keys to values

When dealing with data in some sort of collection, we often don’t care if it’s
in some kind of order; we just want to be able to access it by a certain key.

### Association lists (dictionaries)

```haskell
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
```

- If we dont find the key, it will return the head of an empty vector which throws a runtime error. Solution:

```haskell
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey' key xs
```

With `fold`

```haskell
findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing
```

```haskell
ghci> findKey "penny" phoneBook
Just "853-2492"

ghci> findKey "betty" phoneBook
Just "555-2938"

ghci> findKey "wilma" phoneBook
Nothing
```

### Enter `Data.Map`

- Data.Map module offers association lists that are much faster
- Data.Map.fromList takes an association list and returns a map witht the same associations
- If there are duplicates keys in the orignal association list, the duplicates are discarded
- Signature of `fromList`
- Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
- It says it takes a list of pairs of type k and v and returns a map that maps from keys of type k to values of type v.

```haskell
import qualified Data.Map as Map
```

```haskell
ghci> Map.fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
fromList [(3,"shoes"),(4,"trees"),(9,"bees")]

ghci> Map.fromList [("kima","greggs"),("jimmy","mcnulty"),("jay","landsman")]
fromList [("jay","landsman"),("jimmy","mcnulty"),("kima","greggs")]
```

```haskell
ghci> Map.fromList [("MS",1),("MS",2),("MS",3)]
fromList [("MS",3)]
```

```haskell
import qualified Data.Map as Map
phoneBook :: Map.Map String String
phoneBook = Map.fromList $
[("betty", "555-2938")
,("bonnie", "452-2928")
,("patsy", "493-2928")
,("lucille", "205-2928")
,("wendy", "939-8282")
,("penny", "853-2492")
]
```

```haskell
ghci> :t Map.lookup
Map.lookup :: (Ord k) => k -> Map.Map k a -> Maybe a

ghci> Map.lookup "betty" phoneBook
Just "555-2938"

ghci> Map.lookup "wendy" phoneBook
Just "939-8282"

ghci> Map.lookup "grace" phoneBook
Nothing
```

```haskell
ghci> :t Map.insert
Map.insert :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a

ghci> Map.lookup "grace" phoneBook
Nothing

ghci> let newBook = Map.insert "grace" "341-9021" phoneBook

ghci> Map.lookup "grace" newBook
Just "341-9021"
```

```haskell
ghci> :t Map.size
Map.size :: Map.Map k a -> Int

ghci> Map.size phoneBook
6

ghci> Map.size newBook
7
```

Suppose we would rather use lists of Int s to represent phone numbers. So,
instead of having a number like "939-8282" , we want to have `[9,3,9,8,2,8,2]`.

```haskell
string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
```

```haskell
ghci> string2digits "948-9282"
[9,4,8,9,2,8,2]
```

```haskell
ghci> let intBook = Map.map string2digits phoneBook
ghci> :t intBook
intBook :: Map.Map String [Int]

ghci> Map.lookup "betty" intBook
Just [5,5,5,2,9,3,8]
```

Let’s extend our phone book. Say that a person can have several num-
bers, and we have an association list set up like this:

```haskell
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

```

If we just use fromList to put that into a map, we’ll lose a few numbers!
Instead, we’ll use another function found in Data.Map : fromListWith . This
function acts like fromList , but instead of discarding duplicate keys, it uses a function supplied to it to decide what to do with them

If fromListWith finds that the key is already there, it uses the function sup-
plied to it to join those two values into one and replaces the old value with
the one it got by passing the conflicting values to the function.

```haskell
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
  where
    add number1 number2 = number1 ++ ", " ++ number2
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"

ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282"

ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
"342-2492, 555-2938"
```

We could also re-write the function `phoneBookToMap` as,

```haskell
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook

["827-9162","943-2929","493-2928"]
```

Another example is when we’re making a map from an association list of numbers,and when a duplicate key is found, we want the biggest value for the key to be kept.

```haskell
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
```

Another example is when we choose to add the keys,

```haskell
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,108),(3,62),(4,37)]
```

## Make your own modules

### A Geometry module

```haskell
-- Specify the functions this files export
-- To use this module, please import it by stating import Geometry in your file
module Geometry
  ( sphereVolume,
    sphereArea,
    cubeVolume,
    cubeArea,
    cuboidArea,
    cuboidVolume,
  )
where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b
```

### Hierarchical Modules

Create a folder called `Geometry` and create following files in it,

**`Sphere.hs`**

```haskell
module Geometry.Sphere
  ( volume,
    area,
  )
where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
```

**`Cuboid.hs`**

```haskell
module Geometry.Cuboid
  ( volume,
    area,
  )
where

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b
```

**`Cube.hs`**

```haskell
module Geometry.Cube
  ( volume,
    area,
  )
where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

Now you can import the modules in a file that is one more step closer to the root folder. See example below,

**`GeometryNew.hs`**

```haskell
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Sphere as Sphere
```
