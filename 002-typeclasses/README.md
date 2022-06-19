# Types and type classes

## Type declarations

The `::` operator can be seen as "has type of".

```haskell
ghci> :t 'a'
'a' :: Char

ghci> :t True
True :: Bool

ghci> :t "HELLO!"
"HELLO!" :: [Char]

ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)

ghci> :t 4 == 5
4 == 5 :: Bool
```

## Function types

Here some functions with type declarations

Following function takes a `[Char]` and returns a `[Char]`.

```haskell
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]
```

Following function takes three `Int` as arguments and returns an `Int`.

```haskell
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

Following function takes an `Integer` and returns an `Integer`.

```haskell
factorial :: Integer -> Integer
factorial n = product [1 .. n]
```

Following function takes a `Float` and returns a `Float`.

```haskell
circumference :: Float -> Float
circumference r = 2 * pi * r
```

You can also inspect the function definition by running `ghci> :t functionName`.

## Type variables

`a` below is a type variable which means that `a` can be of any type. Functions that use type variables are called polymorphic functions.

```haskell
ghci> :t fst
fst :: (a, b) -> a

ghci> :t head
head :: [a] -> a
```

## Type classes

A type class is an interface that defines some behaviour. If a type is an instance of a type class, then it supports and implements the behaviour the type describes.

As an example, the definition/type of `==` is,

```haskell
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool
```

Everything before the `=>` sign is called a **class constraint**. It says that the equality function `==` takes any two values that are of the same type and returns a `bool`. The type of those two values must be an instance of the `Eq` class. The `Eq` type class provides an interface for testing for equality.

## The `Eq` type class

`Eq` is a type class for types that support equality testing.

## The `Ord` type class

`Ord` is a type class for types whose values can be put in some order.

The type of the `>` variable is:

```haskell
ghci> :t (>)
(>) :: (Ord a) => a -> a -> Bool
```

The `compare` function takes two values whose type is an `Ord` instance and returns an `Ordering`. `Ordering` is a type that can be `GT`, `LT`, or `EQ`.

```haskell
ghci> "Abrakadabra" < "Zebra"
True

ghci> "Abrakadabra" `compare` "Zebra"
LT

ghci> 5 >= 2
True

ghci> 5 `compare` 3
GT

ghci> 'b' > 'a'
True
```

## The Show type class

Values whose types are instances of the `Show` type class can be represented as strings. The most commonly used function that operates on instances of this type class is `show`, which prints the given value as a string,

```haskell
ghci> show 3
"3"

ghci> show 5.334
"5.334"

ghci> show True
"True"
```

## The Read type class

Opposite to `Show`. The `read` function takes a string and returns a value whose type is an instance of `Read`.

```haskell
ghci> read "True" || False
True

ghci> read "8.2" + 3.8
12.0

ghci> read "5" - 2
3

ghci> read "[1,2,3,4]" ++ [3]
[1,2,3,4,3]
```

Type annotations are a way to explicitly tell Haskell what the type of an expression should be. We do this by adding `::` to the end of the expression and then specifying the type.

```haskell
ghci> read "5" :: Int
5

ghci> read "5" :: Float
5.0

ghci> (read "5" :: Float) * 4
20.0

ghci> read "[1,2,3,4]" :: [Int]
[1,2,3,4]

ghci> read "(3, 'a')" :: (Int, Char)
(3, 'a')
```

## The Enum Type Class

- Enum instrances are sequanetially ordered types i.e. their values can be enumerated
- Main advantage: We can use its values in list ranges
- They have defined successors and predecessors, which we can get with the `succ` and `pred` functions.
- Examples of types in this class are: `Bool`, `Char`, `Ordering`, `Int`, `Integer`, `Float`, and `Double`

```haskell
ghci> ['a'..'e']
"abcde"

ghci> [LT .. GT]
[LT,EQ,GT]

ghci> [3 .. 5]
[3,4,5]

ghci> succ 'B'
'C'
```

## The Bounded Type Class

- Instances of the `Bounded` type class have an upper bound and a lower bound.

```haskell
ghci> minBound :: Int
-2147483648

ghci> maxBound :: Char
'\1114111'

ghci> maxBound :: Bool
True

ghci> minBound :: Bool
False

ghci> maxBound :: (Bool, Int, Char)
(True,2147483647,'\1114111')
```

## The Num Type Class

- `Num` is a numeric type class. Its instances can act like numbers.
- Whole numbers are also polymorphic constans. They can act like any type thats an instance of the `Num` type class. (`Int`, `Integer`, `Float`, `Double`)

```haskell
ghci> :t 20
20 :: (Num t) => t

ghci> 20 :: Int
20

ghci> 20 :: Integer
20

ghci> 20 :: Float
20.0

ghci> 20 :: Double
20.0

ghci> :t (*)
(*) :: (Num a) => a -> a -> a
```

## The Floating Type Class

- `Floating` type class includes `Float` and `Double` types.

## The Integral Type Class

- Includes only integral (whole) numbers.
- Includes `Int` and `Integer` types.
- `fromIntegral` is a useful function when you want integral and floating point numbers type work together.

```haskell
fromIntegral :: (Num b, Integral a) => a -> b

length :: [a] -> Int

ghci> fromIntegral (length [1,2,3,4]) + 3.2
7.2
```

## Final notes

- A type class defines an abstract interface
- One type can be instances of many type classes
- `Char` is an instance of `Eq` and `Ord`, because we compare two character.
- Sometimes a type must first be an instance of one type class to be allowed to become an instance of another. For example, to be an instance of `Ord`, a type must first be an instance of `Eq`
