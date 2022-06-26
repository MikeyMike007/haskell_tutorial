# Monoids

- The type class `Monoid` is for types whose values can be combined toether with a binary operation.

## Wrapping existing types into a new type

- You can make new types out of existing data types with the keyword `newtype`

- There two couple of ways for the list type to be an applicative functor. One way is to have `<*>` take every function out of the list that is its left parameter and apply that to every value in the list that is on the right, resulting in every possible combination of applying a function from the left list to a value in the right list:

- The second way is like zipping two lists together

- Lists are already an instance of `Applicative`, so how do we also make lists an instance of `Applicative` in this second way? As you learned, the `ZipList` a type was introduced for this reason.

```haskell
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3] $
[2,200,15]
```

- One way of declare the `ZipList` type,

```haskell
data ZipList a = ZipList [a]
```

This is a type that has just one value constructor, and that value constructor has just one field that is a list of things. We might also want to use record syntax so that we automatically get a function that extracts a list from a `ZipList`:

```haskell
data ZipList a = ZipList { getZipList :: [a] }
```

- We had two ways of making an existing type an instance of a type class, so we used the data keyword to just wrap that type into another type and made the other type an instance in the second way. The newtype keyword in Haskell is made exactly for cases when we want to just take one type and wrap it in something to present it as another type,

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }
```

- Pro with `newtype` - Faster
- Con with `newtype` - When you make a new type from an existing type by using the newtype keyword, you can have only one value constructor, and that value constructor can have only one field.

```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
```

```haskell
ghci> CharList "this will be shown!"
CharList {getCharList = "this will be shown!"}

ghci> CharList "benny" == CharList "benny"
True

ghci> CharList "benny" == CharList "oisters"
False
```

In this particular `newtype` , the value constructor has the following type,

```haskell
CharList :: [Char] -> CharList
```

It takes a `[Char]` value, such as "my sharona" and returns a `CharList` value.

Conversely, the `getCharList` function, which was generated for us because we used record syntax in our `newtype` , has this type:

```haskell
getCharList :: CharList -> [Char]
```

It takes a `CharList` value and converts it to a `[Char]` value. You can think of this as wrapping and unwrapping,

## Using `newtype` to make type class `instances`

- Now what if we wanted to make the tuple an instance of `Functor` in such a way that when we `fmap` a function over a tuple, it is applied to the first component of the tuple? That way, doing `fmap (+3) (1, 1)` would result in `(4, 1)`.

- We can `newtype` our tuple in such a way that the second type parameter represents the type of the first component in the tuple,

```haskell
newtype Pair b a = Pair { getPair :: (a, b) }
```

And now we can make it an instance of `Functor` so that the function is mapped over the first component:

```haskell
instance Functor (Pair c) where
fmap f (Pair (x, y)) = Pair (f x, y)
```

- If we imagine what the type `fmap` would be if it worked only on our new pairs, it would look like this:

```haskell
fmap :: (a -> b) -> Pair c a -> Pair c b
```

- `Pair c` took the place of the `f` in the type class definition for `Functor`:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Now if we convert a tuple into a `Pair b a` , we can use `fmap` over it, and the function will be mapped over the first component:

```haskell
ghci> getPair $ fmap (*100) (Pair (2, 3))
(200,3)

ghci> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol",3)
```

## On `newtype` laziness

- Haskell doesn’t need to evaluate any other elements in a list if we want to see only the first element.

```haskell
ghci> head [3,4,5,undefined,2,undefined]
3
```

```haskell
data CoolBool = CoolBool { getCoolBool :: Bool }
```

```haskell
helloMe :: CoolBool -> String
helloMe (CoolBool \_) = "hello"
```

```haskell
ghci> helloMe undefined
"*** Exception: Prelude.undefined
```

Types defined with the `data` keyword can have multiple value constructors (even though `CoolBool` has only one). So in order to see if the value given to our function conforms to the `(CoolBool _)` pattern, Haskell must evaluate the value just enough to see which value constructor was used when we made the value. And when we try to evaluate an undefined value, even a little, an exception is thrown.

```haskell
newtype CoolBool = CoolBool { getCoolBool :: Bool }
```

```haskell
ghci> helloMe undefined
"hello"
```

- Haskell can internally represent the values of the new type in the same way as the original values. It doesn’t need to add another box around them; it just must be aware of the values being of different types. And because Haskell knows that types made with the newtype keyword can have only one constructor, it doesn’t need to evaluate the value passed to the function to make sure that the value conforms to the `(CoolBool _)` pattern, because `newtype` types can have only one possible value constructor and one field!

## `type` vs `newtype` vs `data`

- The `type` keyword is for making `type` synonyms i.e.

```haskell
type IntList = [Int]
```

- We use type synonyms when we want to make our type signatures more descriptive.

- The `newtype` keyword is for taking existing types and wrapping them in new types, mostly so it’s easier to make them instances of certain type classes.

```haskell
newtype CharList = CharList { getCharList :: [Char] }
```

- We can’t use `++` to put together a `CharList` and a list of type `[Char]` . We can’t even use `++` to put together two `CharList` lists, because `++` works only on lists, and the `CharList` type isn’t a list, even though it could be said that `CharList` contains a list. We can, however, convert two `CharList`'s to lists, `++` them, and then convert that back to a `CharList`.

- When we use record syntax in our `newtype` declarations, we get functions for converting between the new type and the original type — namely the value constructor of our `newtype` and the function for extracting the value in its field. The new type also isn’t automatically made an instance of the type classes that the original type belongs to, so we need to derive or manually write it.

- In practice, you can think of `newtype` declarations as data declarations that can have only one constructor and one field. If you catch yourself writing such a data declaration, consider using `newtype`. The `data` keyword is for making your own data types. You can go hog wild with them. They can have as many constructors and fields as you wish and can be used to implement any algebraic data type — everything from lists and `Maybe`-like types to trees. In summary, use the keywords as follows:

• If you just want your type signatures to look cleaner and be more descriptive, you probably want type synonyms.

• If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you’re looking for a newtype.

• If you want to make something completely new, odds are good that you’re looking for the data keyword.

## About those Monoids

```haskell
ghci> 4 * 1
4

ghci> 1 * 9
9

ghci> [1,2,3] ++ []
[1,2,3]

ghci> [] ++ [0.5, 2.5]
[0.5,2.5]
```

It seems that `*` together with `1` and `++` along with `[]` share some common properties:

• The function takes two parameters.
• The parameters and the returned value have the same type.
• There exists such a value that doesn’t change other values when used with the binary function.

When we have three or more values and we want to use the binary function to reduce them to a single result, the order in which we apply the binary function to the values doesn’t matter.

```haskell
ghci> (3 * 2) * (8 * 5)
240

ghci> 3 * (2 * (8 * 5))
240

ghci> "la" ++ ("di" ++ "da")
"ladida"

ghci> ("la" ++ "di") ++ "da"
"ladida"
```

## The `Monoid` type class

- A monoid is made up of an associative binary function and a value that acts as an identity with respect to that function.
- `1` is the identity with respect to `*` , and `[]` is the identity with respect to `++`
- Defined in `Data.Monoid`,

```haskell
class Monoid m where
mempty :: m
mappend :: m -> m -> m
mconcat :: [m] -> m
mconcat = foldr mappend mempty
```

- Only concrete types can be made instances of `Monoid`, because the `m` in the type class definition doesn’t take any type parameters. This is different from `Functor` and `Applicative`, which require their instances to be type constructors that take one parameter.

- `mempty` represents the identity value for a particular monoid.
- `mappend`, which, as you’ve probably guessed, is the binary function. It takes two values of the same type and returns another value of that same type

- Think in terms of `mappend` being a binary function that takes two monoid values and returns a third. The last function in this type class definition is `mconcat`. It takes a list of monoid values and reduces them to a single value by using `mappend` between the list’s elements. It has a default implementation, which just takes `mempty` as a starting value and folds the list from the right with `mappend`.

## `Monoid` laws

• `mempty mappend x = x`

• `x mappend mempty = x`

• `(x mappend y) mappend z = x mappend (y mappend z)`

- The first two laws state that `mempty` must act as the identity with respect to `mappend`, and the third says that `mappend` must be `associative` (the order in which we use `mappend` to reduce several monoid values into one doesn’t matter).

## Meet some Monoids

### Lists

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

- Lists are an instance of the `Monoid` type class, regardless of the type of the elements they hold. Notice that we wrote `instance Monoid [a]` and not `instance Monoid []`, because `Monoid` requires a concrete type for an instance.

```haskell
ghci> [1,2,3] `mappend` [4,5,6]
[1,2,3,4,5,6]

ghci> ("one" `mappend` "two") `mappend` "tree"
"onetwotree"

ghci> "one" `mappend` ("two" `mappend` "tree")
"onetwotree"

ghci> "one" `mappend` "two" `mappend` "tree"
"onetwotree"

ghci> "pang" `mappend` mempty
"pang"

ghci> mconcat [[1,2],[3,6],[9]]
[1,2,3,6,9]

ghci> mempty :: [a]
[]
No
```

- Because `mconcat` has a default implementation, we get it for free when we make something an instance of `Monoid`. In the case of the list,
  `mconcat` turns out to be just `concat`. It takes a list of lists and flattens it, because that’s the equivalent of doing `++` between all the adjacent lists in a list.

- Notice that monoids don’t require that `a mappend b` be equal to `b mappend a`. In the case of the list, they clearly aren’t:

```haskell
ghci> "one" `mappend` "two"
"onetwo"

ghci> "two" `mappend` "one"
"twoone"
```

### `Product` and `Sum`

- `Product`: Binary function be `*` - identity value be `1`
- `Sum`: Binary function `+` - identity value `0`

- With two equally valid ways for numbers to be monoids, which way do we choose? Well, we don’t have to pick. Remember that when there are several ways for some type to be an instance of the same type class, we can wrap that type in a `newtype` and then make the new type an instance of the type class in a different way.

```haskell
newtype Product a = Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)
```

- `mempty` is just `1` wrapped in a `Product` constructor. `mappend` pattern matches on the `Product` constructor, multiplies the two numbers, and then wraps the resulting number.

```haskell
ghci> getProduct $ Product 3 `mappend` Product 9
27

ghci> getProduct $ Product 3 `mappend` mempty
3

ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
24

ghci> getProduct . mconcat . map Product $ [3,4,2]
24
```

```haskell
ghci> getSum $ Sum 2 `mappend` Sum 9
11

ghci> getSum $ mempty `mappend` Sum 3
3

ghci> getSum . mconcat . map Sum $ [1,2,3]
6
```

### `Any` and `All`

- Another type that can act like a monoid in two distinct but equally valid ways
  is `Bool`. The first way is to have the function `||` , which represents a logical
  `OR`, act as the binary function along with `False` as the identity value. With
  the logical `OR`, if any of the two parameters is `True`

```haskell
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
```

Its instance looks like this:

```haskell
instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)
```

```haskell
ghci> getAny $ Any True `mappend` Any False
True

ghci> getAny $ mempty `mappend` Any True
True

ghci> getAny . mconcat . map Any $ [False, False, False, True]
True

ghci> getAny $ mempty `mappend` mempty
False
```

- The other way for `Bool` to be an instance of `Monoid` is to kind of do the
  opposite: Have `&&` be the binary function and then make `True` the identity
  value. Logical `AND` will return `True` only if both of its parameters are `True`,

This is the newtype declaration:

```haskell
newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
```

And this is the instance:

```haskell
instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)
```

```haskell
ghci> getAll $ mempty `mappend` All True
True

ghci> getAll $ mempty `mappend` All False
False

ghci> getAll . mconcat . map All $ [True, True, True]
True

ghci> getAll . mconcat . map All $ [True, True, False]
False
```

### The `Ordering``type

```haskell
ghci> 1 `compare` 2
LT
ghci> 2 `compare` 2
EQ
ghci> 3 `compare` 2
GT
```

```haskell
instance Monoid Ordering where
mempty = EQ
LT `mappend` _ = LT
EQ `mappend` y = y
GT `mappend` _ = GT
```

- It’s important to note that in the `Monoid` instance for `Ordering` , `x mappend y` doesn’t equal `y mappend x` . Because the first parameter is kept unless it’s `EQ`, `LT mappend GT` will result in`LT`, whereas `GT mappend LT` will result in `GT`:

```haskell
ghci> LT `mappend` GT
LT

ghci> GT `mappend` LT
GT

ghci> mempty `mappend` LT
LT

ghci> mempty `mappend` GT
GT
```

```haskell
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                  in if a == EQ then b else a
```

But by employing our understanding of how `Ordering` is a monoid, we can rewrite this function in a much simpler manner:

```haskell
import Data.Monoid
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
```

```haskell
ghci> lengthCompare "zen" "ants"
LT
ghci> lengthCompare "zen" "ant"
GT
```

Now suppose that we want to expand this function to also compare for the number of vowels and set this to be the second most important criterion for comparison

```haskell
import Data.Monoid
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")
```

```haskell
ghci> lengthCompare "zen" "anna"
LT

ghci> lengthCompare "zen" "ana"
LT

ghci> lengthCompare "zen" "ann"
GT
```

### `Maybe` the `Monoid`

- One way is to treat `Maybe` a as a monoid only if its type parameter a is a monoid as well and then implement mappend in such a way that it uses the mappend operation of the values that are wrapped with `Just`.

```haskell
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

- Notice the class constraint. It says that `Maybe` a is an instance of `Monoid` only if a is an instance of `Monoid`. If we mappend something with a `Nothing`, the result is that something. If we mappend two `Just` values, the contents of the Justs are mappended and then wrapped back in a `Just`.

```haskell
ghci> Nothing `mappend` Just "andy"
Just "andy"

ghci> Just LT `mappend` Nothing
Just LT

ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})
```

- But what if the type of the contents of the `Maybe` is not an instance of `Monoid`? When we don’t know if the contents are monoids, we can’t use `mappend` between them, so what are we to do? Well, one thing we can do is discard the second value and keep the first one.

```haskell
newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Read, Show)
```

```haskell
instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x
```

```haskell
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'

ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'

ghci> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'
```

- First is useful when we have a bunch of Maybe values and we just want to
  know if any of them is a Just . The mconcat function comes in handy:

```haskell
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9
```

- If we want a monoid on `Maybe` a such that the second parameter is kept if both parameters of `mappend` are `Just` values, `Data.Monoid` provides the `Last` a type, which works like `First a` , but the last `non-Nothing` value is kept when mappending and using `mconcat` :

```haskell
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
Just 10

ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")
Just "two"
```

## Folding with `Monoids`

- `Foldable` is for things that can be folded up
- Defined in `Data.Foldable` - Use `import qualified Data.Foldable as F`

```haskell
ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
ghci> :t F.foldr
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
```

- `foldr` takes a list and folds it up, the `foldr` from `Data.Foldable` accepts any type that can be folded up

```haskell
ghci> foldr (*) 1 [1,2,3]
6
ghci> F.foldr (*) 1 [1,2,3]
6
```

```haskell
ghci> F.foldl (+) 2 (Just 9)
11
ghci> F.foldr (||) False (Just True)
True
```

```haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
```

- One way to make a type constructor an instance of `Foldable` is to just directly implement `foldr` for it. But another, often much easier way, is to implement the `foldMap` function, which is also a part of the `Foldable` type class. The `foldMap` function has the following type:

```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

- Its first parameter is a function that takes a value of the type that our foldable structure contains (denoted here with `a` ) and returns a monoid value. Its second parameter is a foldable structure that contains values of type `a`. It maps that function over the foldable structure, thus producing a foldable structure that contains monoid values. Then, by doing mappend between those monoid values, it joins them all into a single monoid value.

```haskell
instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x `mappend`
                           F.foldMap f r
```

```haskell
testTree =
  Node 5
    (Node 3
      (Node 1 EmptyTree EmptyTree)
      (Node 6 EmptyTree EmptyTree)
    )
    (Node 9
      (Node 8 EmptyTree EmptyTree)
      (Node 10 EmptyTree EmptyTree)
    )
```

```haskell
ghci> F.foldl (+) 0 testTree
42
i
ghci> F.foldl (*) 1 testTree
64800
```

- `foldMap` isn’t useful only for making new instances of `Foldable` . It also comes in handy for reducing our structure to a single monoid value. For instance, if we want to know if any number in our tree is equal to `3` , we can do this:

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
True
```

- Here, `\x -> Any $ x == 3` is a function that takes a number and returns a monoid value: a `Bool` wrapped in `Any`.

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree
False
```

- The mappend action that takes place between all those singleton lists results in a single list that holds all of the elements that are in our tree:

```haskell
ghci> F.foldMap (\x -> [x]) testTree
[1,3,6,5,8,9,10]
```

What’s cool is that all of these tricks aren’t limited to trees. They work on any instance of Foldable
