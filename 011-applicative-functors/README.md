# Applicative Functors

## Functors

- Functors are things that can be mapped over (lists, `Maybe`, trees)
- They are described by the type class `Functor`
- Type class `Functor` has only one type class method `fmap` with definition:

```haskell
fmap :: (a -> b) -> f a -> f b
```

- Definition can be translated to something line: **"Give me a function that takes an `a` and returns an `b` and a box that holds an `a` inside it and I'll give you a box with a `b` inside it"**

- Functor values has added an added contecxt to them i.e. `Maybe` values has the contect that they may have failed i.e. `Nothing`.

- `fmap` performs a function to a value with a context while it preserves the context

- `Maybe` can be made an instance of `Functor` because it takes one type parameter to produce a concrete type, like `Maybe Int` or `Maybe String`.

- We have a problem with `Either` since that takes two parameters to produce a concrete type. To solve for this, we need to partially apply the type conmstructor until it takes only one type parameter i.e. we cannot write `instance Functor Either where` but we can write `instance Functor (Either a) where`. `fmap` is then only for `Either a` and will have following definition,

```haskell
fmap :: (b -> c) -> Either a b -> Either a c
```

## I/O Actions as Functors

Lets see how `IO` is an instance of `Functor`

```haskell
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)
```

`return` is a function that makes an I/O action that doesnt do anything but ony yields something as its result

Two following code blocks performs exactly the same thing,

```haskell
main = do line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
```

```haskell
main = do line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
```

In the same way that we can apply a function to something that’s inside a Maybe box, we can apply a function to what’s inside an IO box,

The I/O action `fmap (++"!") getLine` behaves just like `lgetLine` , except that its result always has `"!"` appended to it!

Some more examples,

With lambda function,

```haskell
import Data.Char ( toUpper )
import Data.List ( intersperse )

main = do
  line <- fmap (\xs -> intersperse '-' (reverse (map toUpper xs))) getLine
  putStrLn line
```

Same example without lambda function,

```haskell
import Data.Char ( toUpper )
import Data.List ( intersperse )

main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line
```

## Functions as Functors

- Another instance of functor is `(->) r`
- `r -> a` can be rewrittwen as `-> r a` - i.e. its actually a type constructor that takes two parameters, like `Either`
- A type constructor must take exactly one type parameter in order to be able to be made an instance of `Functor`
- We cannot make `(->)` an instance of `Functor` but we can partially apply it to `(->) r`.
- Implementation exists in `Control.Monad.Instances`,

```haskell
instance Functor ((->) r) where
fmap f g = (\x -> f (g x))
```

```haskell
fmap :: (a -> b) -> f a -> f b
```

Now, lets replace every `f` with `(->) r`.

```haskell
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
```

Rearrange,

```haskell
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

- `fmap` takes a function from `a` to `b` and a function from `r` to `a` and returns a function from `r` to `b`. This can be translated into that we pipe the output of `r -> a` into the input of `a -> b` to get a function `r -> b`. This is basically function composition!

- We find the definition in `Control.Monad.Instances`,

```haskell
instance Functor ((->) r) where
fmap = (.)
```

- Some examples,

```haskell
ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a

ghci> fmap (*3) (+100) 1
303

ghci> (*3) `fmap` (+100) $ 1
303

ghci> (*3) . (+100) $ 1
303

ghci> fmap (show . (*3)) (+100) 1
"303"
```

- Note when `fmap` is called as an infix function so that the resemblance to `.` is clear.

- Just like all functors, functions can be thought of as values with contexts. When we have a function like `(+3)` , we can view the value as the eventual result of the function, and the context is that we need to apply the function to something to get to the result. Using `fmap (*3) on (+100)` will create another function that acts like `(+100)` , but before producing a result, `(*3)` will be applied to that result.

- Type of `fmap`,

```haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

Can be rewritten as,

```haskell
fmap :: (Functor f) => (a -> b) -> (f a -> f b)
```

- This means we can think of `fmap` not as a function that takes one function and a functor value and returns a functor value, but as a function that takes a function and returns a new function that’s just like the old one, except that it takes a functor value as a parameter and returns a functor value as the result. It takes an `a -> b` function and returns a function `f a -> f b` . This is called lifting a function.

Example,

```haskell
ghci> :t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a

ghci> :t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a
```

The expression `fmap (*2)` is a function that takes a functor `f` over numbers and returns a functor over numbers. That functor can be a list, a `Maybe` , an `Either String`, or anything else. The expression `fmap (replicate 3)` will take a functor over any type and return a functor over a list of elements of that type.

The type `fmap (replicate 3) :: (Functor f) => f a -> f [a]` means that the function will work on any functor. What it will do depends on the functor. If we use `fmap (replicate 3)` on a list, the list’s implementation for `fmap` will be chosen, which is just `map`. If we use it on `Maybe a` , it will apply `replicate 3` to the value inside the `Just` . If it’s `Nothing` , it stays `Nothing` .

```haskell
ghci> fmap (replicate 3) [1,2,3,4]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]

ghci> fmap (replicate 3) (Just 4)
Just [4,4,4]

ghci> fmap (replicate 3) (Right "blah")
Right ["blah","blah","blah"]

ghci> fmap (replicate 3) Nothing
Nothing

ghci> fmap (replicate 3) (Left "foo")
Left "foo"
```

## Functor laws

### Law 1

- If we map the `ìd` function over a functor value, the functor value we get back should be the same as the original functor value i.e. if we do `fmap id` over a functor value, it should be the same as applying `id` to the value.

- id is basically `(\x -> x)`

Some examples,

```haskell
ghci> fmap id (Just 3)
Just 3

ghci> id (Just 3)
Just 3

ghci> fmap id [1..5]
[1,2,3,4,5]

ghci> id [1..5]
[1,2,3,4,5]

ghci> fmap id []
[]

ghci> fmap id Nothing
Nothing
```

Implementation of `fmap` for `Maybe`,

```haskell
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
```

`fmap id` over `Just x` will yield the result `Just (id x)`

### Law 2

Composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one.

```haskell
fmap (f . g) = fmap f . fmap g
```

```haskell
fmap (f . g) x = fmap f (fmap g x)
```

## Breaking the law

- Example of a type constructor being an instance of the `Functor` type class but not really being a functor.

```haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)
```

The `C` stands for counter.

```haskell
ghci> CNothing
CNothing

ghci> CJust 0 "haha"
CJust 0 "haha"

ghci> :t CNothing
CNothing :: CMaybe a

ghci> :t CJust 0 "haha"
CJust 0 "haha" :: CMaybe [Char]

ghci> CJust 100 [1,2,3]
CJust 100 [1,2,3]
```

```haskell
instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)
```

Examples,

```haskell
ghci> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"

ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"

ghci> fmap (++"blah") CNothing
CNothing
```

```haskell
ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"

ghci> id (CJust 0 "haha")
CJust 0 "haha
```

If we map `id` over a functor value, it should be the same as just calling id with the same functor value. Our example demonstrates that this is not true for our CMaybe functor.

## Using Applicative Functors

- What happends when we map a function that takes two parameters over a functor?

- `fmap (*) (Just 3)` results in `Just ((*) 3)` which can be rewritten as `Just (3 *)` if we use sections. This means we get a function wrapped in a `Just`

```haskell
ghci> :t fmap (++) (Just "hey")

fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
ghci> :t fmap compare (Just 'a')

fmap compare (Just 'a') :: Maybe (Char -> Ordering)
ghci> :t fmap compare "A LIST OF CHARS"

fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]

fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
```

If we map `compare`, which has a type of `(Ord a) => a -> a -> Ordering` , over a list of characters, we get a list of functions of type `Char -> Ordering` , because the function compare gets partially applied with the characters in the list.

We see how by mapping “multiparameter” functions over functor val- ues, we get functor values that contain functions inside them. So now what can we do with them? For one, we can map functions that take these functions as parameters over them, because whatever is inside a functor value will be given to the function that we’re mapping over it as a parameter,

```haskell
ghci> let a = fmap (*) [1,2,3,4]

ghci> :t a
a :: [Integer -> Integer]

ghci> fmap (\f -> f 9) a
[9,18,27,36]
```

But what if we have a functor value of `Just (3 *)` and a functor value of `Just 5`, and we want to take out the function from `Just (3 *)` and map it over `Just 5`? With normal functors, we’re out of luck, because they support only mapping normal functions over existing functors. Even when we mapped `\f -> f 9` over a functor that contained functions, we were just mapping a normal function over it. But we can’t map a function that’s inside a functor value over another functor value with what `fmap` offers us. Here is where the type class `Applicative` comes in to help.

## Applicative type class

- Defined in `Control.Appicative`
- Defines two functions `pure` and `<*>` for which it doesnt provide any default implementation

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

- Constraint says that if we want to make a type constructor part of the `Applicative` type class, it must be in `Functor` first.

- `pure` should take a value of any type and return an applicative value with that value inside it.

- The `a -> f a` type declaration for `pure` is still pretty descriptive. We take a value and we wrap it in an applicative value that has that value as the result inside it. In other words, it takes a value and puts it in some sort of default (or pure) context—a minimal context that still yields that value.

- `(<*>) :: f (a -> b) -> f a -> f b` reminds of `fmap :: (a -> b) -> f a -> f b`

- Whereas `fmap` takes a function and a functor value and applies the function inside the functor value, `<*>` takes a functor value that has a function in it and another functor, and extracts that function from the first functor and then maps it over the second one.

## `Maybe` the `Applicative` Functor

```haskell
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
```

So for `Maybe` , `<*>` extracts the function from the left value if it’s a `Just` and maps it over the right value. If any of the parameters is `Nothing`, `Nothing` is the result.

```haskell
ghci> Just (+3) <*> Just 9
Just 12

ghci> pure (+3) <*> Just 10
Just 13

ghci> pure (+3) <*> Just 9
Just 12

ghci> Just (++"hahah") <*> Nothing
Nothing

ghci> Nothing <*> Just "woot"
Nothing
```

## The `Applicative` Style

Chaining,

```haskell
ghci> pure (+) <*> Just 3 <*> Just 5
Just 8

ghci> pure (+) <*> Just 3 <*> Nothing
Nothing

ghci> pure (+) <*> Nothing <*> Just 5
Nothing
```

Step by step,

```haskell
pure (+) <*> Just 3 <*> Just 5
(pure (+) <*> Just 3) <*> Just 5
```

So we have `pure (+)` , which is `Just (+)` . Next, `Just (+) <*> Just 3` happens. The result of this is `Just (3+)` . This is because of partial application. Only applying the `+` function to `3` results in a function that takes one parameter and adds `3` to it. Finally, `Just (3+) <*> Just 5` is carried out, which results in a `Just 8`

- `pure f <*> x` equals `fmap f x` .

- `pure` puts a value in a default context. If we just put a function in a default context and then extract and apply it to a value inside another applicative functor, that’s the same as just mapping that function over that applicative functor. Instead of writing `pure f <*> x <*> y <*> ...`, we can write `fmap f x <*> y <*> ...` . This is why `Control.Applicative` exports a function called `<$>` , which is just fmap as an infix operator. Here’s how it’s defined:

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

- By using `<$>`, the applicative style really shines, because now if we want to apply a function `f` between three applicative values, we can write `f <$> x <*> y <*> z`.

- Compare following examples,

```haskell
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
```

```haskell
ghci> (++) "johntra" "volta"
"johntravolta"
```

## Lists

- Lists (actually the list type constructor, [] ) are applicative functors.

```haskell
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
```

```haskell
ghci> pure "Hey" :: [String]
["Hey"]

ghci> pure "Hey" :: Maybe String
Just "Hey"
```

```haskell
ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9
```

- Every function in the left list is applied to every function in the right one. I

```haskell
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
```

- `<*>` is left-associative, so `[(+),(*)] <*> [1,2]` happens first, resulting in a list that’s the same as `[(1+),(2+),(1*),(2*)]`, because every function on the left gets applied to every value on the right. Then `[(1+),(2+),(1*),(2*)] <*> [3,4]` happens, which produces the final result.

```haskell
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
```

- Compare following examples,

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

```haskell
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
```

- If we wanted all possible products of those two lists that are more than 50, we would use the following:

```haskell
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
```

## `IO` as an `Applicative` Functor

- Another instance of `Applicative` that we’ve already encountered is `IO`,

```haskell
instance Applicative IO where
  pure = return
  a <*> b = do
  f <- a
  x <- b
  return (f x)
```

- `pure` is just `return`. `return` makes an I/O action that doesn’t do anything. It just yields some value as its result, without performing any I/O operations like printing to the terminal or reading from a file

Compare following code blocks which does exactly the same,

```haskell
myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b
```

```haskell
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

Another example,

```haskell
main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines concatenated turn out to be: " ++ a
```

## Functions as applicatives

The function instance is implemented in the following way,

```haskell
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)
```

```haskell
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a

ghci> (+) <$> (+3) <*> (*100) $ 5
508
```

- When we do `(+) <$> (+3) <*> (*100)` , we’re making a function that will use `+` on the results of `(+3)` and `(*100)` and return that. With `(+) <$> (+3) <*> (*100) $ 5 , (+3)` and `(*100)` are first applied to `5`, resulting in `8` and `500`. Then `+` is called with `8` and `500` , resulting in `508`.

Example,

```haskell
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
```

- We create a function that will call the function `\x y z -> [x,y,z]` with the eventual results from `(+3)`, `(*2)` and `(/2)` . The `5` is fed to each of the three functions, and then `\x y z -> [x, y, z]` is called with those results.

## Zip Lists

- If we write `[(+3),(*2)] <*> [1,2] , (+3)` will be applied to both `1` and `2`, and `(*2)` will also be applied to both `1` and `2` , resulting in a list that has four elements: `[4,5,2,4]` . However, `[(+3),(*2)] <*> [1,2]` could also work in such a way that the first function in the left list is applied to the first value in the right one, the second function is applied to the second value, and so on. That would result in a list with two values: `[4,4]`. You could look at it as `[1 + 3, 2 * 2]`

```haskell
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

- `<*>` applies the first function to the first value, the second function to the second value, and so on. This is done with `zipWith (\f x -> f x) fs xs`. Because of how `zipWith` works, the resulting list will be as long as the shorter of the two lists.

- So how do zip lists work in an applicative style? Well, the ZipList a type doesn’t have a Show instance, so we need to use the getZipList function to extract a raw list from a zip list:

```haskell
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]

ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]

ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]

ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
```

- The `(,,)` function is the same as `\x y z -> (x,y,z)` . Also, the `(,)` function is the same as `\x y -> (x,y)`.

## Applicative laws

The most important law is the one that `pure f <*> x = fmap f x` holds.

## Useful functions for applicatives

- Defined in `Control.Applicatives`

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
```

Definition,

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```

```haskell
ghci> fmap (\x -> [x]) (Just 4)
Just [4]
```

```haskell
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]

ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
```

Let’s try implementing a function that takes a list of applicative values and returns an applicative value that has a list as its result value. We’ll call it `sequenceA`.

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
  sequenceA [] = pure []
  sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

Following codes are the same,

```haskell
sequenceA [Just 1, Just 2]
(:) <$> Just 1 <*> sequenceA [Just 2]
(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
-- sequenceA [] = Just []
(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])
(:) <$> Just 1 <*> Just [2]
Just [1, 2]
```

Imlement `sequenceA` with a `fold`

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
```

- We approach the list from the right and start off with an accumulator value of `pure []`.

- Then we call `liftA2 (:)` with the now last element and the current accumulator and so on, until we’re left with just the accumulator, which holds a list of the results of all the applicatives.

```haskell
ghci> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]

ghci> sequenceA [Just 3, Nothing, Just 1]
Nothing

ghci> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]

ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]
```

- Doing `(+) <$> (+3) <*> (*2)` will create a function that takes a parameter, feeds it to both `(+3)` and `(*2)` , and then calls `+` with those two results. In the same vein, it makes sense that `sequenceA [(+3),(*2)]` makes a function that takes a parameter and feeds it to all of the functions in the list. Instead of calling `+` with the results of the functions, a combination of `:` and `pure []` is used to gather those results in a list, which is the result of that function.

- Using `sequenceA` is useful when we have a list of functions and we want to feed the same input to all of them and then view the list of results. For instance, suppose that we have a number and we’re wondering whether it satisfies all of the predicates in a list. Here’s one way to do that:

```haskell
ghci> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]

ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
True
```

Another way to achieve the same thing is with `sequenceA`,

```haskell
ghci> sequenceA [(>4),(<10),odd] 7
[True,True,True]

ghci> and $ sequenceA [(>4),(<10),odd] 7
True
```

- `sequenceA [(>4),(<10),odd]` creates a function that will take a number and feed it to all of the predicates in `[(>4),(<10),odd]` and return a list of `Booleans`

- When used with `[]`, `sequenceA` takes a list of lists and returns a list of lists. It actually creates lists that have all possible combinations of their elements.

```haskell
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

ghci> sequenceA [[1,2],[3,4]]
[[1,3],[1,4],[2,3],[2,4]]

ghci> [[x,y] | x <- [1,2], y <- [3,4]]
[[1,3],[1,4],[2,3],[2,4]]

ghci> sequenceA [[1,2],[3,4],[5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]

ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
```

- When used with I/O actions, `sequenceA` is the same thing as `sequence`

```haskell
ghci> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
```
