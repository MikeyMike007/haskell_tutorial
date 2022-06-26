# Applicative Functors

## Functors redux

functors are things can be mapped over, like `lists`, `Maybes`, or `trees`. functors are described by the type class `Functor` that has only one type class method `fmap` with type,

```haskell
fmap :: (a -> b) -> f a -> f b
```

This can be seen as "Give me a function that take an `a` and returns a `b` and a box with an `a` inside it and i will give you a box with a `b`"

If we want to make a type constructor an instance of `Functor` , it must have a kind of `* -> *`, which means that it takes exactly one concrete type as a type parameter. This means `Maybe` can be made an instance beacuse it takes one type parameter to produce a concrete type, like `Maybe Int` or `Maybe String`. If a type constructor takes two parameters like `Either`, we need to partially apply the type constructor until it takes only one type parameter.

This gives that we cannot write `instance Functor Either where` but that we can write `instance Functor (Either a) where`.

In the example below, the `Either a` part is fixed, because `Either a` takes only one type parameter.

```haskell
fmap :: (b -> c) -> Either a b -> Either a c
```

### I/O Actions as Functors

type `IO String` means its an I/O action that will go out in the real world and get some string for us. We can use `<-` in a `do` syntax to bind the result to a name.

How is IO an instance of a `Functor`,

```haskell
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)
```

The code above says that we first perform the original I/O action and binds it results to `result`. Then we call `return (f result)`. Note that we use `return` to make an I/O action that doesnt really do anything; it just yields `f result` as the result of the new I/O action.

We following to examples that yields the same result,

Without the use of `fmap`

```haskell
main = do line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
```

With `fmap`,

```haskell
main = do line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
```

Note that `getLine` is a IO action of type `IO String`.

The I/O action `fmap (++"!") getLine` behaves just like `getLine` , except that its result always has `"!"` appended to it!

If `fmap` were limited to `IO` , its type would be `fmap :: (a -> b) -> IO a -> IO b`. `fmap` takes a function and an I/O action and returns a new I/O action that’s like the old one, except that the function is applied to its contained result.

Another example with a lambda function

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

### Functions as `Functors`

The function type `r -> a` can be rewritten as `(->) r a`, much like we can write `2+3` as `(+) 2 3`. So if we see this in the light of `-> r a`, its just a type constructor that can take two parameters.

A type constructor must take exactly one type parameter in order for it to be made an instance of `Functor`. This is the reason why we cant make `(->)` an instance of `Functor`; however, if we partially apply it ti `(->) r`, it doesnt pose any problems.

If the syntax allowed for type constructors to be partially applied with sections, we could write `(->) r` as `(r ->)`. For clarfication, note that we can partially apply `+` by doing `(2+)`, which is the same as `(+) 2`.

See definitions below to understand how functions are `Functors`.

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

The results says that mapping over a function must produce a function, just like mapping over a `Maybe` must produce a `Maybe`, and mapping over a list must produce a list.

The definition above says that we pipe the output of `r -> a` into the input of `a -> b` to get a function `r -> b`, which is exactly what function composition is all about.

Here’s another way to write this instance

```haskell
instance Functor ((->) r) where
fmap = (.)
```

This makes it clear thjat using `fmap` over functions is just function composition.

The instances are defined in `Control.Monad.Instances`.

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

When we have a function like (+3) , we can view the value as the eventual result of the function, and the context is that we need to apply the function to something to get to the result.

Using `fmap (*3) on (+100)` will create another function that acts like `(+100)`, but before producing a result, `(*3)` will be applied to that result.

### Lifting a function

Type of `fmap`,

```haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

- All Haskell functions takes only one parameter
- A function `a -> b -> c` actually takes only one parameter and returns a function `b -> c`
- Calling a function with too few parameters gives us back a function that takes the number of parameters which we left out i.e. `a -> b -> c` can be written as `a -> (b -> c)`.

With this in mind, we can rewrite,

```haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

As,

```haskell
fmap :: (Functor f) => (a -> b) -> (f a -> f b)
```

This says that we can think of `fmap` not as a function that takes one function and a functor value and returns a functor value, but as a function that takes a function and returns a new function that’s just like the old one, except that it takes a functor value as a parameter and returns a functor value as the result. It takes an `a -> b` function and returns a function `f a -> f b`. This is what is called **lifting a function**. See example below,

```haskell
ghci> :t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a

ghci> :t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a
```

The expression `fmap (\*2)` is a function that takes a functor `f` over numbers and returns a functor over numbers.

The expression `fmap (replicate 3)` will take a functor over any type and return a functor over a list of elements of that type.

You can think of `fmap` in two ways:

- As a function that takes a function and a functor value and then maps that function over the functor value
- As a function that takes a function and lifts that function so it operates on functor values

Some more examples,

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

## Functors laws

Calling fmap on a functor should just map a function over the functor—nothing
more. This behavior is described in the functor laws. All instances of Functor
should abide by these two laws.

### Law 1

The first functor law states that if we map the id function over a functor
value, the functor value that we get back should be the same as the original
functor value.

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

Looking at the implementation of `fmap` for `Maybe`, for example, we can figure out why the first functor law holds:

```haskell
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
```

### Law 2

The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one. Formally written, that means `fmap (f . g) = fmap f . fmap g`. Or to write it in another way, for any functor value x, the following should hold: `fmap (f . g) x = fmap f (fmap g x)`

### Example that brakes the law

```haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)
```

The `C` here stands for counter. It’s a data type that looks much like `Maybe a`, but the `Just` part holds two fields instead of one. The first field in the `CJust` value constructor will always have a type of `Int` , and it will be some sort of counter. The second field is of type `a` , which comes from the type parameter, and its type will depend on the concrete type that we choose for `CMaybe a`.

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

Let’s make this an instance of `Functor` so that each time we use `fmap` , the function is applied to the second field, whereas the first field is increased by `1`.

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

## Using Applicative Functors

- Applicative Functors: Mapping functions with more than one parameter over a functor
- `fmap (*) (Just 3) = Just ((*) 3) = Just (3 *)` which means that the result in a function wrapped in a `Just`

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

We see how by mapping “multiparameter” functions over functor values, we get functor values that contain functions inside them. So now what can we do with them? For one, we can map functions that take these functions as parameters over them, because whatever is inside a functor value will be given to the function that we’re mapping over it as a parameter:

```haskell
ghci> let a = fmap (*) [1,2,3,4]

ghci> :t a
a :: [Integer -> Integer]

ghci> fmap (\f -> f 9) a
[9,18,27,36]
```

But what if we have a functor value of `Just (3 *)` and a functor value of `Just 5`, and we want to take out the function from `Just (3 *)` and map it over `Just 5`? For this, we can use the `Applicative` type class.

## Applicative type class

- Defined in `Control.Applicative`
- Defines two functions `pure` and `<*>` but does not provide a default implementation

Class definition,

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

```haskell
The first line starts the definition of the Applicative class, and it also introduces a class constraint. The constraint says that if we want to make a type constructor part of the `Applicative` type class, it must be in `Functor` first. That’s why if we know that a type constructor is part of the `Applicative` type class, it’s also in `Functor` , so we can use `fmap` on it.
```

`pure` should take a value of any type and return an applicative value with that value inside it. “Inside it” refers to our box analogy again, even though we’ve seen that it doesn’t always stand up to scrutiny. But the `a -> f a` type declaration is still pretty descriptive. We take a value and we wrap it in an applicative value that has that value as the result inside it. A better way of thinking about `pure` would be to say that it takes a value and puts it in some sort of default (or pure) context—a minimal context that still yields that
value.

The type declaration of `<*>` is,

```haskell
f (a -> b) -> f a -> f b
```

`<*>` is like `fmap`. Please compare the definitions: `fmap :: (a -> b) -> f a -> f b`. Whereas `fmap` takes a function and a functor value and applies the function inside the functor value, `<*>` takes a functor value that has a function in it and another functor, and extracts that function from the first functor and then maps it
over the second one.

## Maybe the Applicative Functor

Applicative instance implementation for Maybe,

```haskell
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
```

`f` that plays the role of the applicative functor should take one concrete type as a parameter, so we write `instance Applicative Maybe where` instead of `instance Applicative (Maybe a) where`.

Remember that it’s supposed to take something and wrap it in an applicative value. We wrote `pure = Just` , because value constructors like `Just` are normal functions. We could have also written `pure x = Just x`.

`<*>` extracts the function from the left value if it’s a `Just` and maps it over the right value. If any of the parameters is `Nothing` , `Nothing` is the result.

Some examples,

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

You see how doing pure `(+3)` and `Just (+3)` is the same in this case. Use pure if you’re dealing with Maybe values in an applicative context (using them with `<*>`); otherwise, stick to `Just`.

With normal functors, when you map a function over a functor, you can’t get the result out in any general way, even if the result is a partially applied function. Applicative functors, on the other hand, allow you to operate on several functors with a single function.

## The applicative style

With the Applicative type class, we can chain the use of the `<*>` function, thus enabling us to seamlessly operate on several applicative values instead of just one.
fmap :: (a -> b) -> f a -> f b

```haskell
ghci> pure (+) <*> Just 3 <*> Just 5
Just 8

ghci> pure (+) <*> Just 3 <*> Nothing
Nothing

ghci> pure (+) <*> Nothing <*> Just 5
Nothing
```

We wrapped the `+` function inside an applicative value and then used `<*>` to call it with two parameters, both applicative values.

`<*>` is left-associative, which means that this,

This is,

```haskell
pure (+) <*> Just 3 <*> Just 5
(pure (+) <*> Just 3) <*> Just 5
```

First, the `+` function is put in an applicative value—in this case, a `Maybe` value that contains the function. So we have `pure (+)` , which is `Just (+)` . Next, `Just (+) <*> Just 3` happens. The result of this is `Just (3+)` . This is because of partial application. Only applying the `+` function to `3` results in a function that takes one parameter and adds 3 to it. Finally, `Just (3+) <*> Just 5` is carried out, which results in a `Just 8`.

Applicative functors and the applicative style of `pure f <*> x <*> y <*> ...` allow us to take a function that expects parameters that aren’t applicative values and use that function to operate on several applicative values. The function can take as many parameters as we want, because it’s always partially applied step by step between occurrences of `<*>`.

This becomes even more handy and apparent if we consider the fact that pure `f <*> x` equals `fmap f x`.

`pure` puts a value in a default context. If we just put a function in a default context and then extract and apply it to a value inside another applicative functor, that’s the same as just mapping that function over that applicative functor. Instead of writing `pure f <*> x <*> y <*> ...`, we can write `fmap f x <*> y <*> ...` . This is why `Control.Applicative` exports a function called `<$>` , which is just fmap as an infix operator. Here’s how it’s defined:

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

By using `<$>`, the applicative style really shines, because now if we want to apply a function `f` between three applicative values, we can write `f <$> x <*> y <*> z`. If the parameters were normal values rather than applicative functors, we would write `f x y z`.

Some examples,

```haskell
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
```

Compare the code above with following,

```haskell
ghci> (++) "johntra" "volta"
"johntravolta"
```

Back to our `(++) <$> Just "johntra" <*> Just "volta"` : First `(++)` , which has a type of `(++) :: [a] -> [a] -> [a]` , is mapped over `Just "johntra"` . This results in a value that’s the same as `Just ("johntra"++)` and has a type of `Maybe ([Char] -> [Char])`. Notice how the first parameter of `(++)` got eaten up and how the as turned into `Char` values. And now `Just ("johntra"++) <*> Just "volta"` happens, which takes the function out of the `Just` and maps it over `Just "volta"`, resulting in `Just "johntravolta"` . Had either of the two values been `Nothing`, the result would have also been `Nothing`

## Lists (Another instance of `Applicative`)

Lists (actually the list type constructor, `[]` ) are applicative functors. What a surprise! Here’s how `[]` is an instance of `Applicative`,

```haskell
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
```

Remember that `pure` takes a value and puts it in a default context. In other words, it puts it in a minimal context that still yields that value. The minimal context for lists would be the empty list, but the empty list represents the lack of a value, so it can’t hold in itself the value on which we used pure. That’s why `pure` takes a value and puts it in a singleton list. Similarly, the minimal context for the `Maybe` applicative functor would be a `Nothing` , but it represents the lack of a value instead of a value, so `pure` is implemented as `Just` in the instance implementation for `Maybe`.

```haskell
ghci> pure "Hey" :: [String]
["Hey"]

ghci> pure "Hey" :: Maybe String
Just "Hey"
```

What about `<*>`? If the `<*>` function’s type were limited to only lists, we would get `(<*>) :: [a -> b] -> [a] -> [b]` . It’s implemented with a list comprehension. `<*>` must somehow extract the function out of its left parameter and then map it over the right parameter. But the left list can have zero functions, one function, or several functions inside it, and the right list can also hold several values. That’s why we use a list comprehension to draw from both lists. We apply every possible function from the left list to every possible value from the right list. The resulting list has every possible combination of applying a function from the left list to a value in the right one.

Example,

```haskell
ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9
```

The left list has three functions, and the right list has three values, so the resulting list will have nine elements. Every function in the left list is applied to every function in the right one. If we have a list of functions that take two parameters, we can apply those functions between two lists. In the following example, we apply two function between two lists:

```haskell
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
```

`<*>` is left-associative, so `[(+),(*)] <*> [1,2]` happens first, resulting in a list that’s the same as `[(1+),(2+),(1*),(2*)]`, because every function on the left gets applied to every value on the right. Then `[(1+),(2+),(1*),(2*)] <*> [3,4]` happens, which produces the final result.

Example,

```haskell
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
```

Applicative style can be more convenient than list comprehensions. Compare following code blocks that are exactly the same thing,

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

```haskell
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
```

Example of all possible products of those two lists that are more than 50, we would use the following,

```haskell
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
```

It’s easy to see how `pure f <*> xs` equals `fmap f xs` with lists. `pure f` is just `[f]`, and `[f] <*> xs` will apply every function in the left list to every value in the right one, but there’s just one function in the left list, so it’s like mapping.

## IO (As an applicative functor)

Another instance of `Applicative` that we’ve already encountered is `IO`. This is how the instance is implemented,

```haskell
instance Applicative IO where
  pure = return
  a <*> b = do
  f <- a
  x <- b
  return (f x
```

Since `pure` is all about putting a value in a minimal context that still holds the value as the result, it makes sense that `pure` is just `return`.

`return` makes an I/O action that doesn’t do anything. It just yields some value as its result, without performing any I/O operations like printing to the terminal or reading from a file.

If `<*>` were specialized for `IO` , it would have a type of `(<*>) :: IO (a -> b) -> IO a -> IO b`. In the case of `IO` , it takes the `I/O` action `a`, which yields a function, performs the function, and binds that function to `f` . Then it performs b and binds its result to `x`. Finally, it applies the function `f` to `x` and yields that as the result. We used do syntax to implement it here. (Remember that do syntax is about taking several I/O actions and gluing them into one.)

With `Maybe` and `[]` , we could think of `<*>` as simply extracting a function from its left parameter and then applying it over the right one. With `IO` , extracting is still in the game, but now we also have a notion of sequencing, because we’re taking two I/O actions and gluing them into one. We need to extract the function from the first I/O action, but to extract a result from an I/O action, it must be performed. Consider this:

```haskell
myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b
```

Another way to write this is,

```haskell
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

Example,

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

When we wrap a value into an applicative value with `pure`, the result it yields must be that value. A minimal default context still yields that value as a result. That’s why in the function instance implementation, `pure` takes a value and creates a function that ignores its parameter and always returns that value. The type for pure specialized for the `(->) r` instance is `pure :: a -> (r -> a)`.

```haskell
ghci> (pure 3) "blah"
3
```

Because of currying, function application is left-associative, so we can omit the parentheses.

```haskell
ghci> pure 3 "blah"
3
```

The instance implementation for `<*>` is a bit cryptic, so let’s just take a look at how to use functions as applicative functors in the applicative style:

```haskell
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a

ghci> (+) <$> (+3) <*> (*100) $ 5
508
```

Calling `<*>` with two applicative values results in an applicative value, so if we use it on two functions, we get back a function. So what goes on here? When we do `(+) <$> (+3) <*> (*100)` , we’re making a function that will use `+` on the results of `(+3)` and `(*100)` and return that. With `(+) <$> (+3) <*> (*100) $ 5 , (+3)` and `(*100)` are first applied to `5`, resulting in `8` and `500`. Then `+` is called with `8` and `500` , resulting in `508`.

```haskell
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
```

## Zip Lists as applicatives

For example, if we write `[(+3),(*2)] <*> [1,2] , (+3)` will be applied to both `1` and `2`, and `(*2)` will also be applied to both `1` and `2` , resulting in a list that has four elements: `[4,5,2,4]` . However, `[(+3),(*2)] <*> [1,2]` could also work in such a way that the first function in the left list is applied to the first value in the right one, the second function is applied to the second value, and so on. That would result in a list with two values: `[4,4]`. You could look at it as `[1 + 3, 2 * 2]`

An instance of `Applicative` that we haven’t encountered yet is `ZipList` , and it lives in `Control.Applicative` Because one type can’t have two instances for the same type class, the `ZipList` a type was introduced, which has one constructor (`ZipList` ) with just one field (a list). Here’s the instance:

```haskell
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

`<*>` applies the first function to the first value, the second function to the second value, and so on. This is done with `zipWith (\f x -> f x) fs xs`. Because of how `zipWith` works, the resulting list will be as long as the shorter of the two lists.

`pure` is also interesting here. It takes a value and puts it in a list that just has that value repeating indefinitely. `pure "haha"` results in `ZipList (["haha", "haha","haha"....` This might be a bit confusing, since you’ve learned that pure should put a value in a minimal context that still yields that value. And you might be thinking that an infinite list of something is hardly minimal. But it makes sense with zip lists, because it must produce the value on every position. This also satisfies the law that `pure f <*> xs should equal fmap f xs . If pure 3 just returned ZipList [3]` , `pure (*2) <*> ZipList [1,5,10]` would result in `ZipList [2]`, because the resulting list of two zipped lists has the length of the shorter of the two. If we zip a finite list with an infinite list, the length of the resulting list will always be equal to the length of the finite list.

So how do zip lists work in an applicative style? Well, the ZipList a type doesn’t have a Show instance, so we need to use the getZipList function to extract a raw list from a zip list:

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

By using zip lists with an applicative style, we don’t need to have a separate zip function for each number of lists that we want to zip together. We just use the applicative style to zip together an arbitrary amount of lists with a function, and that’s pretty handy.

## Applicative laws

Like normal functors, applicative functors come with a few laws. The most important law is the one that `pure f <\*> x = fmap f x holds`

```haskell
pure id <*> v = v

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

pure f <*> pure x = pure (f x)

u <*> pure y = pure ($ y) <*> u
```

## Useful functions for applicatives

`Control.Applicative` defines a function that’s called `liftA2` , which has the following type:

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
```

Definition,

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```

With ordinary functors, we can just map functions over one functor value. With applicative functors, we can apply a function between several functor values. It’s also interesting to look at this function’s type as `(a -> b -> c) -> (f a -> f b -> f c)` . When we look at it like this, we can say that liftA2 takes a normal binary function and promotes it to a function that operates on two applicatives.

Here’s an interesting concept: We can take two applicative values and combine them into one applicative value that has inside it the results of those two applicative values in a list. For instance, we have `Just 3` and `Just 4` . Let’s assume that the second one contains a singleton list, because that’s really easy to achieve:

```haskell
ghci> fmap (\x -> [x]) (Just 4)
Just [4]
```

Okay, so let’s say we have `Just 3` and `Just [4]` . How do we get `Just [3,4]`? That’s easy:

```haskell
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]

ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
```

Remember that `:` is a function that takes an element and a list and returns a new list with that element at the beginning. Now that we have `Just [3,4]`, could we combine that with `Just 2` to produce `Just [2,3,4]` ? Yes,
we could. It seems that we can combine any amount of applicative values into one applicative value that has a list of the results of those applicative values inside it.

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

You could also implement `sequenceA` with a `fold`.

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
```

We approach the list from the right and start off with an accumulator value of `pure []`. We put `liftA2 (:)` between the accumulator and the last element of the list, which results in an applicative that has a singleton in it. Then we call `liftA2 (:)` with the now last element and the current accumulator and so on, until we’re left with just the accumulator, which holds a list of the results of all the applicatives.

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

When used on `Maybe` values, `sequenceA` creates a `Maybe` value with all the results inside it as a list. If one of the values is `Nothing` , then the result is also a `Nothing`. This is cool when you have a list of `Maybe` values, and you’re interested in the values only if none of them is a `Nothing`.

`sequenceA [(+3),(+2),(+1)] 3` will call `(+3)` with `3`, `(+2)` with `3`, and `(+1)` with `3`, and present all those results as a list.

Doing `(+) <$> (+3) <*> (*2)` will create a function that takes a parameter, feeds it to both `(+3)` and `(*2)` , and then calls `+` with those two results. In the same vein, it makes sense that `sequenceA [(+3),(*2)]` makes a function that takes a parameter and feeds it to all of the functions in the list. Instead of calling `+` with the results of the functions, a combination of `:` and `pure []` is used to gather those results in a list, which is the result of that function.

Using `sequenceA` is useful when we have a list of functions and we want to feed the same input to all of them and then view the list of results. For instance, suppose that we have a number and we’re wondering whether it satisfies all of the predicates in a list. Here’s one way to do that:

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

`sequenceA [(>4),(<10),odd]` creates a function that will take a number and feed it to all of the predicates in `[(>4),(<10),odd]` and return a list of `Booleans`. It turns a list with the type `(Num a) => [a -> Bool]` into a function with the type `(Num a) => a -> [Bool]`.

When used with `[]`, `sequenceA` takes a list of lists and returns a list of lists. It actually creates lists that have all possible combinations of their elements. For illustration, here’s the preceding example done with `sequenceA` and then done with a list comprehension:

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

`(+) <$> [1,2] <*> [4,5,6]` results in a nondeterministic computation `x + y`, where `x` takes on every value from `[1,2]` and `y` takes on every value from `[4,5,6]`. We represent that as a list that holds all of the possible results. Similarly, when we call `sequenceA [[1,2],[3,4],[5,6]]` , the result is a nondeterministic computation `[x,y,z]` , where `x` takes on every value from `[1,2]`, `y` takes on every value from `[3,4]` and so on. To represent the result of that nondeterministic computation, we use a list, where each element in the list is one possible list. That’s why the result is a list of lists.

When used with I/O actions, `sequenceA` is the same thing as sequence ! It takes a list of I/O actions and returns an I/O action that will perform each of those actions and have as its result a list of the results of those I/O actions. That’s because to turn an `[IO a]` value into an `IO [a]` value, to make an I/O action that yields a list of results when performed, all those I/O actions must be sequenced so that they’re then performed one after the otherwhen evaluation is forced. You can’t get the result of an I/O action without
performing it.

```haskell
ghci> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
```
