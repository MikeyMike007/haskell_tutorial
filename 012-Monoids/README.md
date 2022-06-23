# Monoids

The type class `Monoid` is for types whose values can be combined toether with a binary operation.

## Wrapping existing types into a new type

You can make new types out of existing data types with the keyword `newtype`

Key question, how can we make lists an instance of `Applicative` so that the first function on the left side of `<*>` and apply it to the first value of the right, then take the second function from the list and apply on the second value on the right. Almost like zipping list together.

`ZipList` is made an instance of `Applicative` , so that when we want to use lists as applicatives in the zipping manner, we just wrap them with the `ZipList` constructor. The we can unwrap them with `getZipList`.

```haskell
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3] $
[2,200,15]
```

One way of declare the `ZipList` type,

```haskell
data ZipList a = ZipList [a]
```

This is a type that has just one value constructor, and that value constructor has just one field that is a list of things. We might also want to use record syntax so that we automatically get a function that extracts a list from a `ZipList`:

```haskell
data ZipList a = ZipList { getZipList :: [a] }
```

We had two ways of making an existing type an instance of a type class, so we used the data keyword to just wrap that type into another type and made the other type an instance in the second way. The newtype keyword in Haskell is made exactly for cases when we want to just take one type and wrap it in something to present it as another type. In the actual libraries, ZipList a is defined like this:

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }
```

Instead of the data keyword, the newtype keyword is used. Now why is that? Well for one, newtype is faster. If you use the data keyword to wrap a type, there’s some overhead to all that wrapping and unwrapping when your program is running. But if you use newtype , Haskell knows that you’re just using it to wrap an existing type into a new type (hence the name), because you want it to be the same internally but have a different type. With that in mind, Haskell can get rid of the wrapping and unwrapping once it resolves which value is of which type.

So why not just use newtype instead of data all the time? When you make a new type from an existing type by using the newtype keyword, you can have only one value constructor, and that value constructor can have only one field. But with data, you can make data types that have several value constructors, and each constructor can have zero or more fields:

```haskell
data Profession = Fighter | Archer | Accountant
data Race = Human | Elf | Orc | Goblin
data PlayerCharacter = PlayerCharacter Race Profession
```

We can also use the deriving keyword with `newtype` just as we would with data. We can derive instances for `Eq` , `Ord` , `Enum` , `Bounded` , `Show` , and `Read` . If we derive the instance for a type class, the type that we’re wrapping must already be in that type class. It makes sense, because `newtype` just wraps an existing type. So now if we do the following, we can print and equate values of our new type:

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

It takes a `[Char]` value, such as "my sharona" and returns a `CharList` value. From the preceding examples where we used the `CharList` value constructor, we see that really is the case. Conversely, the `getCharList` function, which was generated for us because we used record syntax in our `newtype` , has this type:

```haskell
getCharList :: CharList -> [Char]
```

It takes a `CharList` value and converts it to a `[Char]` value. You can think of this as wrapping and unwrapping, but you can also think of it as converting values from one type to the other.

## Using `newtype` to make type class `instances`

Many times, we want to make our types instances of certain type classes, but the type parameters just don’t match up for what we want to do. It’s easy to make `Maybe` an instance of `Functor` , because the `Functor` type class is defined like this:

```haskell
class Functor f where
fmap :: (a -> b) -> f a -> f
```

```haskell
instance Functor Maybe where
```

Then we implement `fmap` . All the type parameters add up because Maybe takes the place of `f` in the definition of the `Functor` type class. Looking at `fmap` as if it worked on only `Maybe`, it ends up behaving like this:

```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b
```

Now what if we wanted to make the tuple an instance of `Functor` in such a way that when we `fmap` a function over a tuple, it is applied to the first component of the tuple? That way, doing `fmap (+3) (1, 1)` would result in `(4, 1)`. It turns out that writing the instance for that is kind of hard. With `Maybe` , we just say instance `Functor Maybe` where because only type constructors that take exactly one parameter can be made an instance of `Functor`. But it seems like there’s no way to do something like that with `(a, b)` so that the type parameter a ends up being the one that changes when we use fmap. To get around this, we can `newtype` our tuple in such a way that the second type parameter represents the type of the first component in the tuple:

```haskell
newtype Pair b a = Pair { getPair :: (a, b) }
```

And now we can make it an instance of `Functor` so that the function is mapped over the first component:

```haskell
instance Functor (Pair c) where
fmap f (Pair (x, y)) = Pair (f x, y)
```

As you can see, we can pattern match on types defined with `newtype` . We pattern match to get the underlying tuple, apply the function `f` to the first component in the tuple, and then use the Pair value constructor to convert the tuple back to our `Pair b a` . If we imagine what the type fmap would be if it worked only on our new pairs, it would look like this:

```haskell
fmap :: (a -> b) -> Pair c a -> Pair c b
```

Again, we said instance `Functor (Pair c) where` , and so `Pair c` took the place of the `f` in the type class definition for `Functor`:

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
