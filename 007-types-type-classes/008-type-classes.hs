-- # Type classes 102

-- - Section explains how to make your own type classes and how to make type
--   instances of them by hand
--
-- - Type classes are like interfaces i.e. type classes defines some behaviour
--   (such as comparing for equlaity, comparing for ordering and enumeration)
--
-- - Types that can behave in that way are made isntances of that type class.
--
-- - The behaviour of type classes is achieved by defining functions or just
--   type declarations that we then implement.
--
-- - When we say that a type is a instance of a type class, we mean that we can
--   use the functions that the type class defined with that type.
--
-- ## Inside the `Eq` type class

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)

-- - class Eq a means a new type class called Eq is defined
--
-- - Note that its not mandatory to implement the function bodies themselves;
--   just their type declarations are required
--
-- - Function bodies for the functions that Eq defines are implemented -
--   defined in terms of mutual recursion. It basically says that two values
--   whose types are instances of Eq  are equal if they are not different, and
--   that they are different if they are not equal.
--
-- - The final type of the functions that we define in a type class is also worth
--   noting. If we have, say, class Eq a where , and then define a type declaration
--   within that class like (==) :: a -> a -> Bool , when we examine the type of
--   that function later, it will have the type of (Eq a) => a -> a -> Bool

-- ## A traffic light data type
--
-- - So once we have a class, we can make type instances of that class and get
--   some nice functionality
--
-- - Notice that we dont derive any class instances for it. Thats because we
--   are going to write some instances by hand
--
-- - See example below on how to make the TrafficLight type an instance of Eq
--
-- - class is for defining new type classes and instance is for making our
--   types instances of type classes

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- - Because == was defined in terms of /= and vice versa in the class declara-
--   tion, we needed to overwrite only one of them in the instance declaration.
--   That’s called the minimal complete definition for the type class—the minimum
--   of functions that we must implement so that our type can behave as the class
--   advertises. To fulfill the minimal complete definition for Eq , we need to over-
--   write either == or /=. If Eq were defined simply like this:
--
--   class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--
--   we would need to implement both of these functions when making a type an
--   instance of Eq, because Haskell wouldn’t know how these two functions are
--   related. The minimal complete definition would then be both == and /= .
--   You can see that we implemented == simply by doing pattern matching.
--   Since there are many more cases where two lights aren’t equal, we specified
--   the ones that are equal, and then just did a catchall pattern saying that if it’s
--   none of the previous combinations, then two lights aren’t equal.
--
--   Let’s make this an instance of Show by hand, too. To satisfy the minimal
--   complete definition for Show , we just need to implement its show function,
--   which takes a value and turns it into a string:
--
instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

-- - Lets see some examples
--   ghci> Red == Red
--   True
--   ghci> Red == Yellow
--   False
--   ghci> Red `elem` [Red, Yellow, Green]
--   True
--   ghci> [Red, Yellow, Green]
--   [Red light,Yellow light,Green light]

-- - We could have just derived Eq , and it would have had the same effect (but
--   we didn’t for educational purposes). However, deriving Show would have just
--   directly translated the value constructors to strings. If we want our lights to
--   appear as Red light, we need to make the instance declaration by hand.

-- ## Subclassing

-- - You can also make type classes that are subclasses of other type classes. The
--   class declaration for Num is a bit long, but here’s the first part:
--
-- class (Eq a) => Num a where
-- ...
--
-- - As mentioned previously, there are a lot of places where we can cram in
--   class constraints. So this is just like writing class Num a where , but we state that
--   our type a must be an instance of Eq. We’re essentially saying that we need
--   to make a type an instance of Eq before we can make it an instance of Num .
--   Before some type can be considered a number, it makes sense that we can
--   determine whether values of that type can be equated.
--   That’s all there is to subclassing—it’s just a class constraint on a class
--   declaration! When defining function bodies in the class declaration or in
--   instance declarations, we can assume that a is a part of Eq , so we can use ==
--   on values of that type.
--
--   ## Parameterized types as instances of type classes
--
--   - How are the Maybe or list types made instances of type classes?
--
--   - There is an error with the following example:
--
--     instance Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False
--
--   - We use == on the contents of the Maybe , but we have no assurance that
--     what the Maybe contains can be used with Eq! That’s why we modify our
--     instance declaration like this (to add a class constraint)
--
--     instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False
--
--   - if you want to see what the instances of a type class are, just type
--     :info YourTypeClass in GHCi.
--
--     ghci> :info Maybe
--     data Maybe a = Nothing | Just a -- Defined in Data.Maybe
--     instance (Eq a) => Eq (Maybe a) -- Defined in Data.Maybe
--     instance Monad Maybe -- Defined in Data.Maybe
--     instance Functor Maybe -- Defined in Data.Maybe
--     instance (Ord a) => Ord (Maybe a) -- Defined in Data.Maybe
--     instance (Read a) => Read (Maybe a) -- Defined in GHC.Read
--     instance (Show a) => Show (Maybe a) -- Defined in GHC.Show
