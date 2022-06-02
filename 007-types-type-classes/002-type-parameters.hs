-- # Type parameters
--
-- - A value constructor can take some parameters and then produce a new value
-- - In a similar manner, **type constuctors** can take types as parameters and produce new types
--
-- In the following example,
--
-- - The `a` is a **type parameter**
-- - Since there is a type parameter involved, we call `Maybe` a **type constructor**
-- - Depending on what we want this data type to hold when its not `Nothing`,
--   this type constructor can end up produciong a type of `Maybe Int`, `Maybe
--   Car`, `Maybe String` etc.
-- - No value can have a type of just `Maybe`, because thats not a type -- Its a type constructor.

data Maybe a = Nothing | Just a

-- - Most of the time, we dont passs types as parameters to type constructors
--   explicitly. Thats because Haskell has type inference. So when we make a
--   value `Just a` for example. Haskell figures out that its a `Maybe Char`.
--
-- - If we want to explicitly pass a type as a type paramter, we must do it in
--   the type part of Haskell, which usually after the :: symbol.
--
-- Lets look at some examples
--
-- ghci> Just 3 :: Maybe Int
-- Just 3

-- ghci> Just "Haha"
-- Just "Haha"
--
-- ghci> Just 84
-- Just 84
--
-- ghci> :t Just "Haha"
-- Just "Haha" :: Maybe [Char]
--
-- ghci> :t Just 84
-- Just 84 :: (Num a) => Maybe a
--
-- ghci> :t Nothing
-- Nothing :: Maybe a
--
-- ghci> Just 10 :: Maybe Double
-- Just 10.0

-- ## Should we parameterize our car
--
-- ### Examples and illustrations with Car

-- See the first example

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- Are we better of by paramerize like this?

data Car' a b c = Car' {company' :: a, model' :: b, year' :: c} deriving (Show)

-- But are we better off? Probably not. See examples below.

--  First without parameterization

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- We can test this with:

-- ghci> let stang = Car {company="Ford", model="Mustang", year=1967}
-- ghci> tellCar stang
-- "This Ford Mustang was made in 1967

-- Next with parameterization

tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y}) = "This " ++ c ++ " " ++ m ++ " was made in" ++ show y

-- ghci> tellCar (Car "Ford" "Mustang" 1967)
-- "This Ford Mustang was made in 1967"
--
-- ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")
-- "This Ford Mustang was made in \"nineteen sixty seven\""
--
-- ghci> :t Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t
--
-- ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"
-- Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]

-- Conculsion is that we are not better off. In real life though, we would end
-- up using Car String String Int most of the time. So, parameterizing the Car
-- type isn’t worth it.

-- ### Example and illustrations with Map
--
-- Another example of a parameterized type that you’ve already met is
-- Map k v from Data.Map. The k is the type of the keys in a map, and v is the type
-- of the values. This is a good example of where type parameters are very use-
-- ful. Having maps parameterized enables us to have mappings from any type
-- to any other type, as long as the type of the key is part of the Ord type class. If
-- we were defining a mapping type, we could add a type class constraint in the
-- data declaration:

-- data (Ord k) => Map k v = ..

-- However, it’s a very strong convention in Haskell to never add type class
-- constraints in data declarations. Why? Well, because it doesn’t provide much
-- benefit, and we end up writing more class constraints, even when we don’t need
-- them. If we put the Ord k constraint in the data declaration for Map k v , we
-- still need to put the constraint into functions that assume the keys in a map
-- can be ordered. If we don’t put the constraint in the data declaration, then we
-- don’t need to put (Ord k) => in the type declarations of functions that don’t
-- care whether the keys can be ordered. An example of such a func- tion is
-- toList, which just takes a mapping and converts it to an associative list.
--
-- Its type signature is toList :: Map k a -> [(k, a)] . If Map k v had a type con-
-- straint in its data declaration, the type for toList would need to be toList ::
-- (Ord k) => Map k a -> [(k, a)], even though the function doesn’t compare
-- keys by order.
--
-- So don’t put type constraints into data declarations, even if it seems to
-- make sense. You’ll need to put them into the function type declarations ei-
-- either way

-- ## Vector von doom
--
-- vplus, dotprod, vmult

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

dotprod :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `dotprod` (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector x1 y1 z1) s = Vector (x1 * s) (y1 * s) (z1 * s)

-- These functions can operate on any type in the form of Vector a , as long as
-- the a is an instance of the Num type class. For instance, they can operate on
-- values of type Vector Int , Vector Integer , Vector Float , and so on, because
-- Int , Integer, and Float are all instances of the Num type class. However, they
-- won’t work on values of type Vector Char or Vector Bool . Also, if yo
--
-- Also, if you examine the type declaration for these functions, you’ll see
-- that they can operate only on vectors of the same type, and the numbers in-
-- volved must also be of the type that is contained in the vectors. We can’t add
-- together a Vector Int and a Vector Double
--
-- Notice that we didn’t put a Num class constraint in the data declaration.
-- As explained in the previous section, even if we put it there, we would still
-- need to repeat it in the functions. Once
--
-- Lets test these functions:
--
--
-- ghci> Vector 3 5 8 `vplus` Vector 9 2 8
-- Vector 12 7 16
-- ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
-- Vector 12 9 19
-- ghci> Vector 3 9 7 `vmult` 10
-- Vector 30 90 70
-- ghci> Vector 4 9 5 `dotProd` Vector 9.0 2.0 4.0
-- 74.0
-- ghci> Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9 2 4)
-- Vector 148 666 222
