# Types and type-classes

## Defining a new data-type

Lets look how `Bool` is defined,

```haskell
data Bool = False | True
```

The syntax follows that the code on the left side of the `=` sign defines the type and the code on the right side defines the value contructors i.e, `data Bool` is the type while `False` and `True` are value constructors.

## Defining geometric data-types

A Circle can be defined as its `radius` and its `x` and `y` coordinate on a canvas.
A Rectangle can be defined as the coordinates of each corner.

Following example implements that in Haskell,

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

Note that value constructors are functions that return a value of a data-type i.e.,

```haskell
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape

ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

Lets now implement some functionality to our data-type. Note that the declarations says the function takes a `Shape` and returns a `Float`. We cannot write `Circle -> Float` since `Circle` is not a type. Only `Shape` is a type.

```haskell
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

```haskell
 ghci> area $ Circle 10 20 10
 314.15927

 ghci> area $ Rectangle 0 0 100 100
 10000.0
```

In order to print out a `Circle` or `Rectangle` to the prompt we need to make out type `Shape` an instance of the `Show` type class i.e. we need to redefine the type,

```haskell
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float deriving (Show)
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

Now, you can do the following,

```haskell
ghci> Circle 10 20 5
Circle 10.0 20.0 5.0

ghci> Rectangle 50 230 60 90
Rectangle 50.0 230.0 60.0 90.0
```

## Improving our `Shape` type with a `Point` type

```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

```haskell
ghci> area (Rectangle (Point 0 0) (Point 100 100))
10000.0

ghci> area (Circle (Point 0 0) 24)
1809.5574
```

Lets implement a function `nudge` that can move the position of a `Shape` type on a canvas,

```haskell
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) x_delta y_delta = Circle (Point (x + x_delta) (y + y_delta)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x_delta y_delta = Rectangle (Point (x1 + x_delta) (y1 + y_delta)) (Point (x2 + x_delta) (y2 + y_delta))
```

Lets test this in `GHCI`,

```haskell
ghci> nudge (Circle (Point 34 34) 10) 5 10
Circle (Point 39.0 44.0) 10.0
```

If we don’t want to deal with points directly, we can make some auxiliary functions that create shapes of some size at the zero coordinates and then nudge those,

```haskell
baseCircle :: Float -> Shape''
baseCircle r = Circle'' (Point 0 0) r

baseRect :: Float -> Float -> Shape''
baseRect width height = Rectangle'' (Point 0 0) (Point width height)
```

Lets now test this in `GHCI`,

```haskell
GHCi> nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
```

## Exporting our `Shape` type in a module

We can export our `Shape` by inserting the code above in a new file and inserting following code at the top of that file,

```haskell
module Shapes
( Point(..)
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
) where
```

By using `Shape(..)`, we export all the value constructors for Shape . This means that people who import our module can make shapes by using the Rectangle and Circle value constructors. Also, if we decide to add some value constructors to our type later on, we don’t need to modify the exports. That’s because using `..` automatically exports all value constructors for agiven type.

Alternatively, we could opt to not export any value constructors for `Shape` by just writing `Shape` in the export statement, without the parentheses. That way, people who import our module could make shapes only by using the auxiliary functions `baseCircle` and `baseRect`.

Remember that value constructors are just functions that take the fields as parameters and return a value of some type (like `Shape` ). So when we choose not to export them, we prevent the person importing our module from using those value constructors directly. Not exporting the value con- structors of our data types makes them more abstract, since we’re hiding their implementation. Also, whoever uses our module can’t pattern match against the value constructors. This is good if we want people who import our module to be able to interact with our type only via the auxiliary functions that we supply in our module. That way, they don’t need to know about the internal details of our module, and we can change those details whenever we want, as long as the functions that we export act the same.

## Record syntax

Lets create a data type that describes a Person using two different methods,

### The not so smart way

```haskell
data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
```

Lets now run some examples in `GHCI`,

```haskell
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

ghci> guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

ghci> firstName guy
"Buddy"

ghci> height guy
184.2

ghci> flavor guy
"Chocolate"
```

### The much better way

```haskell
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Int,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)
```

The main benefit of using this syntax is that it creates functions that look up fields in the data type. By using record syntax to create this data type, Haskell automatically makes these functions: `firstName` , `lastName` , `age` , `height` , `phoneNumber` , and `flavor` . Take a look,

```haskell
ghci> :t flavor
flavor :: Person -> String

ghci> :t firstName
firstName :: Person -> String
```

### Another example with a `Car`

The not so smart way,

```haskell
data Car = Car String String Int deriving (Show)
```

```haskell
ghci> Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967
```

The better way,

```haskell
data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)
```

```haskell
ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
```

## Type parameters

A **value constructor** can take some parameters and produce a new **value**.

A **type constructor** can take types as parameters and produce a new **type**.

In the following example,

```haskell
data Maybe a = Nothing | Just a
```

The `a` is a **type parameter** and since there is a type parameter involved, we call `Maybe` a **type constructor**

Depending on what we want this data type to hold when its not `Nothing`, this type constructor can end up producing a type of `Maybe Int`, `Maybe Car`, `Maybe String` etc.

No value can have a type of just `Maybe`, because thats not a type -- Its a **type constructor**

Most of the time, we dont pass types as parameters to type constructors explicitly. Thats because Haskell has type inference. So when we make a value `Just a` for example. Haskell figures out that its a `Maybe Char`.

If we want to explicitly pass a type as a type parameter, we must do it in the type part of Haskell, which usually after the :: symbol.

Some examples,

```haskell
ghci> Just 3 :: Maybe Int
Just 3

ghci> Just "Haha"
Just "Haha"

ghci> Just 84
Just 84

ghci> :t Just "Haha"
Just "Haha" :: Maybe [Char]

ghci> :t Just 84
Just 84 :: (Num a) => Maybe a

ghci> :t Nothing
Nothing :: Maybe a

ghci> Just 10 :: Maybe Double
Just 10.0
```

## Examples with `Car`

See following two examples,

**Without parameterization:**

```haskell
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
```

**With parameterization:**

```haskell
data Car a b c = Car {company' :: a, model :: b, year :: c} deriving (Show)
```

Question is, are we better of with or without parameterization?

See examples **without** parameterization,

```haskell
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

```haskell
ghci> let stang = Car {company="Ford", model="Mustang", year=1967}

ghci> tellCar stang
"This Ford Mustang was made in 1967
```

Now, see examples **with** parameterization,

```haskell
tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y}) = "This " ++ c ++ " " ++ m ++ " was made in" ++ show y
```

```haskell
ghci> tellCar (Car "Ford" "Mustang" 1967)
"This Ford Mustang was made in 1967"

ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")
"This Ford Mustang was made in \"nineteen sixty seven\""

ghci> :t Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t

ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"
Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]
```

Conculsion is that we are not better off. In real life though, we would end up using Car String String Int most of the time. So, parameterizing the Car type isn’t worth it.

## Examples and illustrations with `Map`

Another example of a parameterized type is `Map k v` from `Data.Map`. The `k` is the type of the keys in a map, and `v` is the type of the values. This is a good example of where type parameters are very useful. Having maps parameterized enables us to have mappings from any type to any other type, as long as the type of the key is part of the Ord type class. If we were defining a mapping type, we could add a type class constraint in the data declaration:

```haskell
data (Ord k) => Map k v = ..
```

However, it’s a very strong convention in Haskell to never add type class constraints in data declarations because it doesn’t provide much benefit, and we end up writing more class constraints, even when we don’t need them. If we put the `Ord k` constraint in the data declaration for `Map k v` , we still need to put the constraint into functions that assume the keys in a map can be ordered. If we don’t put the constraint in the data declaration, then we don’t need to put `(Ord k) =>` in the type declarations of functions that don’t care whether the keys can be ordered. An example of such a function is `toList`, which just takes a mapping and converts it to an associative list.

Its type signature is `toList :: Map k a -> [(k, a)]` . If `Map k v` had a type constraint in its data declaration, the type for `toList` would need to be `toList :: (Ord k) => Map k a -> [(k, a)]`, even though the function doesn’t compare keys by order.

So don’t put type constraints into data declarations, even if it seems to make sense. You’ll need to put them into the function type declarations either way

## Examples with Vector algebra

```haskell
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

dotprod :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `dotprod` (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector x1 y1 z1) s = Vector (x1 * s) (y1 * s) (z1 * s)
```

These functions can operate on any type in the form of `Vector a` , as long as the `a` is an instance of the `Num` type class. For instance, they can operate on values of type `Vector Int` , `Vector Integer` , `Vector Float` , and so on, because `Int` , `Integer`, and `Float` are all instances of the `Num` type class. However, they won’t work on values of type `Vector Char` or `Vector Bool` .

Also, if you examine the type declaration for these functions, you’ll see that they can operate only on vectors of the same type, and the numbers involved must also be of the type that is contained in the vectors. We can’t add together a Vector Int and a `Vector Double`.

Notice that we didn’t put a `Num` class constraint in the data declaration. As explained in the previous section, even if we put it there, we would still need to repeat it in the functions.

```haskell
ghci> Vector 3 5 8 `vplus` Vector 9 2 8
Vector 12 7 16

ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 12 9 19

ghci> Vector 3 9 7 `vmult` 10
Vector 30 90 70

ghci> Vector 4 9 5 `dotProd` Vector 9.0 2.0 4.0
74.0

ghci> Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9 2 4)
Vector 148 666 222
```

## Derived instances

A type class is a sort of an interface that defines some behavior, and that a type can be made an instance of a type class if its compliant with the interfaces behaviour.

For example, the `Int` type is an instance of the `Eq` type class because the `Eq` type class defines behaviour for stuff that can be equated.

Haskell type classes are often confused with classes in languages like Java, Python, C++ and the like, which trips up a lot of programmers. In those languages, classes are a blueprint from which we create objects that can do some actions. But we don’t make data from Haskell type classes. Instead, we first make our data type, and then we think about how it can act. If it can act like something that can be equated, we make it an instance of the `Eq` type class. If it can act like something that can be ordered, we make it an instance of the `Ord` type class.

### Deriving instances from the `Eq` type class

The definition below states that `Person` is an instance of `Eq` type class.

```haskell
data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Eq)
```

Some examples,

```haskell
ghci> mikeId = Person {firstName = "Michael", lastName = "Diamond", age = 43}

ghci> adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

ghci> mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

ghci> mca == adRock
False

ghci> mikeD == adRock
False

ghci> mikeD == mikeD
True

ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
True

ghci> let beastieBoys = [mca, adRock, mikeD]

ghci> mikeD `elem` beastieBoys
True
```

### Deriving instances from the `Show` and `Read` typeclasses

```haskell
data Person' = Person' {firstName' :: String, lastName' :: String, age' :: Int} deriving (Eq, Show, Read)
```

Some examples,

```haskell
ghci> mikeId = Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
ghci> adRock = Person {firstName' = "Adam", lastName' = "Horovitz", age' = 41}
ghci> mca = Person {firstName' = "Adam", lastName' = "Yauch", age' = 44}

ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}

ghci> "mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

mysteryDude = "Person' { firstName' =\"Michael\"" ++ ", lastName' =\"Diamond\"" ++ ", age' = 43}"
ghci> read mysteryDude :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}

ghci> read mysteryDude == mikeD
True

ghci> read "Just 3" :: Maybe Int
Just 3
```

### Deriving instances from the `Ord` type class

We can derive instances for the `Ord` type class, which is for types that have values that can be ordered.

Lets see an example,

```haskell
data Bool' = False' | True' deriving (Eq, Ord)
```

```haskell
ghci> True' `compare` False'
GT

ghci> True' > False'
True

ghci> True' < False'
False
```

If two values were made using the same constructor, they are considered to be equal, unless they have fields. If they have fields, the fields are compared to see which is greater. (Note that in this case, the types of the fields also must be part of the `Ord` type class.) In the `Maybe a` data type, the `Nothing` value constructor is specified before the `Just` value constructor, so the value of `Nothing` is always smaller than the value of `Just` something, even if that something is minus one billion trillion. But if we specify two `Just` values, then it will compare what’s inside them.

```haskell
ghci> Nothing < Just 100
True

ghci> Nothing > Just (-49999)
False

ghci> Just 3 `compare` Just 2
GT

ghci> Just 100 > Just 50
True
```

### Deriving instances from the `Enum` and `Bounded` type classes

The `Enum` type class is for things that have predeccessors and successors.

The `Bounded` type class is for things that have a lowest possible value and highest possible value.

```haskell

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

Lets run some examples,

```haskell
ghci> Wednesday
Wednesday

ghci> show Wednesday
"Wednesday"

ghci> read "Saturday" :: Day
Saturday

ghci> Saturday == Sunday
False

ghci> Saturday == Saturday
True

ghci> Saturday > Friday
True

ghci> Monday `compare` Wednesday
LT

ghci> minBound :: Day
Monday

ghci> maxBound :: Day
Sunday

ghci> succ Monday
Tuesday

ghci> pred Saturday
Friday

ghci> [Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]

ghci> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
```

## Type synonyms

`[Char]` and `String` are equivalent and interchangeble. The reason for this is that `String` is implemented with a type synonym,

```haskell
type String = [Char]
```

## Making our PhoneBook prettier with type synonyms

```haskell
import qualified Data.Map as Map

type PhoneNumber = String

type Name = String

type Phonebook = [(Name, PhoneNumber)]

phoneBook :: Phonebook
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

inPhoneBook :: Name -> PhoneNumber -> Phonebook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
```

## Parameterizing type synonyms

Type synonyms can be parameterized. If we want a type that represents an association list type, but still want it to be general so it can use any type as the keys and values. We can define that in following way,

```haskell
type AssocList k v = [(k, v)]
```

Now a function that gets the value by a key in an association list can have the type `(Eq k) => k -> AssocList k v -> Maybe v`

`AssocList` is a type constructor that takes two types and produces a concerete type - for instance `AssocList Int String`

Just as we can partially apply functions to get new functions, we can partially apply type parameters and get new type constructors from them. When we call a function with too few parameters, we get back a new function. In the same way, we can specify a type constructor with too few type parameters and get back a partially applied type constructor. If we wanted a type that represents a `map` (from `Data.Map` ) from integers to something, we could do this:

```haskell
type IntMap v = Map.Map Int v
```

or,

```haskell
type IntMap = Map.Map Int
```

Either way, the `IntMap` type constructor takes one parameter, and that is the type of what the integers will point to.

## Either, Left or Right

The data type has two value constructors i.e. If `Left` is used, then its contents are of type `a` and if `Right` is used, then its contents are of type `b`

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

Some examples

```haskell
ghci> Right 20
Right 20

ghci> Left "w00t"
Left "w00t"

ghci> :t Right 'a'
Right 'a' :: Either a Char

ghci> :t Left True
Left True :: Either Bool b
```

So far, you’ve seen `Maybe a` mostly used to represent the results of com- putations that could have failed. But sometimes, `Maybe a` isn’t good enough, because `Nothing` doesn’t convey much information other than that something has failed. That’s fine for functions that can fail in only one way, or if we’re not interested in how or why they failed. For instance, a `Data.Map` lookup fails only if the key wasn’t in the map, so we know exactly what happened.

However, when we’re interested in how or why some function failed, we usually use the result type of `Either a b` , where `a` is a type that can tell us something about the possible failure, and `b` is the type of a successful computation. Hence, errors use the `Left` value constructor, and results use `Right`.

Lets write a program that illustrates the discussions above,

```haskell
data LockerStatus = Taken | Free deriving (Eq, Ord)

type LockerNumber = Int

type Code = String

type ErrorMessage = String

type LockerMap = Map.Map LockerNumber (LockerStatus, Code)

lockerLookup :: LockerNumber -> LockerMap -> Main.Either ErrorMessage Code
lockerLookup lockernumber map = case Map.lookup lockernumber map of
  Nothing -> Main.Left $ "The lockernumber " ++ show lockernumber ++ " doesnt exist"
  Just (status, code) ->
    if status /= Taken
      then Main.Right code
      else Main.Left $ "The locker " ++ show lockernumber ++ " is taken"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]
```

Now, lets test the program,

```haskell
ghci> lockerLookup 101 lockers
Right "JAH3I"

ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"

ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"

ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"

ghci> lockerLookup 105 lockers
Right "QOTSA"
```

We could have used a `Maybe a` to represent the result, but then we wouldn’t know why we couldn’t get the code. But now we have information about the failure in our result type.

## Recursive data structures

A constructor in an algebraic data type can have several fields and each field must be of some concrete type. This means that we can make types that have themselves as types in their fields i.e. recursive data types where one value of some type contains values of that type, which in turn contains more values of the same type.

```haskell
[5] = 5 : []
[4, 5] = 4 : (5 : []) = 4 : 5 : [] (Since : is right-associative)
[3, 4, 5, 6] = 3 : ( 4 : (5 : (6 : []))) = 3 : 4 : 5 : 6 : [] (Since : is right-associative)
```

The examples above shows that `:` is actually a constructor that takes a value and another list and returns a list.

Lets define a List data-type,

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

Now, lets look at some examples,

```haskell
ghci> Empty
Empty

ghci> 5 `Cons` Empty
Cons 5 Empty

ghci> 4 `Cons` (5 `Cons` Empty)
Cons 4 (Cons 5 Empty)

ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
Cons 3 (Cons 4 (Cons 5 Empty))
```

We called our `Cons` constructor in an `infix` manner so you can see how it’s just like `:`. `Empty` is like `[]` , and `4 'Cons' (5 'Cons' Empty)` is like `4:(5:[])` .

## Improving our List

We can define functions to be automatically infix by naming them using only special characters. We can also do the same with constructors, since they’re just functions that return a data type. There is one restriction however: Infix constructors must begin with a colon.

Fixity declaration states how tigtly the operator binds and whether its left-assoiciative or right-associative according to examples below,

See `*` as an example,

The `*` operator fixity is `infixl 7 *`. This means that `5 * 4 * 3 * 2 * 1 = ((((5 * 4) * 3) * 2) * 1)`

See `+` as an example,

The `+` operator fixity is `infixl 6 +` This means that `5 + 4 + 3 + 2 + 1 = ((((5 + 4) + 3) + 2) + 1)`

NOTE that the `*` operator binds tighter (fixity 7) compared to the operator `+` (fixity 6). This means that `5 * 4 + 3 = ((5 * 4) + 3) = 20 + 3 = 23`

Now, lets improve our list with `:-:`,

```haskell
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
```

Some examples

```haskell
ghci> 3 :-: 4 :-: 5 :-: Empty
3 :-: (4 :-: (5 :-: Empty))

ghci> let a = 3 :-: 4 :-: 5 :-: Empty

ghci> 100 :-: a
100 :-: (3 :-: (4 :-: (5 :-: Empty)))
```

Now, we can make a function that adds two lists together. We can find inspiration from the function `++` that adds lists together. See implementation below,

```haskell
infixr 5 ++

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)
```

Now, lets draw inspiration from above to our List,

```haskell
infixr 5 ^++

(^++) :: List' a -> List' a -> List' a
Empty' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
```

An example of concatenation of lists,

```haskell
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> let b = 6 :-: 7 :-: Empty

ghci> a ^++ b
3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))
```

## Implementation of a Tree-structure in Haskell

```haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- A function that returns a Node and two empty trees
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- A function that inserts a value into a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x == y = Node x left right
  | x > y = Node y left (treeInsert x right)
  | x < y = Node y (treeInsert x left) right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x > y = treeElem x right
  | x < y = treeElem x left
```

Please note: In this `foldr` example below, `treeInsert` is the folding binary function (it takes a tree and a list element and produces a new tree), and `EmptyTree` is the starting accumulator. `nums`, of course, is the list we’re folding over.

```haskell
ghci> let nums = [8,6,4,1,7,3,5]
ghci> let numsTree = foldr treeInsert EmptyTree nums
ghci> numsTree

Node 5

  (Node 3
    (Node 1 EmptyTree EmptyTree)
    (Node 4 EmptyTree EmptyTree))

  (Node 7
    (Node 6 EmptyTree EmptyTree)
    (Node 8 EmptyTree EmptyTree))
```

Testing the `treeElem` function,

```haskell
ghci> 8 `treeElem` numsTree
True

ghci> 100 `treeElem` numsTree
False

ghci> 1 `treeElem` numsTree
True

ghci> 10 `treeElem` numsTree
False
```

## Type-classes

Type classes are like interfaces i.e. type classes defines some behaviour (such as comparing for equlaity, comparing for ordering and enumeration).

Types that can behave in that way are made isntances of that type class.

The behaviour of type classes is achieved by defining functions or just type declarations that we then implement.

When we say that a type is a instance of a type class, we mean that we can use the functions that the type class defined with that type.

### The `Eq` type class

Definition,

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```

`class Eq a` means a new type class called Eq is defined.

Note that its not mandatory to implement the function bodies themselves; just their type declarations are required

Function bodies for the functions that `Eq` defines are implemented / defined in terms of mutual recursion. It basically says that two values whose types are instances of `Eq` are equal if they are not different, and that they are different if they are not equal.

The final type of the functions that we define in a type class is also worth noting. If we have, say, `class Eq a` and then define a type declaration within that class like `(==) :: a -> a -> Bool` , when we examine the type of that function later, it will have the type of `(Eq a) => a -> a -> Bool`.

### A `TrafficLight` data type

So once we have a class, we can make type instances of that class and get some nice functionality.

Notice that we dont derive any class instances for it. Thats because we are going to write some instances by hand.

See example below on how to make the `TrafficLight` type an instance of `Eq`,

Note that `class` is for defining new type classes and `instance` is for making our types instances of type classes

```haskell
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
Red == Red = True
Green == Green = True
Yellow == Yellow = True
_ == _ = False
```

Because `==` was defined in terms of `/=` and vice versa in the class declaration, we needed to overwrite only one of them in the instance declaration. That’s called the **minimal complete definition** for the type class—the minimum of functions that we must implement so that our type can behave as the class advertises. To fulfill the minimal complete definition for `Eq` , we need to overwrite either `==` or `/=`. If `Eq` were defined simply like this:

```haskell
class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

We would need to implement both of these functions when making a type an instance of `Eq`, because Haskell wouldn’t know how these two functions are related. The minimal complete definition would then be both `==` and `/=` . You can see that we implemented `==` simply by doing pattern matching. Since there are many more cases where two lights aren’t equal, we specified the ones that are equal, and then just did a catchall pattern saying that if it’s none of the previous combinations, then two lights aren’t equal.

Let’s make this an instance of `Show` by hand, too. To satisfy the minimal complete definition for `Show` , we just need to implement its show function, which takes a value and turns it into a string,

```haskell
instance Show TrafficLight where
show Red = "Red Light"
show Yellow = "Yellow Light"
show Green = "Green Light"
```

Some examples,

```haskell
ghci> Red == Red
True

ghci> Red == Yellow
False

ghci> Red `elem` [Red, Yellow, Green]
True

ghci> [Red, Yellow, Green]
[Red light,Yellow light,Green light]
```

We could have just derived `Eq` , and it would have had the same effect (but we didn’t for educational purposes). However, deriving `Show` would have just directly translated the value constructors to strings. If we want our lights to appear as `Red light`, we need to make the instance declaration by hand.

## Subclassing

You can also make type classes that are subclasses of other type classes. The class declaration for Num is a bit long, but here’s the first part:

```haskell
class (Eq a) => Num a where
```

There are a lot of places where we can cram in class constraints. So this is just like writing `class Num a where` , but we state that our type a must be an instance of `Eq`. We’re essentially saying that we need to make a type an instance of `Eq` before we can make it an instance of `Num` . Before some type can be considered a number, it makes sense that we can determine whether values of that type can be equated. That’s all there is to subclassing—it’s just a class constraint on a class declaration! When defining function bodies in the class declaration or in instance declarations, we can assume that a is a part of `Eq` , so we can use `==` on values of that type.

## Parameterized types as instances of type classes

How are the Maybe or list types made instances of type classes?

There is an error with the following example:

```haskell
instance Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```

We use `==` on the contents of the `Maybe` , but we have no assurance that what the `Maybe` contains can be used with `Eq`. That’s why we modify our instance declaration like this (to add a class constraint)

```haskell
instance (Eq m) => Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```

if you want to see what the instances of a type class are, just type `:info YourTypeClass` in GHCi.

```haskell
ghci> :info Maybe
data Maybe a = Nothing | Just a -- Defined in Data.Maybe
instance (Eq a) => Eq (Maybe a) -- Defined in Data.Maybe
instance Monad Maybe -- Defined in Data.Maybe
instance Functor Maybe -- Defined in Data.Maybe
instance (Ord a) => Ord (Maybe a) -- Defined in Data.Maybe
instance (Read a) => Read (Maybe a) -- Defined in GHC.Read
instance (Show a) => Show (Maybe a) -- Defined in GHC.Show
```

## A Yes or No type class

In javascript, all these expressions will throw an alert of NO!

```javascript
if (0) alert("YEAH!") else alert("NO!")
if ("") alert ("YEAH!") else alert("NO!")
if (false) alert("YEAH!") else alert("NO!")
```

This will throw an alert of Yes! since javascript considers any nonempty string to be a true value:

```javascript
if ("WHAT") alert ("YEAH!") else alert("NO!")
```

Lets now try to implement this in Haskell

```haskell
class YesNo a where
yesno :: a -> Bool

instance YesNo [a] where
yesno [] = False
yesno _ = True

instance YesNo Bool where
yesno = id
-- id is just a standard library function that takes the same parameter and returns the same thing

instance YesNo (Maybe a) where
yesno (Just _) = True
yesno Nothing = False

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

instance YesNo (Tree a) where
yesno EmptyTree = False
yesno _ = True

data TrafficLight = Red | Yellow | Green

instance YesNo TrafficLight where
yesno Red = False
yesno Yellow = False
yesno _ = True
```

Now, lets run some examples in `GHCI`,

```haskell
ghci> yesno $ length []
False

ghci> yesno "haha"
True

ghci> yesno ""
False

ghci> yesno $ Just 0
True

ghci> yesno True
True

ghci> yesno EmptyTree
False

ghci> yesno []
False

ghci> yesno [0,0,0]
True

ghci> :t yesno
yesno :: (YesNo a) => a -> Bool
```

Now lets implement the function that mimics the if else statement,

```haskell
yesnoIf :: (YesNo b) => b -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
```

## The `Functor` type class

The `Functor` type class is for things that can be mapped over. For example, the list type is part of the functor type class.

The `Functor` type class is defined in the following way,

```haskell
class Functor f where
fmap :: (a -> b) -> f a -> f b
```

`fmap` doesnt provide any default implementation.

The `f` is not a concrete type (`a` type that a value can hold, like `Int`, `Bool`, or `Maybe String`) but a type constructor that takes one type parameter (Example: `Maybe Int` is a concrete type, but `Maybe` is a type constructor that takes one type as the parameter)

`fmap` takes a function from one type to another and a functor value applied
with one type and returns a functor value applied with another type.

The type signature of the map function looks like following,

```haskell
map :: (a -> b) -> [a] -> [b]
```

The reason to why it almost looks the same as the definition above is because the list is an instance of the `Functor` type class:

instance Functor [] where
fmap = map

Notice how we didnt write `instance Functor [a] where`. This is because `f` must be a type constructor that takes one type. `[a]` is already a concrete type while `[]` is a type constructor that takes one type and can produce types such as `[Int]`, `[String]`, or even `[[String]]`

See examples below:

```haskell
ghci> fmap (\*2) [1..3]
[2,4,6]

ghci> map (\*2) [1..3]
[2,4,6]
```

## Maybe as an Functor

Types that can act like a box can act like functors. You can think of a list as a box that can be empty or have something inside it, including another box.

`Maybe a` can also be seen as a box i.e. it can hold Nothing or it can contain one item.

Here is how `Maybe` is a functor:

```haskell
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
```

Notice how we wrote `instance Functor Maybe where` instead of `instance Functor (Maybe m) where` , as we did when we were dealing with `YesNo`. Functor wants a type constructor that takes one type, and not a concrete type.

Some examples,

```haskell
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
Just "Something serious. HEY GUYS IM INSIDE THE JUST"

ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
Nothing

ghci> fmap (\*2) (Just 200)
Just 400

ghci> fmap (\*2) Nothing
Nothing
```

## Trees as an Functor

```haskell
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
```

Examples,

```haskell
ghci> fmap (\*2) EmptyTree
EmptyTree

ghci> fmap (\*4) (foldr treeInsert EmptyTree [5,7,3])
Node 20 (Node 12 EmptyTree EmptyTree) (Node 28 EmptyTree EmptyTree)
```

## Either a As a Functor

The `Functor` type class wants a type constructor that takes only one type parameter, but `Either` takes two. The solution to this is to partially apply `Either` by feeding it only one parameter, so that it has one free parameter.

```haskell
instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x
```

You can see how `Either a` was made an instance instead of just `Either`. Thats because `Either a` is a type constructor that takes one parameter, whereas `Either` takes two.

If `fmap` were specifically for `Either a`, the type signature would be this:

```haskell
(b -> c) -> Either a b -> Either a c
```

Because this is the same as following:

```haskell
(b -> c) -> (Either a) b -> (Either a) c
```

The function is mapped in the case of a `Right` value constructor, but it isn’t mapped in the case of a `Left`. Why is that? Well, looking back at how the `Either a b` type is defined, we see this,

```haskell
data Either a b = Left a | Right b
```

If we wanted to map one function over both of them, `a` and `b` would need to be the same type. Think about it: If we try to map a function that takes a string and returns a string, and `b` is a string but `a` is a number, it won’t really work out. Also, considering what `fmap` ’s type would be if it operated only on `Either a b` values, we can see that the first parameter must remain the same, while the second one can change, and the first parameter is actualized by the `Left` value constructor.

This also goes nicely with our box analogy if we think of the Left part as sort of an empty box with an error message written on the side telling us why it’s empty.

Maps from `Data.Map` can also be made into functor values, because they hold values (or not!). In the case of `Map k v` , `fmap` will map a function `v -> v'` over a map of type `Map k v` and return a map of type `Map k v'`.
