-- # Making your own types and type classes

-- ## Defining a new data type
-- You can define your own type with the `data` keyword
data Bool = False | True

-- Syntax:
-- data Bool (This denotes the type) = False | True (These are the value constructors)

-- ## Shaping up
--
-- A Circle can be defined as its radius and its x and y coordinate on a canvas
-- A Rectangle can be defined as the coordinates of each corner
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- Value constructors are functions that return a value of a data type i.e.
-- ghci> :t Circle
-- Circle :: Float -> Float -> Float -> Shape
--
-- ghci> :t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape

-- Some function as examples of application
-- Please nbote the declaration, it says the function takes a Shape and returns a Float
-- We cannot write Circle -> Float since Circle is not a type. Only Shape is a type!
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Lets use the functions to illustrate:
-- ghci> area $ Circle 10 20 10
-- 314.15927
-- ghci> area $ Rectangle 0 0 100 100
-- 10000.0

-- How can we print out e.g. Circle 10 20 5 to the promt?
-- This can be done by making our Shape type part of the Show type class
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ _ r) = pi * r ^ 2
area' (Rectangle' x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Lets use the functions to illustrate:
-- ghci> Circle' 10 20 5
-- Circle' 10.0 20.0 5.0
-- ghci> Rectangle' 50 230 60 90
-- Rectangle' 50.0 230.0 60.0 90.0
--
-- ## Improving Shape with the Point Data Type
--
data Point = Point Float Float deriving (Show)

data Shape'' = Circle'' Point Float | Rectangle'' Point Point deriving (Show)

area'' :: Shape'' -> Float
area'' (Circle'' _ r) = pi * r ^ 2
area'' (Rectangle'' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- ghci> area (Rectangle (Point 0 0) (Point 100 100))
-- 10000.0
--
-- ghci> area (Circle (Point 0 0) 24)
-- 1809.5574

-- Another example is when we want to move a shape on the canvas. We call this function `nudge`
nudge :: Shape'' -> Float -> Float -> Shape''
nudge (Circle'' (Point x y) r) x_delta y_delta = Circle'' (Point (x + x_delta) (y + y_delta)) r
nudge (Rectangle'' (Point x1 y1) (Point x2 y2)) x_delta y_delta = Rectangle'' (Point (x1 + x_delta) (y1 + y_delta)) (Point (x2 + x_delta) (y2 + y_delta))

-- Tests in GHCi:
-- ghci> nudge (Circle (Point 34 34) 10) 5 10
-- Circle (Point 39.0 44.0) 10.0

-- If we don’t want to deal with points directly, we can make some auxiliary
-- functions that create shapes of some size at the zero coordinates and then
-- nudge those
baseCircle :: Float -> Shape''
baseCircle r = Circle'' (Point 0 0) r

baseRect :: Float -> Float -> Shape''
baseRect width height = Rectangle'' (Point 0 0) (Point width height)

-- Lets test this in GHCi
-- GHCi> nudge (baseRect 40 100) 60 23
-- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

-- ## Exporting our shapes in a module
--
-- We could do this by inserting the code above in a new file under the format below,
-- module Shapes
-- ( Point(..)
-- , Shape(..)
-- , area
-- , nudge
-- , baseCircle
-- , baseRect
-- ) where
--
-- By using Shape(..), we export all the value constructors for Shape . This
-- means that people who import our module can make shapes by using the
-- Rectangle and Circle value constructors. It’s the same as writing Shape
-- (Rectangle, Circle), but shorter. Also, if we decide to add some value
-- constructors to our type later on, we don’t need to modify the exports.
-- That’s because using .. automatically exports all value constructors for a
-- given type.
--
-- Alternatively, we could opt to not export any value constructors for Shape
-- by just writing Shape in the export statement, without the parentheses. That
-- way, people who import our module could make shapes only by using the
-- auxiliary functions baseCircle and baseRect
--
-- Remember that value constructors are just functions that take the fields
-- as parameters and return a value of some type (like Shape ). So when we
-- choose not to export them, we prevent the person importing our module
-- from using those value constructors directly. Not exporting the value con-
-- structors of our data types makes them more abstract, since we’re hiding
-- their implementation. Also, whoever uses our module can’t pattern match
-- against the value constructors. This is good if we want people who import
-- our module to be able to interact with our type only via the auxiliary func-
-- tions that we supply in our module. That way, they don’t need to know about
-- the internal details of our module, and we can change those details when-
-- ever we want, as long as the functions that we export act the same.

-- ## Record syntax
--
-- Lets create a data type that describes a Person
--
-- ### The not so smart way
--
data Person = Person String String Int Float String String deriving (Show)

-- You can now create a person by:
-- ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
--
-- ghci> guy
-- Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- You can see that this representation is not so smart by observing following functions

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

-- You can test the following code with
-- ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- ghci> firstName guy
-- "Buddy"
-- ghci> height guy
-- 184.2
-- ghci> flavor guy
-- "Chocolate"

-- ### The much better way

data Person' = Person'
  { firstName' :: String,
    lastName' :: String,
    age' :: Int,
    height' :: Int,
    phoneNumber' :: String,
    flavor' :: String
  }
  deriving (Show)

-- The main benefit of us- ing this syntax is that it creates functions that look
-- up fields in the data type. By using record syntax to create this data type,
-- Haskell automatically makes these functions: firstName , lastName , age ,
-- height , phoneNumber , and flavor . Take a look:
--
-- ghci> :t flavor
-- flavor :: Person -> String
--
-- ghci> :t firstName
-- firstName :: Person -> String

-- Again, with Car - The not so good way
data Car = Car String String Int deriving (Show)

-- ghci> Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967

-- The better way
data Car' = Car'
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

-- ghci> Car {company="Ford", model="Mustang", year=1967}
-- Car {company = "Ford", model = "Mustang", year = 1967}
