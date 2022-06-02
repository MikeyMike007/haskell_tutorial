-- # Derived instances
--
-- - A type class is a sort of an interface that defines some behavior, and that
--   a type can be made an instance of a type class if iots suppeorts that
--   behaviour
--
-- - For example, the `Int` type is an instance of the `Eq` type class because
--   the `Eq` type class defines behaviour for stuff that can be equated.
--
-- - Haskell type classes are often confused with classes in languages like
--   Java, Python, C++ and the like, which trips up a lot of programmers. In those
--   languages, classes are a blueprint from which we create objects that can do
--   some actions. But we don’t make data from Haskell type classes. Instead, we
--   first make our data type, and then we think about how it can act. If it can act
--   like something that can be equated, we make it an instance of the Eq type
--   class. If it can act like something that can be ordered, we make it an instance
--   of the Ord type class.
--
-- ## Making a Person a instance of the `Eq` type class

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Eq)

-- ghci> mikeId = Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- ghci> adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
-- ghci> mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

-- ghci> mca == adRock
-- False
--
-- ghci> mikeD == adRock
-- False
--
-- ghci> mikeD == mikeD
-- True
--
-- ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- True

-- ghci> let beastieBoys = [mca, adRock, mikeD]
--
-- ghci> mikeD `elem` beastieBoys
-- True

-- ## Show me how to read
--
-- Lets make Person a instance of the `Eq`, `Show` and `Read` type classes

data Person' = Person' {firstName' :: String, lastName' :: String, age' :: Int} deriving (Eq, Show, Read)

-- ghci> mikeId = Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
-- ghci> adRock = Person {firstName' = "Adam", lastName' = "Horovitz", age' = 41}
-- ghci> mca = Person {firstName' = "Adam", lastName' = "Yauch", age' = 44}
--
-- ghci> mikeD
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}
--
-- ghci> "mikeD is: " ++ show mikeD
-- "mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
--
-- mysteryDude = "Person' { firstName' =\"Michael\"" ++ ", lastName' =\"Diamond\"" ++ ", age' = 43}"
-- ghci> read mysteryDude :: Person
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}
--
-- ghci> read mysteryDude == mikeD
-- True
--
-- ghci> read "Just 3" :: Maybe Int
-- Just 3

-- ## Order in the court
--
-- We can derive instances for the Ord type class, which is for types that have
-- values that can be ordered.
data Bool' = False' | True' deriving (Eq, Ord)

-- ghci> True' `compare` False'
-- GT
--
-- ghci> True' > False'
-- True
--
-- ghci> True' < False'
-- False

-- If two values were made using the same constructor, they are considered to
-- be equal, unless they have fields. If they have fields, the fields are com-
-- pared to see which is greater. (Note that in this case, the types of the
-- fields also must be part of the Ord type class.) In the Maybe a data type,
-- the Nothing value constructor is specified before the Just value
-- constructor, so the value of Nothing is always smaller than the value of
-- Just something, even if that something is minus one billion trillion. But if
-- we specify two Just values, then it will compare what’s inside them.

-- ghci> Nothing < Just 100
-- True
-- ghci> Nothing > Just (-49999)
-- False
-- ghci> Just 3 `compare` Just 2
-- GT
-- ghci> Just 100 > Just 50
-- True

-- ## Any day of the week
--
-- The Enum type class is for things that have predeccessors and successors
-- The Bounded type class is for things that have a lowest possible value and highest possible value

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- ghci> Wednesday
-- Wednesday
--
-- ghci> show Wednesday
-- "Wednesday"
--
-- ghci> read "Saturday" :: Day
-- Saturday
--
-- ghci> Saturday == Sunday
-- False
--
-- ghci> Saturday == Saturday
-- True
--
-- ghci> Saturday > Friday
-- True
--
-- ghci> Monday `compare` Wednesday
-- LT
--
-- ghci> minBound :: Day
-- Monday
--
-- ghci> maxBound :: Day
-- Sunday
--
-- ghci> succ Monday
-- Tuesday
--
-- ghci> pred Saturday
-- Friday
--
-- ghci> [Thursday .. Sunday]
-- [Thursday,Friday,Saturday,Sunday]
--
-- ghci> [minBound .. maxBound] :: [Day]
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
