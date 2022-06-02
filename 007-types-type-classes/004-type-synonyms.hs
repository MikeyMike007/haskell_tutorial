import qualified Data.Map as Map

-- # Type Synonyms
--
-- - [Char] and Strings are equivalent and interchangeble. Thats implemented with type synonyms
-- - Its definition is:
-- - type String = [Char]

-- ## Making our Phonebook prettier

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

-- ## Parameterizing type synonyms
--
-- - Type synonyms can be parameterized
-- - If we want a type that represents an association list type, but still want
--   it to be general so it can use any type as the keys and values. We can define that in following way:

type AssocList k v = [(k, v)]

-- - Now a function that gets the value by a key in an association list can have
--   the type (Eq k) => k -> AssocList k v -> Maybe v
--
-- - AssocList is a type constructor that takes two types and produces a
--   concerete type - for instance AssocList Int String
--
-- -  Just as we can partially apply functions to get new functions, we can par-
--    tially apply type parameters and get new type constructors from them. When
--    we call a function with too few parameters, we get back a new function. In
--    the same way, we can specify a type constructor with too few type parame-
--    ters and get back a partially applied type constructor. If we wanted a type
--    that represents a map (from Data.Map ) from integers to something, we could
--    do this:

type IntMap v = Map.Map Int v

-- Or (the same thing)

type IntMap' = Map.Map Int

-- Either way, the IntMap type constructor takes one parameter, and that is the
-- type of what the integers will point to

-- ## Go left, then right

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- - The data type has two value constructors
-- - If Left is used, then its contents are of type a
-- - If Right is used, then its contents are of type b
--
-- Some examples
--   ghci> Right 20
--   Right 20
--   ghci> Left "w00t"
--   Left "w00t"
--   ghci> :t Right 'a'
--   Right 'a' :: Either a Char
--   ghci> :t Left True
--   Left True :: Either Bool b
--

-- So far, you’ve seen Maybe a mostly used to represent the results of com-
-- putations that could have failed. But sometimes, Maybe a isn’t good enough,
-- because Nothing doesn’t convey much information other than that something
-- has failed. That’s fine for functions that can fail in only one way, or if we’re
-- not interested in how or why they failed. For instance, a Data.Map lookup fails
-- only if the key wasn’t in the map, so we know exactly what happened.

-- However, when we’re interested in how or why some function failed,
-- we usually use the result type of Either a b , where a is a type that can tell us
-- something about the possible failure, and b is the type of a successful compu-
-- tation. Hence, errors use the Left value constructor, and results use Right
--
-- See example

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

-- Lets test it:
--
-- ghci> lockerLookup 101 lockers
-- Right "JAH3I"
-- ghci> lockerLookup 100 lockers
-- Left "Locker 100 is already taken!"
-- ghci> lockerLookup 102 lockers
-- Left "Locker number 102 doesn't exist!"
-- ghci> lockerLookup 110 lockers
-- Left "Locker 110 is already taken!"
-- ghci> lockerLookup 105 lockers
-- Right "QOTSA"
--
-- We could have used a Maybe a to represent the result, but then we
-- wouldn’t know why we couldn’t get the code. But now we have informa-
-- tion about the failure in our result type.
--
