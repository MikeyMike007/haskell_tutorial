-- # A yes no type class

-- - In javascript, all these expressions will throw an alert of NO!
--
--   if (0) alert("YEAH!") else alert("NO!")
--   if ("") alert ("YEAH!") else alert("NO!")
--   if (false) alert("YEAH!") else alert("NO!")
--
-- - This will throw an alert of Yes! since javascript considers any nonempty
--   string to be a true value:
--
--   if ("WHAT") alert ("YEAH!") else alert("NO!")
--
-- - Lets now try to implement this in Haskell

-- - Class declaration:

class YesNo a where
  yesno :: a -> Bool

-- - Some instance definitions

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id -- Id is just a standard library function that takes the same parameter and returns the same thing

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

-- - Some examples

--   ghci> yesno $ length []
--   False
--   ghci> yesno "haha"
--   True
--   ghci> yesno ""
--   False
--   ghci> yesno $ Just 0
--   True
--   ghci> yesno True
--   True
--   ghci> yesno EmptyTree
--   False
--   ghci> yesno []
--   False
--   ghci> yesno [0,0,0]
--   True
--   ghci> :t yesno
--   yesno :: (YesNo a) => a -> Bool
--

-- Function that mimics the if else statement

yesnoIf :: (YesNo b) => b -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
