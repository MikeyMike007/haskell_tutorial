-- # Recursive Data Structures

-- - A constructor in an algebraic data type can have several fields and each
--   field must be of some concrete type
--
-- - This means that we can make types that have themselves as types in their
--   fields i.e. recursive data types where one value of some type contains
--   values of that type, which in turn contains more values of the same type.
--
-- - [5] = 5 : []
-- - [4, 5] = 4 : (5 : []) = 4 : 5 : [] (Since : is right-associative)
-- - [3, 4, 5, 6] = 3 : ( 4 : (5 : (6 : []))) = 3 : 4 : 5 : 6 : [] (Since : is right-associative)
--
-- - The examples above shows that : is actually a constructor that takes a
--   value and another list and returns a list

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- Lets look at some examples:

-- ghci> Empty
-- Empty
-- ghci> 5 `Cons` Empty
-- Cons 5 Empty
-- ghci> 4 `Cons` (5 `Cons` Empty)
-- Cons 4 (Cons 5 Empty)
-- ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
-- Cons 3 (Cons 4 (Cons 5 Empty))

-- - We called our Cons constructor in an infix manner so you can see how
--   it’s just like :. Empty is like [] , and 4 `Cons` (5 `Cons` Empty) is like 4:(5:[]) .

--   ## Improving our list

-- - We can define functions to be automatically infix by naming them using only
--   special characters. We can also do the same with constructors, since they’re
--   just functions that return a data type. There is one restriction how- ever:
--   Infix constructors must begin with a colon. So check this out:

-- fixity declaration
-- States how tigtly the operator binds and whether its left-assoiciative or right-associative
infixr 5 :-:

-- - As an example
-- - The * operator fixity is infixl 7 *
-- - This means that 5 * 4 * 3 * 2 * 1 = ((((5 * 4) * 3) * 2) * 1)

-- - The + operator fixity is infixl 6 +
-- - This means that 5 + 4 + 3 + 2 + 1 = ((((5 + 4) + 3) + 2) + 1)
--
-- - PLEASE NOTE that the * operator binds tighter (fixity 7) compared to the operator + (fixity 6)
--   This means that 5 * 4 + 3 = ((5 * 4) + 3) = 20 + 3 = 23
data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

-- Some examples
-- ghci> 3 :-: 4 :-: 5 :-: Empty
-- 3 :-: (4 :-: (5 :-: Empty))
-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty
-- ghci> 100 :-: a
-- 100 :-: (3 :-: (4 :-: (5 :-: Empty)))
--

-- - Now, we can make a function that adds two lists together. We can find
--   inspiration from the function ++ that adds lists together. Just to get it to compile, ill change ++ to ??

infixr 5 ??

(??) :: [a] -> [a] -> [a]
[] ?? ys = ys
(x : xs) ?? ys = x : (xs ?? ys)

-- Now we can use this for our list

infixr 5 ^++

(^++) :: List' a -> List' a -> List' a
Empty' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-- Examples
-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty
-- ghci> let b = 6 :-: 7 :-: Empty
-- ghci> a ^++ b
-- 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))

-- ## Lets plant a tree

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

-- Running this gives:
--
-- ghci> let nums = [8,6,4,1,7,3,5]
-- ghci> let numsTree = foldr treeInsert EmptyTree nums
-- ghci> numsTree

-- Node 5
--
--   (Node 3
--     (Node 1 EmptyTree EmptyTree)
--     (Node 4 EmptyTree EmptyTree))
--
--   (Node 7
--     (Node 6 EmptyTree EmptyTree)
--     (Node 8 EmptyTree EmptyTree))

-- - Please note:  In this foldr, treeInsert is the folding binary function (it
--   takes a tree and a list element and produces a new tree), and EmptyTree is
--   the starting accu- mulator. nums, of course, is the list we’re folding over.

-- Testing the `treeElem` function gives:
--
-- ghci> 8 `treeElem` numsTree
-- True
--
-- ghci> 100 `treeElem` numsTree
-- False
--
-- ghci> 1 `treeElem` numsTree
-- True
--
-- ghci> 10 `treeElem` numsTree
-- False
