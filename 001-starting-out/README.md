# Starting out

## Arithmetic

Following shows some examples on how arithmetic can be done in Haskell.

```haskell
ghci> 2 + 15
17

ghci> 49 * 100
4900

ghci> 1892 - 1472
420

ghci> 5 / 2
2.5
```

```haskell
ghci> (50 * 100) - 4999
1

ghci> 50 * 100 - 4999
1

ghci> 50 * (100 - 4999)
-244950
```

## Booleans

Following shows some boolean syntax in Haskell.

```haskell
ghci> True && False
False

ghci> True && True
True

ghci> False || True
True

ghci> not False
True

ghci> not (True && True)
False
```

```haskell
ghci> 5 == 5
True

ghci> 1 == 0
False

ghci> 5 /= 5
False

ghci> 5 /= 4
True

ghci> "hello" == "hello"
True
```

## Functions

Following illustrates some function examples in Haskell.

`succ` increases a number with 1.

```haskell
ghci> succ 8
9
```

`min` returns the smallest number of two.

`max` returns the largest number of two.

```haskell
ghci> min 9 10
9
ghci> min 3.4 3.2
3.2
ghci> max 100 101
101
```

```haskell
ghci> succ 9 + max 5 4 + 1
16
ghci> (succ 9) + (max 5 4) + 1
16
```

## Function defined in a file

Create `baby.hs` and insert

```haskell
doubleMe x = x + x
```

Then load the file / function in `ghci` by typing `:l baby`.

Then, you can use the `doubleMe` function in `ghci`

```haskell
ghci> doubleMe 9
18

ghci> doubleMe 8.3
16.6
```

Now insert another function `doubleUs` in `baby.hs`.

```haskell
doubleUs x y = x*2 + y*2
```

Load the modules with `ghci> :l baby`. Then you can run the function again with,

```haskell
    ghci> doubleUs 4 9
    26
    ghci> doubleUs 2.3 34.2
    73.0
    ghci> doubleUs 28 88 + doubleMe 123
    478
```

Now, lets add some more functions and load them

```haskell
doubleUs2 x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "Its a-me, Conan O Brien"
```

You can run these functions in a similar way as earlier.

## Lists

```haskell
ghci> let lostNumbers = [4,8,15,16,23,42]

ghci> lostNumbers
[4,8,15,16,23,42]
```

The function / operator `++` concatenates lists and strings.

```haskell
ghci> [1,2,3,4] ++ [9,10,11,12]
[1,2,3,4,9,10,11,12]

ghci> "hello" ++ " " ++ "world"
"hello world"

ghci> ['w','o'] ++ ['o','t']
"woot"
```

```haskell
ghci> 'A':" SMALL CAT"
"A SMALL CAT"

ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]
```

The operator `!!` extracts one element out from a list or string.

```haskell
ghci> "Steve Buscemi" !! 6
'B'

ghci> [9.4,33.2,96.2,11.2,23.25] !! 1
33.2
```

You can also use the `:` operator as well as the `++` operator to concatenate or add to a list or string.

```haskell
ghci> let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b ++ [[1,1,1,1]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
ghci> [6,6,6]:b
[[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b !! 2
[1,2,2,3,4]
```

Some logical examples when applied to lists.

```haskell
ghci> [3,2,1] > [2,1,0]
True
ghci> [3,2,1] > [2,10,100]
True
ghci> [3,4,2] > [3,4]
True
ghci> [3,4,2] > [2,4]
True
ghci> [3,4,2] == [3,4,2]
True
```

`head` takes a list and returns its head. The head of a list is basically its first element.

```haskell
ghci> head [5,4,3,2,1]
5
```

`tail` takes a list and returns its tail. In other words, it chops off a list's head and returns the rest.

```haskell
ghci> tail [5,4,3,2,1]
[4,3,2,1]
```

`last` takes a list and returns its last element.

```haskell
ghci> last [5,4,3,2,1]
1
```

`init` takes a list and returns everything except its last element.

```haskell
ghci> init [5,4,3,2,1]
[5,4,3,2]
```

`length` takes a list and returns its length.

```haskell
ghci> length [5,4,3,2,1]
5
```

`null` checks if a list is empty and returns `True` if that is the case. Use this function instead of xs == [] (if you have a list called xs)

```haskell
ghci> null [1,2,3]
False
ghci> null []
True
```

`reverse` reverses a list.

```haskell
ghci> reverse [5,4,3,2,1]
[1,2,3,4,5]
```

`take` takes number and a list. It extracts that many elements from the beginning of the list.

```haskell
ghci> take 3 [5,4,3,2,1]
[5,4,3]
ghci> take 1 [3,9,3]
[3]
ghci> take 5 [1,2]
[1,2]
ghci> take 0 [6,6,6]
[]
```

`drop` works in a similar way, only it drops the number of elements from the beginning of a list

```haskell
ghci> drop 3 [8,4,2,1,5,6]
[1,5,6]
ghci> drop 0 [1,2,3,4]
[1,2,3,4]
ghci> drop 100 [1,2,3,4]
[]
```

`maximum` returns biggest element in a list.

`minimum` returns the smallest.

```haskell
ghci> minimum [8,4,2,1,5,6]
1
ghci> maximum [1,9,2,3,4]
9
```

`sum` takes a list of numbers and returns their sum.

`product` takes a list of numbers and returns their product.

```haskell
ghci> sum [5,2,1,6,3,2,5,7]
31
ghci> product [6,2,1,2]
24
ghci> product [1,2,5,6,7,9,2,0]
0
```

`elem` checks wether an item exists in a list.

```haskell
ghci> 4 `elem` [3,4,5,6]
True
ghci> 10 `elem` [3,4,5,6]
False
```

## Texas ranges

You can create ranges of both numbers and characters.

```haskell
ghci> [1..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
ghci> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> ['K'..'Z']
"KLMNOPQRSTUVWXYZ"
```

You can also create ranges with different patterns.

```haskell
ghci> [2,4..20]
[2,4,6,8,10,12,14,16,18,20]
ghci> [3,6..20]
[3,6,9,12,15,18]
```

```haskell
ghci> [0.1, 0.3 .. 1]
[0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
```

## Infinite lists

`cycle` takes a list and cycles it into an infinite list.

```haskell
ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
ghci> take 12 (cycle "LOL ")
"LOL LOL LOL "
```

`repeat` takes an element and produces an infinite list of just that element.

```haskell
ghci> take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]
```

## List comprehensions

Some examples.

```haskell
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```

```haskell
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
```

```haskell
ghci> [ x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]

```

```haskell
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
```

```haskell
ghci> boomBangs [7..13]
["BOOM!","BOOM!","BANG!","BANG!"]
```

```haskell
ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]
```

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
[55,80,100,110]
```

```haskell
ghci> let nouns = ["hobo","frog","pope"]
ghci> let adjectives = ["lazy","grouchy","scheming"]
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",
"grouchy pope","scheming hobo","scheming frog","scheming pope"]
```

```haskell
length' xs = sum [1 | _ <- xs]
```

```haskell
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

```haskell
ghci> removeNonUppercase "Hahaha! Ahahaha!"
"HA"
ghci> removeNonUppercase "IdontLIKEFROGS"
"ILIKEFROGS"
```

```haskell
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

## Tuples

Unlike a list, a tuple can contain a combination of several types.

`fst` returns the first element of a tuple with two elements.

```haskell
ghci> fst (8,11)
8

ghci> fst ("Wow", False)
"Wow"
```

`snd` returns the second element of a tuple with two elements.

```haskell
ghci> snd (8,11)
11

ghci> snd ("Wow", False)
False
```

`zip` "zipps" two lists into a list of tuples.

```haskell
ghci> zip [1,2,3,4,5] [5,5,5,5,5]
[(1,5),(2,5),(3,5),(4,5),(5,5)]


ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
```

```haskell
ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
[(5,"im"),(3,"a"),(2,"turtle")]
```

```haskell
ghci> let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
```

```haskell
ghci> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
```

```haskell
ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
ghci> rightTriangles'
[(6,8,10)]
```
