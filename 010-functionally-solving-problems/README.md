# Functionally solving problems

## Reverse Polish Notation Calculator

Lets create a calculator that can handle syntax like `3 4 + = 7`,

```haskell
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where
    -- Syntax: foldingFunction currentStack currentItem
    foldingFunction (x : y : ys) "*" = (y * x) : ys
    foldingFunction (x : y : ys) "+" = (y + x) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction (x : y : ys) "/" = (y / x) : ys
    foldingFunction (x : y : ys) "^" = (y ** x) : ys
    foldingFunction (x : xs) "ln" = log x : xs
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString : xs
```

Lets see what is happening when the program solves for `solveRPN 10 4 3 + 2 * -`

```bash
0)  words "10 4 3 + 2 * -" = ["10", "4", "3", "+", "2", "*", "-"]
1)  stack: [] - item: "10"
2)  stack: [10] - item: "4"
3)  stack: [4, 10] - item "3"
4)  stack: [3, 4, 10] - item "+"
5)  stack: [7, 10] - item "2"
6)  stack [2, 7, 10] - item "*"
7)  stack [14, 10] - item "-"
8)  stack [-4]
```

## Shortest path algorithm

```haskell
import Data.List ()

-- # Shortest path

-- Data type for road system
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

type RoadSystem = [Section]

heaththrowToLondon :: RoadSystem
heaththrowToLondon =
  [ Section 50 10 30,
    Section 5 90 20,
    Section 40 2 25,
    Section 10 8 0
  ]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  -- Calculate current times for the different paths
  let timeA = sum (map snd pathA)
      timeB = sum (map snd pathB)
      -- Calculate the times to the next Section by walking one step direct forward
      forwardTimeToA = timeA + a
      forwardTimeToB = timeB + b
      -- Calculate the times to the next Section by walking straight and then crossing
      crossTimeToA = timeB + b + c
      crossTimeToB = timeA + a + c
      -- Estimate the new paths - Please note that we are prepending just out of effeciency. We need to reverse the lists later
      newPathToA =
        if forwardTimeToA <= crossTimeToA
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB

      newPathToB =
        if forwardTimeToB <= crossTimeToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
   in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  -- 1) Estimate both the best A and B path
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
   in -- 2) Compare the total times for the two best paths
      if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then -- 3) We need to reverse since:
        --
        --    Why do we prepend instead of doing pathA ++ [(A, a)] ? Well, adding an
        --    element to the beginning of a list is much faster than adding it to the end.
        --    This means that the path will be the wrong way around once we fold over a list
        --    with this function, but it’s easy to reverse the list later.
          reverse bestAPath
        else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main :: IO ()
main = do
  -- 1) Get the contents from input
  contents <- getContents
  -- 2) Put the lines into a list
  -- 3) Transform it from a string to a number with `read`
  -- 4) Go through the list and group the items based on the 3 bordering items in the main list
  let threes = groupsOf 3 (map read $ lines contents)
      -- 5) Transform the list of lists into a Section type
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      -- 6) Get the optimal path from the roadSystem that consists of many section types
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathTime = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show pathTime

```
