-- # Reverse Polish Notation Calculator
-- - 3 4 + is equal to 3 + 4

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

-- Example: solveRPN "10 4 3 + 2 * -"
--
-- 0) ["10", "4", "3", "+", "2", "*", "-"]
-- 1) stack: [] - item: "10"
-- 2) stack: [10] - item: "4"
-- 3) stack: [4, 10] - item "3"
-- 4) stack: [3, 4, 10] - item "+"
-- 5) stack: [7, 10] - item "2"
-- 6) stack [2, 7, 10] - item "*"
-- 7) stack [14, 10] - item "-"
-- 8) stack [-4]
