-- interact takes a function of type String -> String as a parameter and returns
-- an I/O action that will take some input, run that function on it, and then
-- print out the function’s result.

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line <= 10) . lines
