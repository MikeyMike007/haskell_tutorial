-- Another example with interact

main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs then "Is palindrome" else "is not palindrome") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs
