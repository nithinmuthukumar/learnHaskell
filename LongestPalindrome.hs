module LongestPalindrome where
--bruteForce
longestPalindrome :: Eq a => [a] -> Int
longestPalindrome p = if reverse p == p || length p <=1 
    then length p 
    else max (longestPalindrome (tail p)) (longestPalindrome (init p))
--dynamic programming
