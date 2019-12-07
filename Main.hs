module Main where
import MazeRunner
import Collatz
import UnlimitedGameOfLife
import LongestPalindrome

spinWords :: String -> String
spinWords str = unwords [if length x<5 then x else reverse x| x<- words str ]

grid :: [[Int]]
grid= [
    [1,0,0,1,1,1,0,1],
    [1,0,1,1,0,0,1,1],
    [1,1,0,0,1,1,0,1],
    [1,0,1,0,1,0,1,0],
    [1,1,1,1,1,0,1,0]]
main :: IO ()
main = do
    --putStrLn $ show $ getGeneration grid 1
    putStrLn $ show $ longestPalindrome "baablkj12345432133d"
    
    
