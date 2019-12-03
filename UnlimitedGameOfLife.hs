module UnlimitedGameOfLife where

genGrid :: [(Int,Int)] -> [[Int]]
genGrid cells = 

getCells :: [[Int]]-> [(Int,Int)]
getCells grid = [(x,y) | x<-length grid!!0, y<- length grid,grid!!y!!x==1]

gol :: [(Int,Int)] -> Int -> [[Int]]
gol cells n = gol [] n-1
gol cells 0 = genGrid cells

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration grid n= gol (getCells grid) n

