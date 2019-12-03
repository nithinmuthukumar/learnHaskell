module UnlimitedGameOfLife where

getRangeX :: [(Int,Int)]-> [Int]
getRangeX cells= [minimum ls.. maximum ls] where ls=[x | (x,y) <- cells]
getRangeY :: [(Int,Int)] ->[Int]
getRangeY cells= [minimum ls.. maximum ls] where ls=[y | (x,y) <- cells]
genGrid :: [(Int,Int)] ->Int-> [[Int]]
genGrid cells=[[if (x,y) `elem` cells then 1 else 0 | x<-getRangeX cells]|y<-getRangeY cells]
 
--finish life function
life :: [(Int,Int)] -> (Int,Int) -> Int
life cells pos=  

step :: [(Int,Int)]->[(Int,Int)]
step cells = [life cells (x,y) | x<-getRangeX cells,y<-getRangeY cells]   

getCells :: [[Int]]-> [(Int,Int)]
getCells grid = [(x,y) | x<-length grid!!0, y<- length grid,grid!!y!!x==1]

gol :: [(Int,Int)] -> Int -> [[Int]]
gol cells n = gol (step cells) n-1
gol cells 0 = genGrid cells

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration grid n= gol (getCells grid) n

