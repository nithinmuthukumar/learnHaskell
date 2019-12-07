module UnlimitedGameOfLife where

getRangeX :: [(Int,Int)]-> [Int]
getRangeX cells= [minimum ls.. maximum ls] where ls=[x | (x,y) <- cells]

getRangeY :: [(Int,Int)] ->[Int]
getRangeY cells= [minimum ls.. maximum ls] where ls=[y | (x,y) <- cells]

genGrid :: [(Int,Int)]-> [[Int]]
genGrid cells=[[if (x,y) `elem` cells then 1 else 0 | x<-getRangeX cells]|y<-getRangeY cells]
 
isNeighbour :: (Int,Int) -> (Int,Int) -> Bool
isNeighbour (x1,y1) (x2,y2) = abs (x1-x2)<1&&abs (y1-y2)<1
--finish life function
life :: [(Int,Int)] -> (Int,Int) -> Bool
life cells (x,y)  
    | live&&neighbours>1&&neighbours<4 = False
    | not live&&neighbours==3 = False
    | otherwise = True
    where 
        neighbours= length [c | c<-cells, isNeighbour (x,y) c]
        live = (x,y) `elem` cells

step :: [(Int,Int)]->[(Int,Int)]
step cells = [(x,y)  | x<-getRangeX cells,y<-getRangeY cells,life cells (x,y)]   

getCells :: [[Int]]-> [(Int,Int)]
getCells grid = [(x,y) | x<-[0..(length $ grid!!0)-1], y<- [0..(length grid)-1],grid!!y!!x==1]

gol :: [(Int,Int)] -> Int -> [[Int]]
gol cells 0 = genGrid cells
gol cells n = gol (step cells) (n-1)


getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration grid n= gol (getCells grid) n
