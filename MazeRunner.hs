module MazeRunner where 
import Data.Maybe
valid :: [[Int]] -> Int -> Int -> Bool
valid maze x y = 0<=x&&x<length (maze!!0) &&0<=y&&y<length maze&&(maze!!y)!!x/=1

findStart :: [[Int]] -> (Int,Int)
findStart maze = [(x,y) | x<-[0..(length (maze!!0))-1],y<-[0..(length maze)-1],maze!!y!!x==2]!!0

nextPos :: Char -> Int -> Int -> (Int,Int)
nextPos d x y 
    | d=='N' = (x,y-1)
    | d=='S' = (x,y+1)
    | d=='W' = (x-1,y)
    | d=='E' = (x+1,y)


run :: [[Int]] -> [Char] -> (Int,Int) -> Maybe (Int,Int)
run maze [] (x,y) = if valid maze x y then Just (x,y) else Nothing
run maze (d:ds) (x,y)=
    if valid maze x y 
        then 
            if maze!!y!!x==3
                 then run maze [] (x,y)
            else run maze ds (nextPos d x y)
    else Nothing

mazeRunner :: [[Int]] -> [Char] -> [Char]
mazeRunner maze directions = do
    let finalPos = run maze directions (findStart maze)
    if isNothing finalPos
        then "Dead"
    else if maze!!(snd (fromJust finalPos))!!(fst (fromJust finalPos))==3
        then "Finish"
        else "Lost"

maze :: [[Int]]
maze = [[1,1,1,1,1,1,1],
        [1,0,0,0,0,0,2],
        [1,0,1,0,1,0,0],
        [0,0,1,0,0,0,1],
        [1,0,1,0,1,0,1],
        [1,0,0,0,0,0,1],
        [1,0,1,0,1,0,1]]