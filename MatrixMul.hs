module MatrixMul where

mulRow:: Int-> [[Int]] -> [[Int]] ->[Int]
mulRow n a b= [sum [a!!n!!j*b!!i!!j|j<-[0..length a-1]]|i<-[0..length a-1]]++mulRow (n-1) a b

mul :: Int->[[Int]]->[[Int]]->[[Int]]
mul n a b = [(mulRow n a b)] ++ [(mulRow (n-1) a b)]
mul 1 _ _ = [[]]
mul (-1) _ _ = [[]]

matMul :: [[Int]] -> [[Int]]-> [[Int]]
matMul a b= mul (length a-1) a b 