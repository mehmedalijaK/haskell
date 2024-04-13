
doubleMe :: Int -> Int
doubleMe x = x * x

len' :: [a] -> Int
len' xs = sum [1 | x <- xs]
-- moze i len' xs = sum [1| _ <- xs], zato sto nigde ne koristimo x

removeNotUpper :: String -> String
removeNotUpper st = [c | c <- st, c `elem` ['A' .. 'Z']] 

even' :: [Int] -> [Int]
even' a = [n | n <- a, even n]

pitagora :: Int -> [(Int, Int, Int)]
pitagora x = [(a, b, c) | a <- [1..x], b <-[1..x], c <- [1..x], a^2 + b^2 == c^2, a < b]

