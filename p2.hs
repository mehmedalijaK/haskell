factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial a = a * factorial (a-1)

addVectors :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
addVectors (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)

-- lenght' :: [a] -> Int
-- lenght' [] = 0
-- lenght' (x:xs) = 1 + lenght' xs 

