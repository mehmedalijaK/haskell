import Distribution.Simple.Utils (xargs)



myFoldL :: (a-> b-> a) -> a -> [b] -> a
myFoldL f acc [] = acc
myFoldL f acc (x:xs) = myFoldL f (f acc x) xs


myFoldR :: (b-> a-> a) -> a -> [b] -> a
myFoldR f acc [] = acc
myFoldR f acc (x:xs) = f x (myFoldR f acc xs)








myFoldLeft :: (a -> b -> a) -> a -> [b] -> a
myFoldLeft f acc [] = acc
myFoldLeft f acc (x:xs) = myFoldLeft f (f acc x) xs

myFoldRight :: (b -> a -> a) -> a -> [b] -> a
myFoldRight f acc [] = acc
myFoldRight f acc (x:xs) = f x (myFoldRight f acc xs)


myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x: xs) = (f x) : (myMap f xs)


nTh :: (Num b, Eq b) => [a] -> b -> a
nTh [] _ = error "something went wrong"
nTh (x:xs) 0 = x
nTh (x:xs) n = nTh xs (n-1)

myLen :: [a] -> Int
myLen [] = 0
myLen (_:xs) = succ $ myLen xs

