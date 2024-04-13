import System.Posix (accessModes)
select_first = \x -> \y -> x
select_second = \x -> \y -> y
true = \x -> \y -> x
false = \x -> \y -> y

make_pair = \x -> \y -> \fun -> ((fun x) y)

my_not = \x -> ((x false) true)
my_and = \x -> \y -> ((x y) false)
my_or = \x -> \y -> ((x true) y)
my_xor = \x -> \y -> ((x (not y)) y)
my_nand = \x -> \y -> ((x (not y)) true)
my_eq = \x -> \y -> ((x y) (not y))
my_imp = \x -> \y -> ((x y) true)

palindrom :: String -> Bool
palindrom [] = True
palindrom [_] = True
palindrom (x:xs) = x == last xs && palindrom (init xs)

findKthElement :: (Num n, Eq n, Ord n) => [a] -> n -> a
findKthElement [] _ = error "too long"
findKthElement (x:xs) n
            | n == 1 = x 
            | otherwise = findKthElement xs (n - 1)


findKthElement' :: (Num n, Eq n, Ord n) => [a] -> n -> a
findKthElement' (x:_) 1 = x
findKthElement' [] _ = error "index out of bounds"
findKthElement' (x:xs) n
            | n < 1     = error "Index out of bounds"
            | otherwise = findKthElement' xs (n - 1)


foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft f acc [] = acc
foldLeft f acc (x:xs) = foldLeft f (f acc x) xs


myLength :: Num b => [a] -> b
myLength [] = 0
myLength x = foldNumberElement (\x y-> x + 1) 0 x

foldNumberElement :: (a -> b -> a) -> a -> [b] -> a
foldNumberElement f acc [] = acc
foldNumberElement f acc (x:xs) = foldNumberElement f (f acc x) xs