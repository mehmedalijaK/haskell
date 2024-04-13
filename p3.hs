
test'' :: Int -> Int -> String
test'' x y
        | x > y = "x vece"
        | x < y = "y vece"
        | otherwise = "isti su"

initials :: String -> String -> String
initials firstName lastName = ime ++ ": " ++ [f] ++ ". " ++ [l] ++ "."
        where ime = firstName ++ " " ++ lastName
              (f:_) = firstName
              (l:_) = lastName

cylinder :: Float -> Float -> Float
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2*topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > max = x
    | otherwise = max
    where max = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
