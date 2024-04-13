-- - Vigenere cipher
--         - Uzima kljuc(string) i tekst i vraca enkriptovan tekst
--         - Uzima kljuc(string) i tekst i vraca dekriptovan tekst
import Data.Char (ord, chr)
import Distribution.Simple.Flag (BooleanFlag)


encoder' :: String -> String -> String
encoder' [x] _ = [x]
encoder' x lst@[y] = encoder' x lst
encoder' (x:xs) lst@(y:ys) = (if ord res > 90 then chr(ord res `mod` 90 + 64) else res) : encoder' xs ys
                where res = chr (ord x + (ord y - ord 'A'))
                      

--  Napisati f-ju koja uzima lisu parcijalnih funkcija i listu parametara
--       i vraca listu koja sadrzi rezultate primene svake funkcije na svaki argument

-- max 1 max 2 max 3          ->   1 2 3

myMap :: (a->b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFoldLeft :: a -> [b] -> (a->b->a) -> a
myFoldLeft acc [] _ = acc
myFoldLeft a (x:xs) f = myFoldLeft (f a x) xs f

myFoldRight :: (b->a->a) -> a -> [b] -> a
myFoldRight _ acc [] = acc
myFoldRight f acc (b:bs) = myFoldRight f (f b acc) bs


palindrom :: String -> Bool
palindrom x = x == reverse x

palindrom' :: String -> Bool
palindrom' [] = True
palindrom' [_] = True
palindrom' list@(x:xs) = (x == last xs) && palindrom' (init xs)


    -- - Napisati funkciju koja uzima broj kao parametar i pronalazi sve proste brojeve
    --   manje od tog broja

checkPrime :: Int -> Bool
checkPrime x = null ([c | c <- [2 .. floor (sqrt (fromIntegral x))], x `mod` c == 0])

prostBroj :: Int -> [Int]
prostBroj 1 = [1]
prostBroj x = if checkPrime x then x : prostBroj (x-1) else prostBroj(x-1)


    -- - Napisati funkciju koja vraca pretposlednji element liste

lastBefore :: [a] -> a
lastBefore (x:y:[]) = x
-- lastBefore [x,y] = x 
lastBefore (x:xs) = lastBefore xs


calculate :: Fractional a => [a] -> (a, a)
calculate a = (calculateFoldl (\acc -> \x -> acc + x) 0 a 0 ,2)

calculateFoldl :: Fractional a => (a->b->a) -> a -> [b] -> a -> a
calculateFoldl f acc [] len = acc / len
calculateFoldl f acc (b:bs) len = calculateFoldl f (f acc b) bs $ len + 1


reverseList' :: [a] -> [a]
reverseList' [] = []
reverseList' (x:xs) = reverseList' xs ++ [x]


secondPalindrom :: String -> Bool
secondPalindrom [] = True
secondPalindrom [_] = True
secondPalindrom (x:xs) = x == last xs && secondPalindrom (init xs)



myCompress :: [a] -> [a]
myCompress [] = []
myCompress list@(x:xs) = x : myCompress xs

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs