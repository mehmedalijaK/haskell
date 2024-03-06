{-  Tipovi

    Type signature
        - addThree :: Int -> Int -> Int -> Int
    Type annotation
        - x = 3 :: Int
    
    Type variable
        - polimorfne funkcije
        - head :: [a] -> a
        - fst :: (a, b) -> a
        - map :: (a -> b) -> [a] -> [b]

-}

{-  Klase tipova (Typeclass)

    Najpribliznije interfejsima kod OOP
    
    Class constraint
        - (==) :: (Eq a) => a -> a -> Bool
    
    Primeri
        - Eq - jednakost
        - Ord - poredjenje
        - Show - mogu da se predstave kao string
        - Read - mogu da se procitaju iz stringa
        - Enum - nabrojivi
        - Num - numericki
        - Integral - celobrojni
        - Floating - realni brojevi
    
    Ne postoji polimorfizam kao u OOP
        - postoji tipski polimorfizam

-}

{-  Funkcije

    Pattern matching
        - vise tela funkcije
        - poziva se ona ciji pattern odgovara prosledjenim podacima
        - "_" se koristi za podatak koji nam nije vazan
        - as patterns "xs@(x:y:ys)" - cuva referencu do cele liste

-}

