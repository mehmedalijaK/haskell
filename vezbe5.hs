import Distribution.Simple.Utils (xargs)
-- Novi tip drugari, sa dve vrednosti, Pera i Mika

data Drugari = Pera | Mika

test :: Drugari -> String
test Pera = "Pera"
test Mika = "Mika"


type Cena = Int
type Temp = Int
data Simptom = Simptom [String] deriving Show

data Pacijent = Zdrav Cena | Bolestan Temp Simptom deriving Show
-- da bismo mogli da prikazemo dodajemo Show, sluzi da konvertuje nase vrednosti u String. Inace ne bi mogao da printa

-- bolnica :: Pacijent -> String
-- bolnica (Zdrav x) = "Plati: " ++ show x
-- bolnica (Bolestan t s) = "Plati: " ++ show x
