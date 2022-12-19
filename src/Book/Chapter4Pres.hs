module Book.Chapter4Pres (pres) where

-- # pattern matching

-- flere "signaturer" som matcher
-- kan minne litt om switch-statement med fall-through fra topp til bunn
pattern :: String -> Int
pattern "a" = 1
pattern "b" = 2
pattern "c" = 3
pattern altAnnet = -1

-- pattern "b"
-- > 2

-- kan brukes til å "destructure" verdier
-- ekstra nyttig for tuples
destructure :: (Int, Int) -> Int
destructure (x, y) = x + y

-- destructure (1, 2)
-- > 3

-- eksempel fra boka på å hente ut spesifikke verdier fra triples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- kan også brukes til å hente ut verdier fra lister
forsteVerdiFraListe :: [Int] -> Int
forsteVerdiFraListe (forsteVerdi : resten) = forsteVerdi

-- kotyme med å bruke "x:xs" på navn
hjemmeSnekraHead :: [Int] -> Int
hjemmeSnekraHead (x : xs) = x

-- hjemmeSnekraHead ["en", "to", "tre"]
-- > "en"

forsteTreTing :: [Int] -> Int
forsteTreTing (en : to : tre : []) = en + to + tre

-- her er haskell litt sint fordi vi ikke er "exhaustive" i pattern matchingen

-- forsteTreTing [1, 2, 3, 4, 5]
-- > 6

-- forsteTreTing [1, 2]
-- > *** Exception: Non-exhaustive patterns in function forsteTreTing

forsteTreTingExhaustive :: [Int] -> Int
forsteTreTingExhaustive (en : to : tre : xs) = en + to + tre
forsteTreTingExhaustive (en : to : xs) = en + to
forsteTreTingExhaustive (en : xs) = en
forsteTreTingExhaustive xs = -1

-- forsteTreTingExhaustive [1, 2, 3, 4, 5]
-- > 6

-- forsteTreTingExhaustive [1, 2]
-- > 3

-- forsteTreTingExhaustive []
-- > -1

-- eksempel fra boka
hjemmeSnekraLength :: (Num b) => [a] -> b
hjemmeSnekraLength [] = 0
hjemmeSnekraLength (_ : xs) = 1 + hjemmeSnekraLength xs

-- dersom man vil ta vare på en referanse til hele lista kan man bruke @
-- eksempel fra boka
forsteBokstav :: String -> String
forsteBokstav "" = ">:("
forsteBokstav all@(x : xs) = "Første bokstaven i " ++ all ++ " er " ++ [x]

-- forsteBokstav "Mordi"
-- > "Første bokstaven i Mordi er M"

-- # guards
-- litt som when i Kotlin (uten parameter)
-- blir fort litt som if-else

premiumhet :: (RealFloat a) => a -> String
premiumhet faktureringsgrad
  | faktureringsgrad > 110 = "Lars er stolt"
  | faktureringsgrad > 100 = "Bra jobba Daniel"
  | faktureringsgrad > 90 = "Håper du har vært på foreldrepermisjon"
  | faktureringsgrad > 80 = "Dette ekke Accenture"
  | otherwise = "Du har sparken"

-- premiumhet 111.69
-- > "Lars er stolt"

-- de kan seff ta inn flere params
-- eksempel fra boka
hjemmeSnekraMax :: (Ord a) => a -> a -> a
hjemmeSnekraMax a b
  | a > b = a
  | otherwise = b

-- denne var litt interessant, du kan definere funksjonssignaturen din som en infix
hjemmeSnekraCompare :: (Ord a) => a -> a -> Ordering
a `hjemmeSnekraCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- 5 `hjemmeSnekraCompare` 3
-- > GT

--  # where
-- en plass vi kan putte variabler for å rødde i køden
--
-- wait it's all functions?
--                     🌎 👨‍🚀 🔫👨‍🚀
--                              always has been
--
-- så du kan bruke "variablene" til å definere små lokale funksjoner
-- veldig "contrived" eksempel:
isEven :: Integer -> Bool
isEven x
  | rest == 0 = True
  | otherwise = False
  where
    -- "variabel"
    rest = evenRest x
    -- "funksjon"
    evenRest y = y `mod` 2

-- eksempel fra boka
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- # let
-- TODO

pres :: IO ()
pres = do
  print $ pattern "a"
