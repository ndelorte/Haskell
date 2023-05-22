
esPar :: Int -> Bool
esPar numero = mod numero 2 == 0

esLongitudPar :: String -> Bool
esLongitudPar palabra = (esPar.length) palabra

-- Ej 1

esMultiploDeTres :: Int-> Bool
esMultiploDeTres numero = mod numero 3 == 0

-- Ej 2

esMultiploDe :: Int -> Int -> Bool
esMultiploDe num1 num2 = mod num2 num1 == 0

-- Ej 3

cubo :: Int -> Int
cubo numero = numero*numero

-- Ej 4

area :: Float -> Float -> Float
area num1 num2 = num1*num2

-- Ej 5

esBisiesto :: Int -> Bool
esBisiesto ano = (esMultiploDe 4 ano && not (esMultiploDe 100 ano))

-- Ej 6

celsiusToFahr :: Fractional a => a -> a
celsiusToFahr celsius = (celsius*1.8)+32

-- Ej 7

fahrToCelsius :: Fractional a => a -> a
fahrToCelsius fahr = (fahr-32)*5/9

-- Ej 8

haceFrioF :: Float -> Bool
haceFrioF temp = (fahrToCelsius temp) < 8

-- Ej 9

mcm :: Int -> Int -> Int
mcm a b = (a * b) `div` (gcd a b)

max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)

min3 :: Ord a => a -> a -> a -> a
min3 a b c = min a (min b c)

-- Ej 10

dispersion :: (Ord a, Num a) => a -> a -> a -> a
dispersion a b c = max3 a b c - min3 a b c

diasParejos :: (Ord a, Num a) => a -> a -> a -> Bool
diasParejos a b c = (dispersion a b c) < 30

diasLocos :: (Ord a, Num a) => a -> a -> a -> Bool
diasLocos a b c = (dispersion a b c) > 100

diasNormales :: (Ord a, Num a) => a -> a -> a -> Bool
diasNormales a b c = not (diasParejos a b c) && not (diasLocos a b c)

-- Ej 11

pesoPinoSegunAlturaEnCm :: Int -> Int
pesoPinoSegunAlturaEnCm alturaCm = (3 * (min 300 alturaCm))+(2 * max 0 (alturaCm - 300))

esPesoUtil :: Int -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

sirvePino :: Int -> Bool
sirvePino altura = esPesoUtil . pesoPinoSegunAlturaEnCm $ altura

-- Ej 12

sumarImpares :: Int -> Int -> Int -> Int
sumarImpares n i j
    | (n == 0) || (n < i) = 0
    | (n == i)            = n
    | otherwise             = sumarImpares n (i+j) (j+2)

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto num = (sumarImpares num 0 1) == num
