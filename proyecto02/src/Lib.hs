--Aplicacion parcial

-- Ej 1

siguiente :: Int -> Int
siguiente num = num+1

-- Ej 2

mitad :: Float -> Float
mitad num = num/2

-- Ej 3

inversa :: Float -> Float
inversa num = 1/num

-- Ej 4

triple :: Int -> Int
triple num = 3*num

-- Ej 5

esNumeroPositivo :: Float -> Bool
esNumeroPositivo num = num>=0

--Composicion

-- Ej 6

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m = ((== 0) . (mod m)) n

-- Ej 7

esBisiesto :: Int -> Bool
esBisiesto n = (esMultiploDe 4 n) && (not . esMultiploDe 100) n

-- Ej 8

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = (inversa . sqrt)

-- Ej 9

incrementMCuadradoN :: Int -> Int -> Int
incrementMCuadradoN n m = ((+m).(*n)) n

-- Ej 10

esResultadoPar :: Int -> Int -> Bool
esResultadoPar n m = ((esMultiploDe 2) . (n ^ ))m