esPar :: Int -> Bool
esPar numero = mod numero 2 == 0


esLongitudPar :: String -> Bool
esLongitudPar palabra = (esPar.length) palabra