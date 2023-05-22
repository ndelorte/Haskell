-- Ej 1
sumatoriaLista :: Num a => [a] -> a
sumatoriaLista nums = sum nums

-- Ej 2

frecuenciaCardiaca :: Fractional a => [a]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
-- a
promedioFrecuenciaCardiaca :: Fractional a => a
promedioFrecuenciaCardiaca = sum frecuenciaCardiaca / fromIntegral (length frecuenciaCardiaca)
--b
frecuenciaCardiacaMinuto :: Fractional a => Int -> a
frecuenciaCardiacaMinuto n = frecuenciaCardiaca !! (div n 10)
--c
frecuenciasHastaMomento :: Fractional a => Int -> [a]
frecuenciasHastaMomento n = take ((div n 10)+1) frecuenciaCardiaca

-- Ej 3

esCapicua :: [String] -> Bool
esCapicua list = concat list == reverse (concat list)

-- Ej 4

duracionLlamadas :: ((String, [Int]),(String, [Int]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))
--a
cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos  
    | (sum . snd . fst $ duracionLlamadas) > (sum . snd . snd $ duracionLlamadas) = fst . fst $ duracionLlamadas
    | (sum . snd . fst $ duracionLlamadas) < (sum . snd . snd $ duracionLlamadas) = fst . snd $ duracionLlamadas
    | otherwise                                                     = "Se hablo igual"
--b
cuandoHizoMasLlamadas :: String
cuandoHizoMasLlamadas
    | (length . snd . fst $ duracionLlamadas) > (length . snd . snd $ duracionLlamadas) = fst . fst $ duracionLlamadas
    | (length . snd . fst $ duracionLlamadas) < (length . snd . snd $ duracionLlamadas) = fst . snd $ duracionLlamadas
    | otherwise                                                     = "Se hablo igual"