data Sustancia = ElementoSustancia Elemento
                | CompuestoSustancia Compuesto
                deriving (Show)

data Elemento = Elemento {
    nombreE :: String,
    simboloQuimico :: Char,
    numeroAtomico :: Int,
    grupo :: String
} deriving (Show)

data Componentes = ElementoCompuesto{
            elemento :: Elemento,
            cantidad :: Int
    } 
        | SubCompuesto{
            compuesto :: Compuesto,
            cantidad :: Int
    } deriving (Show)
 
data Compuesto = Compuesto {
    nombreC :: String,
    elementoYCant :: [Componentes]
} deriving (Show)

esElemento :: Sustancia -> Bool
esElemento (ElementoSustancia _) = True
esElemento _ = False

esCompuesto :: Sustancia -> Bool
esCompuesto (CompuestoSustancia _) = True
esCompuesto _ = False


-- 1
hidrogeno :: Elemento
hidrogeno = Elemento "Hidrógreno" 'H' 1 "no metal"
hidrogenoSustancia :: Sustancia
hidrogenoSustancia = ElementoSustancia hidrogeno

oxigeno :: Elemento
oxigeno = Elemento "Oxígeno" 'O' 8 "no metal"
oxigenoSustancia :: Sustancia
oxigenoSustancia = ElementoSustancia oxigeno

agua :: Compuesto
agua = Compuesto "Agua" [ElementoCompuesto hidrogeno 2 , ElementoCompuesto oxigeno 1] 
aguaSustancia :: Sustancia
aguaSustancia = CompuestoSustancia agua

obtenerElemento :: Sustancia ->Elemento
obtenerElemento sust = Just(ElementoSustancia sust)
-- 2

conduceBien :: Sustancia -> String -> Bool
conduceBien sustancia criterio 
        | Just (grupo sustancia) == "metal" = True
        | esElemento(sustancia) && Just (grupo sustancia) == "gas noble" && criterio == "electricidad" = True
        | esCompuesto(sustancia) && Just (grupo sustancia) == "halógeno" && criterio == "calor"  = True
        | otherwise = False
