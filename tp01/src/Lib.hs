import Data.List

data Sustancia = SustanciaSimple Elemento | SustanciaCompuesta Compuesto deriving (Show)

data Elemento = Elemento {
    nombreE :: String,
    simboloQuimico :: Char,
    numeroAtomico :: Int,
    grupo :: Grupo
} deriving Show

data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Componente = Componente{
    sustancia :: [Sustancia],
    cantidadMoleculas :: Int
} deriving Show

data Compuesto = Compuesto {
    nombreC :: String,
    componentes :: [Componente]
} deriving Show

------------ 1 -------------

hidrogeno :: Elemento
hidrogeno = Elemento "Hidrogeno" 'H' 1 NoMetal

oxigeno :: Elemento
oxigeno = Elemento "Oxígeno" 'O' 8 NoMetal

agua1 = Componente [SustanciaSimple hidrogeno] 2
agua2 = Componente [SustanciaSimple oxigeno] 1

agua :: Compuesto
agua = Compuesto "Agua" [agua1, agua2]

------------ 2 -------------

data Criterio = Electricidad | Calor deriving(Eq)

conduceBien :: Sustancia -> Criterio -> Bool
conduceBien (SustanciaSimple ele) criterio =
    case (grupo ele, criterio) of
        (GasNoble, Electricidad) -> True
        _                        -> grupo ele == Metal
conduceBien (SustanciaCompuesta comp) criterio =
    case (grupoCompuesto comp, criterio) of
        (Halogeno, Calor) -> True
        _                 -> any (\(Componente s _) -> conduceBien (s !! 0) criterio)(componentes comp)

grupoCompuesto :: Compuesto -> Grupo
grupoCompuesto comp = case componentes comp of
    []                  -> NoMetal
    (Componente s _):_  -> grupoSustancia (s !! 0)

grupoSustancia:: Sustancia -> Grupo
grupoSustancia (SustanciaSimple ele) = grupo ele
grupoSustancia (SustanciaCompuesta comp) = grupoCompuesto comp

------------ 3 -------------

esConsonante :: Char -> Bool
esConsonante c = c `elem` "bcdfhjklmnñpqrstvwxyz"

nombreUnion :: String -> String
nombreUnion nombre
    | esConsonante (last nombre) = nombre ++ "uro"
    | otherwise = nombreUnion (dropWhileEnd (esConsonante) nombre)

------------ 4 -------------

combinarNombres :: Elemento -> Elemento -> String
combinarNombres (Elemento nombre1 _ _ _) (Elemento nombre2 _ _ _) = nombreUnion(nombre1) ++ "de" ++ nombre2

------------ 5 -------------



-- mezclarDosComponentes (Componente nom cant) (Componente nom2 cant2) = Compuesto (nom ++ nom2) [nom,nom2]

-- mezclarComponentes :: [Componente] -> Compuesto
-- mezclarComponentes (c:cs) = foldl mezclarDosComponentes c cs

------------ 6 -------------
-- obtenerFormula :: Sustancia -> String
-- obtenerFormula (Elemento _ s _) = s
-- obtenerFormula (Compuesto _ s) = "(" ++ obtenerFormulaCompuestos s ++ ")"
--   where
--     obtenerFormulaCompuestos [] = ""
--     obtenerFormulaCompuestos [c] = obtenerFormulaComponente c
--     obtenerFormulaCompuestos (c:cs) = obtenerFormulaComponente c ++ obtenerFormulaCompuestos cs
--     obtenerFormulaComponente (s, 1) = obtenerFormula s
--     obtenerFormulaComponente (s, n) = obtenerFormula s ++ show n



