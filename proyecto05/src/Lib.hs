--  PdePrime Video --

----- Modelados -----


data Serie = Serie {
    nombre :: String,
    actores :: [Actor],
    presupuesto :: Int,
    cantidadTemporadasEstimadas :: Int,
    ratingPromedio :: Float,
    cancelada :: Bool
} deriving (Show)

data Actor = Actor {
    nombreActor :: String,
    sueldoAnualPretendido :: Int,
    restricciones :: [Restriccion]
} deriving (Show)

type Restriccion = String

seriePrueba :: Serie
seriePrueba = Serie "ejemplo" [Actor "nicolas" 1000 [],Actor "Javier" 1000 [],Actor "miriam" 1000 []] 1000 3 3.4 True


--------------------
----- Punto 01 -----
--------------------

estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = (presupuesto unaSerie) < (pretendidoActores (actores unaSerie))

pretendidoActores :: [Actor] -> Int
pretendidoActores = sum . map sueldoAnualPretendido

esProblematica :: Serie -> Bool
esProblematica unaSerie = actoresConRestricciones (actores unaSerie) > 3

actoresConRestricciones :: [Actor] -> Int
actoresConRestricciones actores = length (filter ((< 1) . length . restricciones) actores)


--------------------
----- Punto 02 -----
--------------------

type Productor = Serie -> Serie

-----     a     -----

conFavoritismos :: [Actor] -> Productor
conFavoritismos actoresFavs = agregarActores actoresFavs . eliminarActores 2

eliminarActores :: Int -> Serie -> Serie
eliminarActores cantidad unaSerie = unaSerie {actores = drop cantidad (actores unaSerie)}

agregarActores :: [Actor] -> Serie -> Serie
agregarActores actoresNuevos unaSerie = unaSerie {actores = actoresNuevos ++ actores unaSerie}

-----     b     -----

timBurton :: Productor
timBurton = conFavoritismos [jhonnyDepp, helenaBonham]

jhonnyDepp :: Actor
jhonnyDepp = Actor "Jhonny Depp" 20000000 []

helenaBonham :: Actor
helenaBonham = Actor "Helena Bonham" 15000000 []

-----     c     -----

gatopardeitor :: Productor
gatopardeitor unaSerie = unaSerie

-----     d     -----

estireitor :: Productor
estireitor = duplicarTemporadas

duplicarTemporadas :: Serie -> Serie
duplicarTemporadas unaSerie = unaSerie {cantidadTemporadasEstimadas = (*2) . (cantidadTemporadasEstimadas) $ unaSerie}

-----     e     -----


-----     f     -----

canceleitor :: Float -> Productor
canceleitor cifra unaSerie 
    | estaEnRojo unaSerie || (ratingPromedio unaSerie) < cifra = unaSerie {cancelada = True}
    | otherwise                                                = unaSerie

--------------------
----- Punto 03 -----
--------------------

bienestar :: Serie -> Int

bienestar unaSerie
    | cancelada unaSerie = 0
    | otherwise = bienestarPorLongitud (cantidadTemporadasEstimadas unaSerie) + bienestarPorReparto(actores unaSerie)

bienestarPorLongitud :: Int -> Int
bienestarPorLongitud temporadas
    | temporadas > 4 = 5
    | otherwise      = (10-temporadas)*2

bienestarPorReparto :: [Actor] -> Int
bienestarPorReparto actores
    | length actores < 10 = 3
    | otherwise           = bienestarConMuchosActores (actoresConRestricciones actores)

bienestarConMuchosActores :: Int -> Int
bienestarConMuchosActores cantidadActores
    | (10-cantidadActores) >= 2 = (10-cantidadActores)
    | otherwise                 = 2

--------------------
----- Punto 04 -----
--------------------

-- aplicarMasEfectiva :: [Serie] -> [Productor] -> [Serie]
-- aplicarMasEfectiva series productores = 


--------------------
----- Punto 05 -----
--------------------

--a) Se puede aplicar porque nos devuelve la misma lista que le pasamos
--b) Si se puede aplicar porque solo estamos interactuando con los dos primeros elementos de esa lista

--------------------
----- Punto 06 -----
--------------------

esControvertida :: Serie -> Bool
esControvertida = not . cobranEnOrden . actores

cobranEnOrden :: [Actor] -> Bool
cobranEnOrden (actor:siguiente) 
    | esListaVacia siguiente = True
    | cobraMas (head siguiente) actor = False
    | otherwise = cobranEnOrden (siguiente)

esListaVacia :: [a] -> Bool
esListaVacia a = length a == 0

cobraMas :: Actor -> Actor -> Bool
cobraMas actor1 actor2 = sueldoAnualPretendido actor1 > sueldoAnualPretendido actor2

--------------------
----- Punto 07 -----
--------------------

-- x es int por buscar los pares, y es tipo lista o foldable por poder aplicarle length
funcionLoca x y = filter (even.x) . map (length.y)