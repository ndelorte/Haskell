
module Lib () where

--1)
type Coordenada = (Int, Int)
type Bolitas    = [Bolita]
type Celdas     = [Celda]
type Condicion  = Tablero -> Bool
type Sentencia  = Tablero -> Tablero
data Cardinal   = NORTE|SUR|ESTE|OESTE
data Bolita     = ROJO|AZUL|VERDE|NEGRO deriving (Show, Eq)
data Cabezal    = Cabezal {coordenadaC :: Coordenada} deriving Show 
data Celda      = Celda {coordenada :: Coordenada, bolitas :: Bolitas} deriving Show
data Tablero    = Tablero {celdas :: Celdas, cabezal :: Cabezal}  deriving Show

--2)
inicializarTablero :: (Int, Int) -> Tablero
inicializarTablero (filas, columnas) = Tablero (crearCeldas (filas, columnas)) (Cabezal(1, 1))

crearCeldas :: (Int, Int) -> [Celda]
crearCeldas (1, columnas)     = crearFila (1, columnas)
crearCeldas (filas,columnas)  = crearFila (filas,columnas) ++ crearCeldas (filas-1, columnas)

crearFila :: (Int, Int) -> [Celda]
crearFila (filas, 1)        = [Celda ((filas, 1)) []]
crearFila (filas, columnas) = [Celda (filas,columnas) []] ++ crearFila (filas,columnas-1)


--Example
tablero3x3 :: Tablero
tablero3x3 = inicializarTablero (3,3)

--3)
--a)


moverCabezalAl :: Cardinal -> Sentencia
moverCabezalAl (NORTE) tablero = moverAlNorte tablero
moverCabezalAl (SUR) tablero   = moverAlSur tablero
moverCabezalAl (ESTE) tablero  = moverAlEste tablero
moverCabezalAl (OESTE) tablero = moverAlOeste tablero

puedeMoverse :: Cardinal -> Tablero -> Bool
puedeMoverse (NORTE) (Tablero celdas (Cabezal (_,y))) = not (y == (snd . coordenada . head $ celdas))
puedeMoverse (SUR) (Tablero celdas (Cabezal (_,y)))   = not (y == (snd . coordenada . last $ celdas))
puedeMoverse (ESTE) (Tablero celdas (Cabezal (x,_)))  = not (x == (fst . coordenada . last $ celdas))
puedeMoverse (OESTE) (Tablero celdas (Cabezal (x,_))) = not (x == (fst . coordenada . head $ celdas))


moverAlNorte :: Tablero -> Tablero
moverAlNorte (Tablero celdas (Cabezal (x,y)))
    | not (puedeMoverse NORTE (Tablero celdas (Cabezal (x,y))))  = error"no se puede mover en esa direccion (NORTE)"
    | otherwise                                                  = Tablero celdas (Cabezal (x,y+1))


moverAlSur :: Tablero -> Tablero
moverAlSur (Tablero celdas (Cabezal (x,y)))
    | not (puedeMoverse SUR (Tablero celdas (Cabezal (x,y)))) = error"no se puede mover en esa direccion(SUR)"
    | otherwise                                               = Tablero celdas (Cabezal (x,y-1))


moverAlEste :: Tablero -> Tablero
moverAlEste (Tablero celdas (Cabezal (x,y)))
    | not (puedeMoverse ESTE (Tablero celdas (Cabezal (x,y))))  = error"no se puede mover en esa direccion(ESTE)"
    | otherwise                                                 = Tablero celdas (Cabezal (x-1,y))


moverAlOeste :: Tablero -> Tablero
moverAlOeste (Tablero celdas (Cabezal (x,y))) 
    | not (puedeMoverse OESTE (Tablero celdas (Cabezal (x,y))))  = error"no se puede mover en esa direccion(OESTE)"
    | otherwise                                                  = Tablero celdas (Cabezal (x+1,y))


--b)


ponerBolitaColor:: Bolita -> Sentencia
ponerBolitaColor bolita (Tablero celdas cabezal) = Tablero (colocarBolita bolita cabezal celdas) cabezal

colocarBolita :: Bolita -> Cabezal -> [Celda] -> [Celda]
colocarBolita bolita celdaActual = map (celdaConBolita celdaActual bolita)

celdaConBolita :: Cabezal -> Bolita -> Celda -> Celda
celdaConBolita (Cabezal (x,y)) bolitaAColocar (Celda (x1,y1) bolita)
    | (x,y) == (x1,y1) = Celda (x,y) (bolitaAColocar:bolita)
    | otherwise        = Celda (x1,y1) bolita


--c)
sacarBolitaColor :: Bolita -> Sentencia
sacarBolitaColor bolita (Tablero celdas cabezal)
    | not (any (== bolita) (bolitas (head (filter((== (coordenadaC cabezal)) . coordenada) celdas)))) = error"No hay bolitas de ese color en la celda actual"
    | otherwise                                                                                       = Tablero (sacarBolitaDeCelda bolita cabezal celdas) cabezal


sacarBolitaDeCelda :: Bolita -> Cabezal -> [Celda] -> [Celda]
sacarBolitaDeCelda bolita celdaActual = map (eliminarBolitaColor celdaActual bolita)


eliminarBolitaColor:: Cabezal -> Bolita -> Celda -> Celda 
eliminarBolitaColor (Cabezal (x,y)) bolitaAEliminar (Celda (x1,y1) bolita)
    | (x,y) == (x1,y1) = Celda (x,y) (filter (\x -> bolitaAEliminar /= x) bolita)
    | otherwise        = Celda (x1,y1) bolita

--4
--a

repetir :: [Sentencia] -> Int -> Sentencia
repetir _ 0 tablero             = tablero
repetir sentencias cant tablero = repetir sentencias (cant -1) (ejecutarSentencias sentencias tablero)

ejecutarSentencias :: [Sentencia] -> Sentencia
ejecutarSentencias [] tablero              = tablero
ejecutarSentencias (primera:resto) tablero = ejecutarSentencias resto (primera tablero)

--b


alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Sentencia
alternativa condicion  sentencias1 sentencias2 tablero
    | condicion tablero = ejecutarSentencias sentencias1 tablero
    | otherwise         = ejecutarSentencias sentencias2 tablero

si :: Condicion -> [Sentencia] -> Sentencia
si condicion sentencias tablero
    | condicion tablero = ejecutarSentencias sentencias tablero
    | otherwise         = tablero

siNo :: Condicion -> [Sentencia] -> Sentencia
siNo condicion sentencias tablero
    | not . condicion $ tablero = ejecutarSentencias sentencias tablero
    | otherwise                 = tablero

-- c

mientras :: Condicion -> [Sentencia] -> Sentencia
mientras condicion sentencias tablero
    | not . condicion $ tablero = tablero
    | otherwise                 = mientras condicion sentencias (ejecutarSentencias sentencias tablero)

-- d


irAlBorde :: Cardinal -> Sentencia
irAlBorde cardinal tablero
    | not (puedeMoverse cardinal tablero) = tablero
    | otherwise                           = irAlBorde cardinal (moverCabezalAl cardinal tablero)


--5
--a

--hecho arriba

--b

celdaActual :: Tablero -> Celda
celdaActual (Tablero celdas (Cabezal (x, y))) = head (filter ((== (x, y)).coordenada)  celdas)

hayBolita :: Bolita -> Tablero -> Bool
hayBolita bolita tablero = any (==bolita) (bolitas (celdaActual tablero))

--c

cantidadDeBolitas :: Tablero -> Bolita -> Int
cantidadDeBolitas tablero color = length . filter(==color) . bolitas . celdaActual $ tablero

--6

programa :: Tablero -> [Sentencia] -> Tablero
programa tablero []     = tablero
programa tablero (x:xs) = programa (x tablero) xs

--7
listaParaProgramaPrueba :: [Sentencia]
listaParaProgramaPrueba = [moverCabezalAl NORTE, ponerBolitaColor NEGRO, ponerBolitaColor NEGRO, ponerBolitaColor AZUL, moverCabezalAl NORTE, repetir  [ponerBolitaColor ROJO, ponerBolitaColor AZUL] 15, si (hayBolita VERDE) [moverCabezalAl ESTE, ponerBolitaColor NEGRO], siNo (hayBolita VERDE) [moverCabezalAl SUR, moverCabezalAl ESTE, ponerBolitaColor AZUL], moverCabezalAl ESTE, mientras ((<=9) . (flip cantidadDeBolitas VERDE)) [ponerBolitaColor VERDE],ponerBolitaColor AZUL]

programaPrueba :: Tablero
programaPrueba = programa tablero3x3 listaParaProgramaPrueba