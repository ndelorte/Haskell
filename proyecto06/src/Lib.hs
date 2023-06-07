import Text.Show.Functions
import Data.List(genericLength)
import Data.Char(isUpper)
import GHC.Platform (platformUsesFrameworks)
--
-- * genericLength :: Num i => [a] -> i
-- -- Esta función es exactamente igual que length,
-- -- con la única diferencia que no devuelve un Int, sino un número
-- -- fácil de operar con otro número que pueden o no ser enteros.
-- --
-- -- -- ghci> length "Luigi Mario" / 2
-- -- -- error:
-- -- --     • No instance for (Fractional Int) arising from a use of ‘/’
-- -- --     • In the expression: length "Luigi Mario" / 2
-- -- --       In an equation for ‘it’: it = length "Luigi Mario" / 2
-- -- -- ghci> genericLength "Luigi Mario" / 2
-- -- -- 5.5
--a
-- * isUpper :: Char -> Bool
-- -- Esta función me dice si una letra es mayúscula o no.
-- --
-- -- -- ghci> isUpper 'B'
-- -- -- True
-- -- -- ghci> isUpper 'b'
-- -- -- False
--
--------------
-- Punto 01 --
--------------

data Plomero = Plomero{
    nombre                  :: String,
    cajaHerramientas        :: [Herramienta],
    historialDeReparaciones :: [Reparacion],
    dineroEncima            :: Float
}

data Herramienta = Herramienta {
    denominacion        :: String,
    precio              :: Float,
    materialEmpuniadura :: MaterialEmpuniadura
} deriving Eq

data MaterialEmpuniadura = Hierro | Madera | Goma | Plastico deriving Eq

mario :: Plomero
mario = Plomero "Mario" [llaveInglesa, martillo] [] 1200

llaveInglesa :: Herramienta
llaveInglesa = Herramienta "LLave inglesa" 200 Hierro

martillo :: Herramienta
martillo = Herramienta "Martillo" 20 Madera

wario :: Plomero
wario = Plomero "Wario" (infinitas llaveFrancesa) [] 0.5 

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta "Llave francesa" 1 Hierro

infinitas :: Herramienta -> [Herramienta]
infinitas unaHerramienta = unaHerramienta : infinitas (unaHerramienta {precio = precio unaHerramienta + 1})


--------------
-- Punto 02 --
--------------

type SaberSi = Plomero -> Bool

tiene :: Herramienta -> SaberSi
tiene unaHerramienta unPlomero = elem unaHerramienta (cajaHerramientas unPlomero) 

esMalvado :: SaberSi
esMalvado unPlomero = empiezaCon "Wa" (nombre unPlomero)

empiezaCon :: String -> String -> Bool
empiezaCon prefijo nombre = take 2 nombre == prefijo

puedeComprar :: Herramienta -> SaberSi
puedeComprar unaHerramienta unPlomero = precio unaHerramienta < dineroEncima unPlomero


--------------
-- Punto 03 --
--------------

esBuena :: Herramienta -> Bool
esBuena unaHerramienta
    | (tieneEmpuniaduraDe Hierro unaHerramienta) && (precio unaHerramienta < 1000)                                      = True
    | esMartillo unaHerramienta && (tieneEmpuniaduraDe Madera unaHerramienta || tieneEmpuniaduraDe Goma unaHerramienta) = True
    | otherwise                                                                                                         = False

tieneEmpuniaduraDe :: MaterialEmpuniadura -> Herramienta -> Bool
tieneEmpuniaduraDe material unaHerramienta = materialEmpuniadura unaHerramienta == material

esMartillo :: Herramienta -> Bool
esMartillo unaHerramienta = take 8 (denominacion unaHerramienta) == "Martillo"


--------------
-- Punto 04 --
--------------

comprar :: Herramienta -> Plomero -> Plomero
comprar unaHerramienta unPlomero
    | puedeComprar unaHerramienta unPlomero = mapHerramientas (++ [unaHerramienta]) . pagar (precio unaHerramienta) $ unPlomero 
    | otherwise = unPlomero

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
mapHerramientas funcion unPlomero = unPlomero {cajaHerramientas = funcion (cajaHerramientas unPlomero)}

pagar :: Float -> Plomero -> Plomero
pagar precio unPlomero = unPlomero {dineroEncima = dineroEncima unPlomero - precio}


--------------
-- Punto 05 --
--------------

data Reparacion = Reparacion{
    descripcion :: String,
    requerimiento :: Requerimiento
}

type Requerimiento = Plomero -> Bool

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion "Se traspasa el agua" (tiene llaveInglesa)

esDificil :: Reparacion -> Bool
esDificil unaReparacion = descripcionComplicada unaReparacion && esGrito unaReparacion

descripcionComplicada :: Reparacion -> Bool
descripcionComplicada unaReparacion = length (descripcion unaReparacion) > 100

esGrito :: Reparacion -> Bool
esGrito unaReparacion = all isUpper (descripcion unaReparacion)

presupuesto :: Reparacion -> Float
presupuesto unaReparacion = 3 * genericLength (descripcion unaReparacion)


--------------
-- Punto 06 --
--------------

hacer :: Reparacion -> Plomero -> Plomero
hacer unaReparacion unPlomero
    | puedeHacer unaReparacion unPlomero = plomeroConReparacionHecha unaReparacion unPlomero
    | otherwise = unPlomero {dineroEncima = dineroEncima unPlomero + 100}

puedeHacer :: Reparacion -> Plomero -> Bool
puedeHacer = requerimiento  

plomeroConReparacionHecha :: Reparacion -> Plomero -> Plomero
plomeroConReparacionHecha unaReparacion unPlomero
    | esMalvado unPlomero = accionesGenerales unaReparacion . mapHerramientas (++[destornillador]) $ unPlomero
    | esDificil unaReparacion = accionesGenerales unaReparacion . mapHerramientas (drop (length (cajaHerramientas unPlomero))) $ unPlomero
    | otherwise = accionesGenerales unaReparacion . mapHerramientas (drop 1) $ unPlomero

destornillador :: Herramienta
destornillador = Herramienta "Destornillador" 0 Plastico

accionesGenerales :: Reparacion -> Plomero -> Plomero
accionesGenerales unaReparacion = cobra (presupuesto unaReparacion) . agregarReparacion unaReparacion

cobra :: Float -> Plomero -> Plomero
cobra cantidad unPlomero = unPlomero {dineroEncima = dineroEncima unPlomero + cantidad}

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = unPlomero {historialDeReparaciones = historialDeReparaciones unPlomero ++ [unaReparacion]}



--------------
-- Punto 07 --
--------------

jornadaDeTrabajo :: Plomero -> [Reparacion] -> Plomero
jornadaDeTrabajo = foldr hacer

--------------
-- Punto 08 --
--------------

masReparador :: [Reparacion] -> [Plomero] -> Plomero
masReparador reparaciones = empleadoConMas cantidadReparaciones . empleadosConJornadaRealizada reparaciones

masAdinerado :: [Reparacion] -> [Plomero] -> Plomero
masAdinerado reparaciones = empleadoConMas dineroEncima . empleadosConJornadaRealizada reparaciones

masInirtio :: [Reparacion] -> [Plomero] -> Plomero
masInirtio reparaciones = empleadoConMas preciosHerramientas . empleadosConJornadaRealizada reparaciones

preciosHerramientas :: Plomero -> Float
preciosHerramientas =  sum . map (precio) . cajaHerramientas

cantidadReparaciones :: Plomero -> Float
cantidadReparaciones = genericLength . historialDeReparaciones

empleadoConMas :: (Plomero -> Float) -> [Plomero] -> Plomero
empleadoConMas funcion plomeros
    |  null [tail plomeros] = head plomeros
    | tieneMas (funcion (head plomeros)) (funcion (head (tail plomeros))) = empleadoConMas funcion (head plomeros :(tail (tail plomeros)) )
    | otherwise = empleadoConMas funcion (tail plomeros)

tieneMas :: Float -> Float -> Bool
tieneMas cant1 cant2 = cant1 > cant2

empleadosConJornadaRealizada :: [Reparacion] -> [Plomero] -> [Plomero]
empleadosConJornadaRealizada reparaciones = map (flip jornadaDeTrabajo reparaciones)