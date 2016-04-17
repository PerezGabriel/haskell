import Data.List

data Raton = UnRaton String Float Float [Enfermedad] deriving (Show, Eq)
type Enfermedad = String

-- nick:: Raton -> String
-- nick (UnRaton nombre _ _ _ ) = nombre

-- ----- EJERCICIO 1

modificarNombre::  (String -> String) -> Raton -> Raton
modificarNombre f (UnRaton nomb edad peso enfermedades) = UnRaton ( f nomb)  edad peso enfermedades

modificarEdad:: (Float -> Float) -> Raton -> Raton
modificarEdad f (UnRaton nomb edad peso enfermedades) = UnRaton nomb  (f edad) peso enfermedades

modificarPeso:: (Float -> Float) -> Raton -> Raton
modificarPeso f (UnRaton nomb edad peso enfermedades) = UnRaton nomb  edad (f peso) enfermedades

modificarEnfermedades:: ([Enfermedad] -> [Enfermedad]) -> Raton -> Raton
modificarEnfermedades f (UnRaton nomb edad peso enfermedades) = UnRaton nomb  edad (peso) (f enfermedades)

-- --------------------

-- ----- EJERCICIO 2

hierbaBuena:: Raton -> Raton
hierbaBuena = modificarEdad sqrt

-- hierbaVerde:: Raton -> String -> Raton
-- hierbaVerde raton = flip modificarEnfermedades raton . quitarEnfermedades 

hierbaVerde:: String -> Raton -> Raton
hierbaVerde terminacion = modificarEnfermedades (quitarEnfermedades terminacion)

alcachofa:: Raton -> Raton
alcachofa = modificarPeso reducirPorcentaje

hierbaLife:: Bool -> Raton -> Raton
hierbaLife False = modificarEnfermedades curarPrimera
hierbaLife True = modificarRaton [modificarEnfermedades curarPrimera, amnesia]

----- AUXILIARES -----

quitarEnfermedades:: String -> [Enfermedad] -> [Enfermedad]
quitarEnfermedades tipo = filter (noTerminaEn tipo)

noTerminaEn:: String -> Enfermedad -> Bool
noTerminaEn tipo = not . (elem tipo) . tails

reducirPorcentaje:: Float -> Float
reducirPorcentaje peso = (*) peso (porcentajeAReducir peso)

porcentajeAReducir:: Float -> Float
porcentajeAReducir = pesaMasDe2Kg . (<) 2

pesaMasDe2Kg:: Bool -> Float
pesaMasDe2Kg True = 0.9
pesaMasDe2Kg _ = 0.95

fortificada:: Bool -> Raton -> Raton
fortificada True  = modificarRaton [modificarEnfermedades curarPrimera, amnesia]

modificarRaton::  [(Raton -> Raton)] -> Raton -> Raton
modificarRaton [] raton = raton
modificarRaton (x:xs) raton = modificarRaton xs (x raton)

amnesia:: Raton -> Raton
amnesia = modificarNombre (\ _ -> "")

-- borrar:: String -> String
-- borrar = ""

curarPrimera:: [Enfermedad] -> [Enfermedad]
curarPrimera = tail
-- curarPrimera (_:xs) = xs

----------------------

-- ----- EJERCICIO 3

type Medicamento = Raton -> Raton

medicamento::  [(Raton -> Raton)] -> Medicamento
medicamento [] = noHacerNada
medicamento (cabeza:cola) = medicamento cola . cabeza

medicamento':: Raton -> [(Raton -> Raton)] -> Raton
medicamento' = foldl (flip ($))

medicamento''':: [(Raton -> Raton)] -> Medicamento
medicamento''' = foldl1 (.)

pondsAntiAge:: Medicamento
pondsAntiAge = medicamento hierbasDePonds

reduceFatFast:: Int -> Medicamento
reduceFatFast  =  medicamento . hiervasReduceFatFast

infectiCilina:: Medicamento
infectiCilina = medicamento (hierbasVerdesCon [ "sis", "itis", "emia", "cocos"])


----- AUXILIARES -----
hiervasReduceFatFast:: Int -> [(Raton -> Raton)] 
hiervasReduceFatFast  = ([hierbaVerde "obesidad"] ++) . agregarAlcachofas 


hierbasDePonds = replicate 3 hierbaBuena ++ [hierbaLife False, alcachofa]

hierbasVerdesCon:: [String] ->  [(Raton -> Raton)]
hierbasVerdesCon  = map hierbaVerde 

noHacerNada:: Raton -> Raton
noHacerNada rat = rat

agregarAlcachofas:: Int -> [Raton -> Raton]
agregarAlcachofas = flip replicate alcachofa 


-- ----- EJERCICIO 4
cantidadIdeal:: (Int -> Bool) -> Int
cantidadIdeal = flip findFirst todosLosNaturales

cantidadIdeal' f = head . filter f $ todosLosNaturales


findFirst:: (Int -> Bool) -> [Int] -> Int
findFirst condicion = head . filter condicion

estanMasLindosQueNunca:: [Raton] -> Medicamento -> Bool
estanMasLindosQueNunca ratones = all pesaMenosQue1 . aplicarATodos ratones

potenciaIdeal:: [Raton] -> Int
potenciaIdeal ratones = potenciar (False, 0 , ratones)


potenciaIdeal':: [Raton] -> Int
potenciaIdeal'  = cantidadIdeal . estenTodosMuyLindos

----- AUXILIARES -----

estenTodosMuyLindos:: [Raton] -> Int -> Bool
estenTodosMuyLindos ratones = estanMasLindosQueNunca ratones . reduceFatFast 


todosLosNaturales:: [Int]
todosLosNaturales = iterate (+1) 1

aplicarATodos:: [Raton] -> Medicamento -> [Raton]
aplicarATodos  = flip map 

pesaMenosQue1:: Raton -> Bool
pesaMenosQue1 = (<1) . peso 

peso:: Raton -> Float
peso  (UnRaton _ _ kg _) = kg

potenciar:: (Bool, Int, [Raton]) -> Int
potenciar (True, n, ratones) = n
potenciar (False, n, ratones) = potenciar (estanMasLindosQueNuncaConReduce ratones $ (n+1), (n+1), ratones)


estanMasLindosQueNuncaConReduce:: [Raton] -> Int -> Bool
estanMasLindosQueNuncaConReduce ratones = estanMasLindosQueNunca ratones . reduceFatFast 