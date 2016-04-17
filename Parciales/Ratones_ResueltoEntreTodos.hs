import Data.List

data Raton = UnRaton String Float Float [Enfermedad] deriving (Show, Eq)

type Enfermedad = String

-- Ejemplo de raton
cerebro = UnRaton "Cerebro" 9.0 0.2 ["brucelosis", "sarampion", "tuberculosis"]

-- 1)
modificarNombre :: (String -> String) -> Raton -> Raton
modificarNombre f (UnRaton nombre edad peso enfermedades) = UnRaton (f nombre) edad peso enfermedades

modificarEdad :: (Float -> Float) -> Raton -> Raton
modificarEdad f (UnRaton nombre edad peso enfermedades) = UnRaton nombre (f edad) peso enfermedades

modificarPeso :: (Float -> Float) -> Raton -> Raton
modificarPeso f (UnRaton nombre edad peso enfermedades) = UnRaton nombre edad (f peso) enfermedades

modificarEnfermedades :: ([Enfermedad] -> [Enfermedad]) -> Raton -> Raton
-- modificarEnfermedades :: ([String] -> [String]) -> Raton -> Raton  ... USAR LOS ALIAS!!
modificarEnfermedades f (UnRaton nombre edad peso enfermedades) = UnRaton nombre edad peso (f enfermedades)

-- 2)
hierbaBuena :: Raton -> Raton
hierbaBuena = modificarEdad sqrt

------------------

hierbaVerde :: String -> Raton -> Raton
hierbaVerde terminacion raton = (flip modificarEnfermedades raton . quitarEnfermedades) terminacion

quitarEnfermedades terminacion = filter (noTerminaEn terminacion)

noTerminaEn terminacion = not . elem terminacion . tails

------------------

hierbaVerde' :: String -> Raton -> Raton
hierbaVerde' terminacion = modificarEnfermedades (quitarEnfermedades terminacion)

------------------

alcachofa :: Raton -> Raton
alcachofa raton
	| peso raton > 2 = modificarPeso (*0.9) raton
	| otherwise = modificarPeso (*0.95) raton

peso (UnRaton _ _ p _) = p

------------------

hierbaLife :: Bool -> Raton -> Raton
hierbaLife False = modificarEnfermedades borrarPrimerEnfermedad
hierbaLife True = modificarNombre borrarNombre . hierbaLife False

borrarPrimerEnfermedad enfermedades = delete (head enfermedades) enfermedades

borrarPrimerEnfermedad' (_ : enfermedades) = enfermedades

borrarPrimerEnfermedad'' = tail

borrarNombre _ = ""

------------------

hierbaLife' fortificada
	| fortificada = curarPrimerEnfermedad
	| otherwise = curarPrimerEnfermedad . causarAmnesia

curarPrimerEnfermedad = modificarEnfermedades borrarPrimerEnfermedad

causarAmnesia raton = modificarNombre (\_ -> "") raton


-- 3)
type Medicamento = Raton -> Raton

medicamento :: [Raton -> Raton] -> Medicamento
medicamento hierbas raton = foldl (\raton hierba -> hierba raton) raton hierbas

medicamento' :: Raton -> [Raton -> Raton] -> Raton  -- No está el Medicamento :(
medicamento' = foldl (flip ($))

medicamento'' [] raton = raton
medicamento'' (f:fs) raton = medicamento fs (f raton)

medicamento''' hierbas raton = foldl aplicarHierba raton hierbas
aplicarHierba raton hierba = hierba raton

medicamento'''' hierbas = foldl1 (.) hierbas

-----------------------

pondsAntiAge :: Medicamento
pondsAntiAge = medicamento hierbasDePonds

hierbasDePonds = [hierbaBuena, hierbaBuena, hierbaBuena, hierbaLife False, alcachofa]
hierbasDePonds' = replicate 3 hierbaBuena ++ [hierbaLife False, alcachofa]

------------------------

reduceFatFast :: Int -> Medicamento
reduceFatFast potencia = medicamento (hierbasReduce potencia)

hierbasReduce potencia = [hierbaVerde "obesidad"] ++ replicate potencia alcachofa

hierbasReduce' potencia = hierbaVerde "obesidad" : replicate potencia alcachofa

hierbasReduce'' = (hierbaVerde "obesidad" :) . flip replicate alcachofa

------------------------

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

infectiCilina :: Medicamento
infectiCilina = (medicamento . crearHierbasVerdes) sufijosInfecciosas

crearHierbasVerdes :: [String] -> [Raton -> Raton]
crearHierbasVerdes = map hierbaVerde

----------------------

infectiCilina' = medicamento enfermedadesInfecciosas

enfermedadesInfecciosas = map hierbaVerde sufijosInfecciosas

----------------------

-- 4)
cantidadIdeal :: (Int -> Bool) -> Int
cantidadIdeal f = (head . filter f) naturales
naturales = [1..]


cantidadIdeal' f = head . filter f $ naturales

-------------------------

estanMasLindosQueNunca :: [Raton] -> Medicamento -> Bool
estanMasLindosQueNunca ratones remedio = all pesaMenosDeUnKilo (map remedio ratones)

pesaMenosDeUnKilo :: Raton -> Bool
pesaMenosDeUnKilo raton = peso raton < 1

pesaMenosDeUnKilo' = (<1) . peso

------------------------

estanMasLindosQueNunca' ratones = all pesaMenosDeUnKilo . aplicarATodos ratones

aplicarATodos = flip map 

------------------------

estanMasLindosQueNunca'' :: Medicamento -> [Raton] -> Bool
estanMasLindosQueNunca'' remedio = all pesaMenosDeUnKilo . map remedio

------------------------

potenciaIdeal :: [Raton] -> Int
potenciaIdeal ratones = potenciar (False, 0, ratones)

potenciar (True, n, _) = n
potenciar (False, n, ratones) = potenciar (estanMasDelgadosQueNunca (n+1) ratones, n+1, ratones)

estanMasDelgadosQueNunca = estanMasLindosQueNunca'' . reduceFatFast 

------------------------

potenciaIdeal' ratones = cantidadIdeal (estenTodosMuyLindos ratones)

potenciaIdeal'' = cantidadIdeal . estenTodosMuyLindos


estenTodosMuyLindos :: [Raton] -> Int -> Bool
estenTodosMuyLindos ratones potencia = estanMasLindosQueNunca ratones (reduceFatFast potencia)

estenTodosMuyLindos' ratones = estanMasLindosQueNunca ratones . reduceFatFast 









