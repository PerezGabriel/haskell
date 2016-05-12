series = [("los soprano", 6, 1999, "HBO"),("lost", 6, 2004, "ABC"),("4400", 4, 2004, "CBS"),("United States of Tara", 3, 2009, "Dreamworks"),("V", 3, 2009, "Warner Bross"),("dr house", 7, 2004, "Universal")]
-- Cada tupla representa:
-- El nombre de la serie
-- La cantidad de temporadas
-- En qué año se emitió la primera temporada
-- Qué cadena lo produjo

actores = [("Ken Leung", ["lost", "los soprano"]),("Joel Gretsch", ["4400", "V", "United States of Tara"]),("James Gandolfini", ["los soprano"]),("Elizabeth Mitchell", ["dr house", "V", "lost"])]

-- El formato que sigue la tupla es:
-- Nombre del actor
-- Lista de series en las que participó

serie (s, _, _, _) = s
temporadas (_, t, _, _) = t
cadenaTV (_, _, _, c) = c
anioComienzo (_, _, a, _) = a
seriesDeActor = snd
nombreActor = fst
find criterio = head . filter criterio

-- EJERCICIO 1 ----------

type Serie = (String, Int, Int, String)


datosDe:: String -> Serie
datosDe  = head . flip filter series . conNombreIgualA 

-- AUX -- 
conNombreIgualA:: String -> Serie -> Bool
conNombreIgualA nombre = (==) nombre . serie 
-- --- --

-- EJERCICIO 2 ----------

type PerfilActor = (String, [String])

listaDeActoresDe:: String -> [String]
listaDeActoresDe = map nombreActor . perfilesConSerie

-- AUX --
perfilesConSerie:: String -> [PerfilActor]
perfilesConSerie = flip filter actores . estuvoEn

estuvoEn:: String -> PerfilActor -> Bool
estuvoEn nombreSerie  = elem nombreSerie . seriesDeActor
-- --- --

-- EJERCICIO 3 ----------

quienesActuaronEn:: String -> String -> [String] -- serie serie listaActores
quienesActuaronEn serie1 = map nombreActor . perfilesEnAlguna serie1

-- AUX --
perfilesEnAlguna:: String -> String -> [PerfilActor]
perfilesEnAlguna serie1 serie2 = flip filter actores (estuvoEnAlguna serie1 serie2)

estuvoEnAlguna:: String -> String -> PerfilActor -> Bool
estuvoEnAlguna serie1 serie2 perfil = estuvoEn serie1 perfil && estuvoEn serie2 perfil
-- --- --

-- EJERCICIO 4 ----------

anioDeComienzoDe:: String -> Int
anioDeComienzoDe = anioComienzo . datosDe
-- --- --

-- EJERCICIO 5 ----------
queSeriesCumplen unCriterio = map serie . filter unCriterio

conMasDe3Temporadas:: [String]
conMasDe3Temporadas = queSeriesCumplen ((>3) . temporadas) series

conMasDe4Actores:: [String]
conMasDe4Actores = queSeriesCumplen ((>4) . length . listaDeActoresDe . serie) series

conTituloMenorA5:: [String]
conTituloMenorA5 = queSeriesCumplen ((<5) . length . serie) series
-- --- --

-- EJERCICIO 6 ----------
-- a

promedioEntero:: [Int] -> Int
promedioEntero lista = div (sum lista)  (length lista)

promedioGeneral:: Int
promedioGeneral = promedioEntero . map temporadas $ series

anioFin:: Serie -> Int
anioFin unaSerie = anioComienzo unaSerie + temporadas unaSerie

promedio:: (Serie -> Int) -> Int
promedio criterioProm = promedioEntero . map criterioProm $ series


-- div (sum (map parametro series)) (totalSeries series)   solucion Gagliardo

-- EJERCICIO 7 ----------
