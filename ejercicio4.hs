-- 4.1
sumarSegun:: Num b => (a->b) -> [a] -> b
sumarSegun f lista = sum (map f lista)

-- 4.2
exists:: (a->Bool) -> [a] -> Bool
exists  f = or . map f

-- 4.3
esCapicua:: [String] -> Bool
esCapicua lista = (concat lista) == (opuesto lista)

opuesto:: [String] -> String
opuesto = reverse.concat

-- 4.4
esMultiploDe:: Int -> Int -> Bool -- ya esta definida
esMultiploDe numero = (== 0) . rem numero

esMultiploDeAlguno:: Int -> [Int] -> Bool
esMultiploDeAlguno  num = any (== True) . verificaMultiplos num

verificaMultiplos:: Int -> [Int] -> [Bool]
verificaMultiplos num = map (esMultiploDe num)

-- 4.5
cuandoHabloMas':: ([Int], [Int]) -> String
cuandoHabloMas' (normal, reducido) = mayorTiempoConsumido (sum normal, sum reducido)

mayorTiempoConsumido:: (Int, Int) -> String
mayorTiempoConsumido (normal, reducido) = mayorNormal (normal > reducido)

mayorNormal:: Bool -> String
mayorNormal True = "normal"
mayorNormal _ = "reducido"

-- 4.6
cuandoHizoMasLlamadas:: ([Int], [Int]) -> String
cuandoHizoMasLlamadas = verificarLlamadas length

cuandoHizoLaLLamadaMasLarga:: ([Int], [Int]) -> String
cuandoHizoLaLLamadaMasLarga = verificarLlamadas maximum

cuandoHizoMasLLamadasBreves:: ([Int], [Int]) -> String
cuandoHizoMasLLamadasBreves = verificarLlamadas totalLlamadasBreves

cuandoHabloMas:: ([Int], [Int]) -> String
cuandoHabloMas = verificarLlamadas sum

-- AUXILIARES --
verificarLlamadas:: ([Int] -> Int) -> ([Int], [Int]) -> String
verificarLlamadas f (normal, reducido) = cuandoMas (f normal, f reducido)

cuandoMas:: (Int, Int) -> String
cuandoMas (normal, reducido) = mayorNormal (normal > reducido)

totalLlamadasBreves:: [Int] -> Int
totalLlamadasBreves = length . (filter (== True)) . map llamadaBreve 

llamadaBreve:: Int -> Bool
llamadaBreve = (>) 2

-- 4.7
promedios:: Fractional a => [[a]] -> [a]
promedios = map average

average:: Fractional a => [a] -> a -- avg no me anduvo
average lista = sum lista / fromIntegral (length lista)

-- 4.8
promediosSinAplazos:: [[Double]] -> [Double]
promediosSinAplazos = map promedio

promedio:: [Double] -> Double
promedio = average . filter (mayorOIgualA 4)

mayorOIgualA:: Ord a => a -> a -> Bool
mayorOIgualA a = not . (>) a

-- 4.9
mejoresNotas::[[Int]] -> [Int]
mejoresNotas = map maximum

-- 4.10
aprobo':: [Int] -> Bool
aprobo' =  ningunaBaja . notas

ningunaBaja:: [Bool] -> Bool
ningunaBaja = not . any (== False)

notas':: [Int] -> [Bool]
notas' = map (mayorOIgualA 4)
-- otra solucion
aprobo:: [Int] -> Bool
aprobo =  all (== True) . notas

notas:: [Int] -> [Bool]
notas = map (mayorOIgualA 4)

-- 4.11
aprobaron:: [[Int]] -> [[Int]]
aprobaron  = filter aprobo

-- 4.12
divisores:: Integral a => a -> [a]
divisores n = filter (divisorDe n) [1..n]

divisorDe:: Integral b => b -> b -> Bool
divisorDe n = (==) 0 . mod n

-- 4.13
hayAlgunNegativo:: [Int] -> Bool
hayAlgunNegativo = any (<0)

-- 4.14
sumaF:: Num a => [(a -> a)] -> a -> a
sumaF valor = sum . aplicarFunciones valor

aplicarFunciones:: Num a => [(a -> a)] -> a -> [a]
aplicarFunciones [] _ = []
aplicarFunciones (x:xs) v = x v: aplicarFunciones xs v

-- 4.15
contar:: (a -> Bool) -> [a] -> Int
contar f lista  = (length . filtraConDupla) (f,lista)

filtraConDupla:: ((a -> Bool), [a]) -> [a]
filtraConDupla (f, lista) = filter f lista

-- 4.16
rechazar:: (a -> Bool) -> [a] -> [a]
rechazar f lista = filter (not . f) lista 
 
-- 4.17
promedioEntero:: [Int] -> Int
promedioEntero lista = (div . sum) lista . length $ lista

-- 4.18
average':: Fractional a => [a] -> a
average' lista = ((/) . sum ) lista . fromIntegral.length $ lista

-- 4.19
promedioSegun:: (a -> Double) -> [a] -> Double
promedioSegun f = promedio . map f

-- 4.20
contiene:: Eq a => a -> [a] -> Bool
contiene elemento = porlomenos1En . filter (== elemento) 

porlomenos1En:: [a] -> Bool
porlomenos1En [] = False
porlomenos1En _ = True

listaIguales:: Eq a => a -> [a] -> [a]
listaIguales e  = filter (== e)


igual::Eq a => a -> a -> Bool
igual  = (==)

-- 4.21
rotar:: [a] -> [a]
rotar[] = []
rotar (cabeza:cola) = cola ++ [cabeza]

-- 4.22
iniciales:: String -> String
iniciales = map head . eliminarUniLetras . words

eliminarUniLetras:: [String] -> [String]
eliminarUniLetras = filter ((>1).length)

-- 4.23
-- esta es como el fold
-- pam'::[a -> a] -> a -> a
-- pam' [] semilla = semilla
-- pam' (x:xs) semilla = pam xs . x $ semilla

pam:: [a -> b] -> a ->[b]
pam [] val = []
pam (x:xs) val = [x val] ++ pam xs val

-- 4.24
armarFixture'::Eq a => [a] -> [a] -> [(a,a)]
armarFixture'  (x:xs) (y:ys) = (chekEquipos x y) ++ (armarFixture xs ys )
armarFixture' [] [] = []
chekEquipos unEquipo otroEquipo = sonElMismo (unEquipo == otroEquipo, unEquipo, otroEquipo)
sonElMismo (True, _ , _) = []
sonElMismo (_, a, b) = [(a,b)]

armarFixture :: Eq a => [a] -> [a] -> [(a, a)]
armarFixture l1 l2 = filtrarIncoherentes (zipWith  tuplar l1 l2)

filtrarIncoherentes:: Eq a => [(a, a)] -> [(a, a)]
filtrarIncoherentes = filter elementosDistintos

elementosDistintos:: Eq a => (a,a) -> Bool
elementosDistintos (a, b) = not . (==a) $ b

tuplar:: a -> a -> (a,a)
tuplar a b = (a,b)