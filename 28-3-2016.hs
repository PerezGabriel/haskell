------- APLICACION PARCIAL ------


-- por ejemplo (+) es una funcion de 2 parametros, pero (+5) es una funcion de 1 parametro
-- el tipo de (+) es Num a => a-> a -> a, y el tipo de (+5) es Num a => a->a (inferido por haskell)

masCinco:: Num a => a->a
masCinco = (+5)

-- Otro ejemplo: con la funcion max que es de tipo Ord a => a -> a -> a.
-- Pero la funcion (max 'e') es de tipo Char -> Char  (inferido por haskell)
 

maxContraMinusE:: Char -> Char
maxContraMinusE = max 'e'

-- Ahora un ejemplo con varias restricciones:
-- Si vemos el tipo de (max 3) con el comando :t max 3, haskell muestra que el tipo es: 
-- (Num a, Ord a) => a -> a, porque a tiene que ser un numero, y tiene que ser ordenable 
-- (existen los complejos que son numeros pero no ordenables, por eso la 2ble restriccion.)

max3:: (Num a, Ord a) => a->a
max3 = max 3


-- ahora con any, :t any da: (a -> Bool) -> [a] -> Bool
-- en la funcion algunMayor3 se da una aplicacion parcial en el (>3)
-- el tipo de (>) es: Ord a => a -> a -> Bool y en (>3) el tipo es: (Num a, Ord a) => a -> Bool
-- donde el 3 es el 2do parametro del >

algunMayor3 = any (>3)

-- ahora con listas

toma4 = take 4

-- ahora queremos escribir de forma "infija" el take, así podemos usar aplicacion parcial con
-- el segundo parametro

-- aplicacion parcial al 1er parametro
primerasNLetras n = take n ['a'..'z']

-- pasando a infijo  3 `take` [1..10]

-- aca se ven 4 aplicaciones parciales: el mas 2, el menor 6, el map +2 y el filter menor 6
-- en la consola:
-- *Main> :t (map (+2).filter(<6))
--  (map (+2).filter(<6)) :: (Num b, Ord b) => [b] -> [b]

mapFilter lista = (map (+2).filter(<6)) lista

--  :t flip (a -> b -> c) -> b -> a -> c
-- recibe una funcion de 2 parametros y 1 resultado, 2 parametros y aplica la funcion a la inversión de los parametros. 
-- el flip invierte los parametros.

-- aplicacion parcial con flip
-- aca declaro la funcion sin pensar en su uso.

primerasLetras = flip take ['a'..'z']

-- ahora la aplicacion parcial con funciones y listas
-- en esta funcion: map take [1,3,5,6], esto devuelve una lista de funciones.
-- devolveria: [take 1, take 3, take 5, take 6]

mapTake = map take [1,3,5,6]

--ej de invertir parametros de funciones.

aplicarInversa num func = func num

-- convinandolo, en la consola:
-- (map (aplicarInversa "Paradigmas de Programacion") . map take) [1,4,9]
-- ejercicio 12.1

esMultiploDe num1 num2 = ((==0) . (rem num1)) num2

--la solucion mas cheta

multiploDe n = (== 0) . rem n

-- mas de lo mismo. ejercicio 12.5 
esResultadoPar n = even . (n^)

-- haskell logra esto, porque todas las funciones de haskell son de 1 parametro. "todas las funciones de haskell
-- son asociativas a derecha". (algo(algo2(algo3(..)))) como que se anidan las funciones
-- en el caso de la funcion (+) no tiene 2 parametos si no que cn el primero arma otra funcion
-- que recibe 1 parametro.
-- a esto se le llama Currificacion, es muy poderoso pq al fin y al cabo se tiene 1 solo tipo de funcion,
-- las de 1 parametro. Todo esto se sostiene tambien con la idea de que se pueden devolver y tomar funciones.



-------------------------------------------------------------------
---         EXPRESIONES LAMBDA   ----------

-- todos en minus y sin espacios
nombreDeVariableValido = all esMinuscula

esMinuscula char = elem char ['a'..'z']
esMinuscula' = flip elem ['a'..'z'] --- muy buen ejemplo de lo visto antes


--declaracion de funcion in-situ. La declaro donde la voy a usar.
esMinuscula'' = all (\char -> elem char ['a'..'z'])

-- otro ejemplo de esto en consola: (\n-> n>10 && n<20)

-- el :t de esa funcion: ((Num a, Ord a) => a -> Bool)
-- a esas funciones se les llama "Expresion Lambda", esto deriva del "Calculo Lambda"


-- por ejemplo haskell ve la funcion esMultiploDe de la sig manera:
-- esMultiploDe = (\n -> (==0).mod n)
-- con dos parametros: 
-- esMultiploDe =(\n -> (\m -> ((==0).mod n)m))

-----------------------------------------------------------------------


-----              FOLD                       --------------
-- fold recibe: funcion semilla lista, devuelve: semilla

-- ejemplo aplicacion:  foldl comer persona comidas

-- en consola: 
-- *Main> foldl (+) 0 [1..4]
--	10
-- *Main> foldl1 (+) [1..4]
--	10
-- igual que el sum  [1..4]



-----------------------------------------------------

data CuentaBancaria = Caja Float deriving (Show, Eq)

depositar monto (Caja actual) = Caja (actual + monto)

extraer monto (Caja actual) = Caja (actual - monto)


cajaPobre = Caja 0

chequesACobrar = [100, 150.5, 2301] :: [Float]

cobrarTodo:: CuentaBancaria -> [Float] -> CuentaBancaria
cobrarTodo cajaInicial cheques =  foldl (\caja cheque -> depositar cheque caja) cajaInicial cheques

cobrarTodo* = foldl (flip depositar)  ---- MIERDA QUE ES ESTO


--mas inteligentemente hecho
cobrarTodo' caja cheques = depositar (sum cheques) caja


-- cobrarTodo'' caja = flip depositar caja . sum

-- le probamos:  (extraer 45.6 . depositar 100) cajaPobre
