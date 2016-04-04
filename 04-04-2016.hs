---- Crear una lista con infinitos elementos---

repetirLista:: a -> [a]
repetirLista elem = [elem]++repetirLista elem

---------------

----- Call by value (eager evaluation "evaluacion ansiosa")
--------- ansia tener los parametros

-- > cuadrado(3+7)
-- > cuadrado 10
-- > 10*10
-- > 100

----- Call by name (lazy evaluation "evaluacion perezosa")
--------- usa los parametros cuando los va necesitando

-- > cuadrado (3+7)
-- > (3+7)*(3+7)
-- > 10*(3+7)
-- > 10*10
-- > 100

-----TAKE-----
tomar:: (Eq a, Num a) => a -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (cabeza:cola) = (cabeza : tomar (n-1) cola)
----

-- > tomar 2 ['a','b',c']
-- > 'a':tomar 1 ['b','c']
-- > 'a':'b':tomar 0 ['c']
-- > 'a':'b:[]
-- > 'a:['b']
-- > ['a,'b']

-------- Naturales desde un n dado
naturalesDesde:: Num a => a->[a]
naturalesDesde n = n : naturalesDesde (n+1)

--- Resolviendo de las 2 maneras
--- Call by value (eager)

-- > take 2 (naturalesDesde 1)
-- > take 2 (1:naturalesDede 2)
-- > take 2 (1:2:naturalesDesde 3)
-- > take 2 (1:2:3 naturalesDesde 4)
-- > se queda sin memoria!

--- Call by name (lazy)

-- > take 2 (naturalesDesde 1)
-- > take 2 (1:naturalesDesde 2) (se fija que no sea vacia para no entrar en la 2da def de take)
-- > 1:tomar 1 [naturalesDesde 2]
-- > 1:2:tomar 0 [naturalesDesde 3] (matchea con la 1er def de take)
-- > 1:2:[]
-- > 1:[2]
-- > [1,2]  (como se ve, arroja resultado)

--- COMO VEMOS, HASKELL LABURA lazy, LO QUE PERMITE LABURAR CON LISTAS INFINITAS.
--ejercicio 15.1 ; 15.4 ; 15.7 ; 15.8	

-- 15.1 

todosLosNaturales:: [Int]
todosLosNaturales = iterate (+1) 1


-- primerosN:: Num a => a -> [a]
primerosN n = take n todosLosNaturales


-- 15.4

repetirPalabras palabra = map (pegarAlgo palabra) todosLosNaturales

pegarAlgo palabra = ((++)palabra).show

--variante repetir palabras pares

repetirPalabrasPares  palabra = map (pegarAlgo palabra) todosLosPares

todosLosPares = filter even todosLosNaturales -- esta es mejor
todosLosPares' = iterate (+2) 0

---------
-- ---- UNA MEJOR ABSTRACCION DEL MAP

-- repetirPalabras':: String -> [String]
-- repetirPalabras' = repetirPalabrasDesde todosLosNaturales

-- repetirPalabrasPares' = repetirPalabrasDesde todosLosPares

-- repetirPalabrasDesde:: Show a => String -> [a] -> [a]
-- repetirPalabrasDesde palabra lista = map (pegarAlgo palabra) lista

----------------

-- 15.7


potenciasDeDos  = map (2^) natMas0

natMas0 = 0:todosLosNaturales

potenciasDeDos' = iterate (*2) 1

---

-- 15.8

-- primeroQueCumple condicion inicio f = findFirst condicion (listaDe incio f)

primeroQueCumple''' c i f = head . filter c . iterate f $ i 

primeroQueCumple'' c i f = ( head . filter c . iterate f ) i

primeroQueCumple' c i  = head . filter c . flip iterate i

primeroQueCumple c i  = primeroFiltrado c . flip iterate i

primeroFiltrado c = head. filter c

-----------

---- 
maximoSegun:: Ord b => (a -> b) -> [a] -> b
maximoSegun f = maximum . map f

-- maximum da el maximo en una lista de Ord

-- maximoSegun (+2):: (Ord a, Num a) a => [a] -> a  (practicando tipos)

-- > maximoSegun (+2) [1..5] 
-- > 7

-- > maximoSegun length ["PDP" , "PHM" , "REDES"]
-- > 5

-- > maximoSegun head ["PDP" , "PHM" , "REDES"]
-- > 'R'
