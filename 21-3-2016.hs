

-- Creando Tipos

--aca creamos el Data Dia, y Lunes, Mart. etc son Constructores
--Con el deriving le digo que se pueden showear y comparar por iwal, el enum los ordena
data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show,Eq, Enum)
esMalo Lunes = True
esMalo _ = False


esFinde:: Dia->Bool
esFinde Viernes = True
esFinde Sabado = True
esFinde Domingo = True
esFinde _ = False

-- otra manera de definirlo, se necesita el Eq
--En esta definicion guardo en una lista los dias de finde
esFinde' dia = elem dia [Viernes,Sabado,Domingo]


-- Ahora con valores compuestos
--en este caso Perro es el Constructor de la clase Animal. -- en el caso del sapo el bool indicaria si es macho
data Animal = Perro String Int | Sapo String Bool Int deriving (Show)

nombre:: Animal-> String
nombre (Perro nombre _ ) = nombre
nombre (Sapo nombre _ _) = nombre


edad:: Animal->Int
edad (Perro _ age) = 7 * age
edad (Sapo _ _ age) = age

--en este caso tengo que crear de nuevo el animal.
pasarUnAnio:: Animal->Animal
pasarUnAnio (Perro nombre edad) = Perro nombre (edad+1)

elPerrito = Perro "Kalif" 10


sapito = Sapo "Pepe" True 15