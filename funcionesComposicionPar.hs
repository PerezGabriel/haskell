elDos:: Int
elDos = 2

esParViejo numero = mod numero 2 == 0
esPar:: Int->Bool
esPar = even

esImpar:: Int->Bool
esImpar numero = not (esPar numero)

siguienteViejo numero = numero + 1
siguiente:: Int->Int
siguiente = succ

esNombreCopadoViejo nombre = esPar (length nombre)
esNombreCopado:: String->Bool
esNombreCopado = esPar.length