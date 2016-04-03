--Constantes de prueba
celuDelRasta = ("Samsung S2", False)
fer = ("Fer", "PHM")
miguel = ("Miguel", 1234, "PDP")

--Primeras aproximaciones
estaLiberadoViejo:: ( String, Bool)->Bool
estaLiberadoViejo ( modelo, condicion) = condicion

type Celular = (String, Bool)

estaLiberado:: Celular -> Bool
estaLiberado( _ , condicion) = condicion


modeloCelular:: Celular->String
modeloCelular (modelo, _ ) = modelo

-- otras definiciones
--usando fst and snd

-- definicion declarativa.
estaLiberadoCel:: Celular->Bool
estaLiberadoCel celular = snd celular

-- definicion imperativa.
modeloCelularNoDeclarativa = fst