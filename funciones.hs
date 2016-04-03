--Constantes de prueba
materiasDeNico = ["PHP","PDP","Proyecto"]
listaVacia = []

tieneMaterias:: [String]->Bool
tieneMaterias [] = False
tieneMaterias _ = True

-- definiendo el len de una lista (mal)
cantidadMaterias:: [String]->Int
cantidadMaterias [] = 0
cantidadMaterias [materia] = 1
cantidadMaterias [materia1, materia2] = 2

--definiendo recursivamente

cantMaterias:: [String]->Int
cantMaterias [] = 0
cantMaterias (cabeza : cola) = 1 + cantMaterias cola

-- idea de Cabeza-cola (una lista definida como cabeza-cola)
materias = ("matematicas":["lengua","historia"])

--Obtener el primer elemento

primeroLista:: [String]-> String
primeroLista [] = "Error: La lista esta vacia"
primeroLista (cabeza: cola) = cabeza



-- FILTER CON CELULARES
type Celular = (String, Bool)
estaLiberado:: Celular->Bool
estaLiberadoCel celular = snd celular
modeloCel:: Celular->String
modeloCel = fst

--tuplas
celuRasta = ("Sansung", True)
celuRolo = ("Motorola",False)

--Listas de tuplas
celulares = [celuRasta,celuRolo]

--funcion como parametros
type condicionFiltrado
--filtrar celus
filtrarCel:: (Celular->Bool)->[Celular]->Celular]
filtrarCeluCondicion:: condicionFiltrado->[Celular]->Celular]


--filter generico
filter:: (a->Bool) ->[a]->[a]


--Filtrando una lista de celulares

celusLiberados:: [Celular]->[Celular]


--contains
contains:: elemento->[elemento]->Bool

--Proyectar map de smalltalk
map:: (a->b)->[a]->[b]

