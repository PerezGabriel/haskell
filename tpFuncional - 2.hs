data Ingrediente = UnIngrediente String Float Float Float Float deriving (Show,Eq)

dulzura (UnIngrediente _ dul _ _ _)   = dul
alcohol (UnIngrediente _ _ alc _ _)   = alc
color   (UnIngrediente _ _ _ col _)   = col
cant    (UnIngrediente _ _ _ _ cant)  = cant

vodka10   = UnIngrediente "vodka" 10  55 0 10
vodka50   = UnIngrediente "vodka" 10  55 0 50
vodka100  = UnIngrediente "vodka" 10  55 0 100
speed80   = UnIngrediente "speed" 30 1 10 80
fernet20  = UnIngrediente "fernet" 10 10 50 20
fernet50  = UnIngrediente "fernet" 10 10 50 50
hielo30   = UnIngrediente "hielo" 0 0 0 30
coca50    = UnIngrediente "cocaCola" 100  0 80 50
naranja50 = UnIngrediente "jugoDeNaranja" 70  0 30 50


type Condimento = (Float, Float, Float)

azucar = (80, 5, 0) :: Condimento
colorante = (15, 0, 100) :: Condimento

-- ---·-----·-----·-----·EJERCICIO 1-----·-----·-----·-----··
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 

ingreTest = [vodka10,naranja50,speed80]

-- ---·-----·AUXILIARES-----·-----
-- ---·-----·-----·-----·-----·---

nombre (UnIngrediente nom _ _ _ _) = nom

prom2:: Fractional a => a -> a -> a
prom2 n = (/2) . (+ n)

ingredienteFromTupla (n, d, a, co, ca) = UnIngrediente n d a co ca

condimentarIngredienteATupla i (d, a, co) = (nombre i, prom2 d (dulzura i), prom2 a (alcohol i), prom2 co (color i), cant i )

maximo:: (a->Float) -> [a]-> Float
maximo f [] = 0.0
maximo f (x:[]) = f x
maximo f (x:xs) = max (maximo f xs) (f x)

condimentoMaximo:: [Ingrediente]->Condimento
condimentoMaximo listIng = (maximo dulzura listIng, maximo alcohol listIng, maximo color listIng) :: Condimento

mezclarExcluyente:: Ingrediente->[Ingrediente]->Ingrediente
mezclarExcluyente elemento = mezclar elemento . removeItem elemento

removeItem:: Eq a => a -> [a] -> [a]
removeItem x = filter (not.(==) x)
-- ---·-----·-----·-----·-----·---
-- ---·-----·-----·-----·-----·---

condimentar:: Ingrediente->Condimento->Ingrediente
condimentar i = ingredienteFromTupla . (condimentarIngredienteATupla i)

mezclar:: Ingrediente->[Ingrediente]->Ingrediente
mezclar i = condimentar i . condimentoMaximo 

batir::[Ingrediente]->[Ingrediente]
batir ingredientes = map (`mezclarExcluyente` ingredientes) ingredientes
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 


-- ---·-----·-----·-----·EJERCICIO 2-----·-----·-----·-----··
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 

type Armadora = [Ingrediente] -> [Ingrediente]

-- ---·-----·AUXILIARES-----·-----
-- ---·-----·-----·-----·-----·---

crearHielo::Int -> Ingrediente
crearHielo cantHielos = UnIngrediente ("hielo") 0 0 0 (fromIntegral cantHielos*10)

agregar5Hielos::Armadora
agregar5Hielos = (++) [crearHielo 5] 

agregarAzucar::Armadora
agregarAzucar = map (flip condimentar azucar)

paridadSegundosAgita::Bool -> Armadora
paridadSegundosAgita False  =  (++) [crearHielo 2] 
paridadSegundosAgita _ = licuadora

flameado::Int -> Ingrediente -> Ingrediente
flameado seg (UnIngrediente nomb dul alc col cant) = UnIngrediente nomb (dul+2) (alc/2) (col+5) (cant - fromIntegral seg*0.1)

flamearPrimero::Int -> Armadora
flamearPrimero segAgita (x:xs) = (flameado segAgita) x : xs

-- ---·-----·-----·-----·-----
-- ---·-----·-----·-----·-----

directo::Int -> Armadora
directo cantHielos = (++) [crearHielo cantHielos]

licuadora::Armadora
licuadora = batir.agregar5Hielos.agregarAzucar

coctelera::Bool -> Int -> Armadora
coctelera False segAgita  = paridadSegundosAgita (even segAgita)
coctelera _ segAgita = flamearPrimero segAgita
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 


-- ---·-----·-----·-----·EJERCICIO 3-----·-----·-----·-----··
-- ---·-----·-----·-----·-----·-----·-----·-----·-----·-----· 

data Trago = UnTrago String [Ingrediente] deriving (Show,Eq)

ingredientes (UnTrago _ ingredientes) = ingredientes


data Persona = UnaPersona String Float Float [Trago] deriving (Show,Eq)

resistencia (UnaPersona _ res _ _)  = res
ebriedad    (UnaPersona _ _ ebr _)  = ebr
tragos      (UnaPersona _ _ _ trgs) = trgs

fernerConCoca   = UnTrago "Branca" [ hielo30, fernet50, coca50]
destornillador  = UnTrago "Destornillador" [hielo30, naranja50, vodka50]
speedConVodka   = UnTrago "Speed con vodka" [ vodka10,  speed80]
puroVodka       = UnTrago "Vodka" [ hielo30,  vodka100]
vodkaMenta      = UnTrago "Vodka Menta" [ vodka50,  fernet20 ]

charlySheen = UnaPersona "charly" 98 50 [ puroVodka , fernerConCoca ]
chuckNorris = UnaPersona "chuck" 100 0 [ vodkaMenta ]
funesMori   = UnaPersona "el memorioso" 0 75 [ speedConVodka ]

-- ---·-----·AUXILIARES-----·-----
-- ---·-----·-----·-----·-----·---

armadorasFijas::[Armadora]
armadorasFijas = [directo 10, licuadora, directo 5, coctelera True 10]

allCoctelerasSinFlamb:: Int -> [Armadora]
allCoctelerasSinFlamb n = [coctelera False n] ++ allCoctelerasSinFlamb (n+1)

apodo:: Persona -> String
apodo (UnaPersona nick _ _ _) = nick

nombTrago:: Trago -> String
nombTrago (UnTrago nomb _) = nomb

escabio::Trago -> Float
escabio = promedio.alcoholIngredientes.ingredientes

alcoholIngredientes::[Ingrediente]->[Float]
alcoholIngredientes = map alcohol

promedio::Fractional a => [a] -> a -- avg no me anduvo
promedio lista = sum lista / fromIntegral (length lista)

agregarResistencia:: Int -> Persona -> Persona
agregarResistencia n guy = UnaPersona (apodo guy) (resistencia guy + fromIntegral n) (ebriedad guy) (tragos guy)

ingerirTrago:: Persona -> Trago -> Persona
ingerirTrago persona = subirAlcoholEnSangre.(recordarTrago persona)

recordarTrago:: Persona -> Trago ->(Persona, Float)
recordarTrago guy trago= (agregarTrago guy trago, escabio trago )

agregarTrago:: Persona -> Trago -> Persona
agregarTrago guy trago = UnaPersona (apodo guy) (resistencia guy) (ebriedad guy) ( [trago] ++ tragos guy)

subirAlcoholEnSangre:: (Persona, Float) -> Persona
subirAlcoholEnSangre  (guy, alc) = UnaPersona (apodo guy) (resistencia guy) (alc + ebriedad guy) (tragos guy)

armarTrago:: Trago -> Armadora -> Trago
armarTrago trago armadora= UnTrago (nombTrago trago) (armadora.ingredientes $ trago)

armarN:: Trago -> Int -> [Trago]
armarN trago = map (armarTrago trago) . flip take armadorasDeLaCasa

beberTodos:: Persona -> [Trago] -> Persona
beberTodos persona = foldl beber persona
-- ---·-----·-----·-----·-----
-- ---·-----·-----·-----·-----
armadorasDeLaCasa::[Armadora]
armadorasDeLaCasa = armadorasFijas ++ allCoctelerasSinFlamb 1

beber::Persona -> Trago -> Persona
beber persona = agregarResistencia 2 . ingerirTrago persona

degustar:: Persona -> Trago -> Int -> Persona
degustar persona trago = beberTodos persona . armarN trago
 

















----- OTRAS SOLUCIONES --------
----EJERCICIO 1--------
promediadosCondimento i (dulCond, alcCond, colCond) = ( ((prom2 dulCond).dulzura) i, ((prom2 alcCond).alcohol) i, ((prom2 colCond).color) i)
ingredienteCondimentado' (nom, cant) (dul, alc, col) = UnIngrediente nom dul alc col cant
hacerTupla i = (nombre i, dulzura i, alcohol i, color i, cant i)
condimentarTuplas (nomb, dulI, alcI, colI, cant) (dul, alc, col) = (nomb, prom2 dul dulI, prom2 alc alcI, prom2 col colI, cant)
ingredienteDesdeTupla (nomb, dul, alc, col, cant) = UnIngrediente nomb dul alc col cant

condimentar''':: Ingrediente->Condimento->Ingrediente
condimentar''' i = (ingredienteCondimentado' (nombre i, cant i)). (promediadosCondimento i)
condimentar' i (dul, alc, col) = UnIngrediente (nombre i) (((prom2 dul).dulzura) i) (((prom2 alc).alcohol) i) (((prom2 col).color) i) (cant i)
condimentar'' ingre = ingredienteDesdeTupla . (condimentarTuplas (hacerTupla ingre))
------------------------------