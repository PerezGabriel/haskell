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

-- ###################################################################################

------------------------------
--------- AUXILIARES ---------
nombre (UnIngrediente nom _ _ _ _) = nom

prom2:: Fractional a => a -> a -> a
prom2 n = (/2) . (+ n)



promediadosCondimento:: Ingrediente->Condimento->Condimento
promediadosCondimento i (dulCond, alcCond, colCond) = ( ((prom2 dulCond).dulzura) i, ((prom2 alcCond).alcohol) i, ((prom2 colCond).color) i)

ingredienteCondimentado (nom, cant) (dul, alc, col) = UnIngrediente nom dul alc col cant
------------------------------
------------------------------

condimentar:: Ingrediente->Condimento->Ingrediente
condimentar i = (ingredienteCondimentado (nombre i, cant i)). (promediadosCondimento i)



condimentar' i (dul, alc, col) = UnIngrediente (nombre i) (((prom2 dul).dulzura) i) (((prom2 alc).alcohol) i) (((prom2 col).color) i) (cant i)



condimentar'' ingre = ingredienteDesdeTupla . (condimentarTuplas (hacerTupla ingre))

hacerTupla i = (nombre i, dulzura i, alcohol i, color i, cant i)

condimentarTuplas (nomb, dulI, alcI, colI, cant) (dul, alc, col) = (nomb, prom2 dul dulI, prom2 alc alcI, prom2 col colI, cant)

ingredienteDesdeTupla (nomb, dul, alc, col, cant) = UnIngrediente nomb dul alc col cant