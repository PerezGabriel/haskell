data Categoria = Novato | Groso | Estrella deriving (Show, Eq, Enum)

data Usuario =  Nuevo String [Post] | Full String Categoria [Post] deriving (Show, Eq)

type Post = (String, Int)


nick (Nuevo n _) = n
nick (Full n _ _) = n

posts (Nuevo _ p) = p
posts (Full _ _ p) = p

categoria (Full _ c _) = c

---AVAVAVAVAVAVAVAVVAAVVAVAVAVAVAVAVAVAVAAVAVAVAVAVAVAVAVAVAVAVAVAVAVAVAV

----PUNTO 1------------------
-----------------------------
---- auxiliares ----
negativo n = n < 0

esSpam:: Post->Bool
esSpam post = elem "ATENCION" (palabrasMsjePost post)

palabrasMsjePost:: Post->[String]
palabrasMsjePost = words.fst

puntaje:: Post->Int
puntaje = snd
---------------------
puntosBase:: Post->Int
puntosBase post = (puntaje post) * ((length.palabrasMsjePost) post)

esMolesto:: Post->Bool
esMolesto post = ((negativo.puntaje) post ) || (esSpam post)


responder:: Post->String->Post
responder post msje = ( (fst post) ++ (" -- " ++ msje) , 0)
-------------------------------
-------------------------------

---AVAVAVAVAVAVAVAVVAAVVAVAVAVAVAVAVAVAVAAVAVAVAVAVAVAVAVAVAVAVAVAVAVAVAV

--------PUNTO 2 --------------------
------------------------------------
------Aux---------
listaPuntos listaPost = map puntosBase listaPost
noMolesto = not . esMolesto
lenMensajePost = length . fst
caracteresImpares = odd . lenMensajePost

esPopular post = (noMolesto post) && (caracteresImpares post)

postPopulares listaPost = filter esPopular listaPost
------------------

puntosTotales:: [Post]->Int
puntosTotales = sum . listaPuntos

mensajesPopulares:: [Post]->[String]
mensajesPopulares listaPost = map fst (postPopulares listaPost)
-------------------------------------
-------------------------------------

---AVAVAVAVAVAVAVAVVAAVVAVAVAVAVAVAVAVAVAAVAVAVAVAVAVAVAVAVAVAVAVAVAVAVAV

---------PUNTO 3 -------------------
------------------------------------
--------AUX-------
tresPri:: String->String
tresPri string = take 3 string

priMsjePriPost:: [Post]->String
priMsjePriPost = fst . head

littleMsj:: [Post]->String
littleMsj = tresPri . priMsjePriPost

categorizar:: (Bool,Bool)->Categoria
categorizar (False, False) = Groso
categorizar (True , _ ) = Estrella
categorizar _ = Novato


puntosMayCien:: [Post]->Bool
puntosMayCien = (> 100) . puntosTotales

algunMolesto:: [Post]->Bool
algunMolesto listaPosts = elem True (map esMolesto listaPosts)

categorizarFull:: Categoria->Categoria
categorizarFull Estrella = Estrella
categorizarFull categoria = succ categoria
------------------

iniciales:: Usuario->String
iniciales  = tresPri . nick

mensajito:: Usuario->String
mensajito = littleMsj . posts

upgradear:: Usuario->Usuario

upgradear (Nuevo n p) = Full n (categorizar (puntosMayCien p, algunMolesto p)) p
upgradear (Full n c p) = Full n (categorizarFull c ) p
----------------------------------------
----------------------------------------


nahue = Nuevo "Nahuel" [("Hola", 0)]
fer = Full "Fernando" Estrella postsDeFer
postsDeFer = [("PDPelement",3),("eEaer",5)]