data Persona = Tranqui String | Agitador Int Int String deriving (Show, Eq)

fer = Tranqui "fer"
flor = Tranqui "flor"
rodri = Agitador 1670 2000 "rodri"
deby = Agitador 7000 1500 "deby"

type Disco = [ (Persona, [(String, Int)])]

laDisco :: Disco
laDisco = [(fer, [( "Coca cola" , 1 ), ( "Sprite Zero" , 1 )]),(rodri, [( "Cerveza" , 2 )]),(deby, [( "Grog XD" , 25 ), ( "Cerveza" , 1 )]),(flor, [( "Grapa" , 1 )])]

tragos = [( "Coca cola" , 0 ), ( "Grog XD" , 350 ),( "Sprite Zero" , 0 ), ( "Cerveza" , 10 ), ( "Grapa" , 40 )]



-- EJERCICIO 1 ----------
type Trago = (String, Int)
type TragoConsumido = (String, Int) -- representan diferentes cosas

type PersonaEnDisco = (Persona, [TragoConsumido])

bebidas:: Disco -> Persona -> [TragoConsumido]
bebidas disco = snd . personaEnBoliche disco

graduacionAlcoholica:: String -> Int
graduacionAlcoholica = graduacion . tragoDeNombre

alcoholEnSangre:: Disco -> Persona -> Int -- sumatoria de graduacion de los tragos que bebio por cantidad bebida
alcoholEnSangre disco = sum . map cantidadPorGraduacion . bebidas disco

-- AUX --
cantidadPorGraduacion:: TragoConsumido -> Int
cantidadPorGraduacion trago =( graduacion . tragoDeNombre . fst ) trago * snd trago

personaEnBoliche:: Disco -> Persona -> PersonaEnDisco
personaEnBoliche disco = head . flip filter disco . esElDeLaJoda 

esElDeLaJoda:: Persona -> PersonaEnDisco -> Bool
esElDeLaJoda persona = (==) persona . fst

tragoDeNombre:: String -> Trago
tragoDeNombre = head . flip filter tragos . esElTrago

esElTrago:: String -> Trago -> Bool
esElTrago nombre = (==) nombre . nombreTrago

nombreTrago:: Trago -> String
nombreTrago (n, _) = n

graduacion:: Trago -> Int
graduacion ( _ , alc) = alc

-- --- --

-- EJERCICIO 2 ----------

estaBorracha:: Persona -> Bool
estaBorracha = esPositivo . soportadoMenosIngerido 

impresentables:: [Persona] -> [Persona]
impresentables = filter  estaBorrachaOPidioMasDe5 

-- AUX --
estaBorrachaOPidioMasDe5:: Persona -> Bool
estaBorrachaOPidioMasDe5 persona = estaBorracha persona || consumioMucho laDisco persona -- le paso laDisco

consumioMucho:: Disco -> Persona -> Bool
consumioMucho disco = (>5) . length . bebidas disco

soportadoMenosIngerido:: Persona -> Int
soportadoMenosIngerido ppl = aguante ppl - alcoholEnSangre laDisco ppl -- le fuerzo laDisco, todo por usar el bebidas dentro de una disco

esPositivo:: (Int -> Bool)
esPositivo =( > 0)

aguante:: Persona -> Int
aguante (Agitador _ aguante _) = aguante
aguante (Tranqui _ ) = 0
-- --- --

-- EJERCICIO 3 ----------
type Plan = [ Disco -> Disco]

entrar:: Persona -> Disco -> Disco
entrar persona = (++) [(persona, [])]

pedirBebida:: String -> Persona -> Disco -> Disco
pedirBebida trago persona disco = [actualizarBebidas trago persona disco] ++ discoSin persona disco

descontrolarse:: Persona -> Disco -> Disco
descontrolarse persona disco = aplicar (planDescontrol ((/2). aguante $ persona)) persona $ disco

planDeCumple:: Persona -> Plan
planCumple persona = rutinaCumple persona

festajarCumple:: Persona -> Disco -- los maximos los debo
festejarCumple persona = aplicar (aplicar (rutinaCumple persona) persona) laDisco

salidaSana:: Persona -> Plan -- toda la que se pueda entiendo hasta q explote la memoria
salidaSana = [entrar persona] ++ repeat (pedirBebida "Agua" persona)

-- AUX --
planDescontrol:: Int -> [Persona -> Disco -> Disco]
planDescontrol cant = take cant . repeat  $ pedirBebida "Grog XD"

rutinaCumple persona = [pedirBebida "Fernet" persona, pedirBebida "Campari" persona, descontrolarse persona, pedirBebida "Agua" persona]

discoSin:: Persona -> Disco -> Disco
discoSin persona = filter (not . esElDeLaJoda persona) 

agregarBebida:: PersonaEnDisco -> TragoConsumido -> PersonaEnDisco
agregarBebida persona trago = (fst persona, snd persona ++ [trago])

convertirATrago:: String -> TragoConsumido
convertirATrago nomb = (nomb, 0)

actualizarBebidas:: String -> Persona -> Disco -> PersonaEnDisco
actualizarBebidas trago persona disco = (persona, bebidas disco persona ++ [convertirATrago trago])


aplicar (x:xs) elemento = (x elemento) : aplicar xs


-- las respuestas: el largo no lo podemos saber, la salida se ve porque haskell trabaja lazy