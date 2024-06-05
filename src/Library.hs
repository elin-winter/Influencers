module Library where
import PdePreludat

-- ------------------------- Dominio --------------------------

data Persona = UnaPersona {
    gustos :: [Gusto],
    miedos :: [Miedo],
    estabilidad :: Estabilidad
}

-- ------------------------- Definición de Tipos --------------------------
type Gusto = String
type Miedo = (String, Number)
type Estabilidad = Number

type Accion = Persona -> Persona
-- ------------------------- Modelaje (Ejemplos) --------------------------

maria :: Persona
maria = UnaPersona {
    gustos = ["Mecanica"],
    miedos = [("Extraterrestres", 600), ("Quedarse sin trabajo", 300)], 
    estabilidad = 85
    }

juanCarlos :: Persona
juanCarlos = UnaPersona {
    gustos = ["Maquillaje", "Trenes"],
    miedos = [("Insectos", 100), ("Coronavirus", 10), ("Vacunas", 20)], 
    estabilidad = 50
    }

-- ------------------------- Funciones Genericas --------------------------
-- ----------------- Personas
gustosSegunF :: ([Gusto] -> [Gusto]) -> Persona -> Persona
gustosSegunF f persona = persona {gustos = f . gustos $ persona}

miedosSegunF :: ([Miedo] -> [Miedo]) -> Persona -> Persona
miedosSegunF f persona = persona {miedos = f . miedos $ persona}

estabilidadSegunF :: (Estabilidad -> Estabilidad) -> Persona -> Persona
estabilidadSegunF f persona = persona {estabilidad = escala . f . estabilidad $ persona}

escala :: Number -> Number
escala = min 100 . max 0

-- ----------------- Miedos
nombreMiedos :: [Miedo] -> [String]
nombreMiedos = map fst

cantMiedoSegunF :: (Number -> Number) -> Miedo -> Miedo
cantMiedoSegunF f (nombre, cant) = (nombre, f cant)

-- ----------------- Cualquiera
encontrarAlgo :: (a -> Bool) -> a ->  [a] -> a
encontrarAlgo cond gusto = head . filter cond

-- ------------------------- Funciones --------------------------
-- ---------------- Parte 1
-- Funcion 1
hacerMiedosa :: Miedo -> Accion
hacerMiedosa miedo = miedosSegunF (agregarMiedo miedo)

agregarMiedo :: Miedo -> [Miedo] -> [Miedo]
agregarMiedo miedo miedos = 
    nuevoMiedo miedo miedos : miedosSin miedo miedos

nuevoMiedo :: Miedo -> [Miedo] -> Miedo
nuevoMiedo miedo miedos
    | estaMiedo miedo miedos = cantMiedoSegunF (+ snd miedo) (encontrarMiedo miedo miedos)
    | otherwise = miedo 

estaMiedo :: Miedo -> [Miedo] -> Bool
estaMiedo miedo miedos = elem (fst miedo) (nombreMiedos miedos) 

encontrarMiedo :: Miedo -> [Miedo] -> Miedo
encontrarMiedo miedo = encontrarAlgo ((fst miedo ==) . fst) miedo

miedosSin :: Miedo -> [Miedo] -> [Miedo]
miedosSin miedo = filter ((fst miedo /=) . fst)

-- Funcion 2
perderMiedo :: Miedo -> Accion
perderMiedo miedo = miedosSegunF (miedosSin miedo)

-- Funcion 3
variarEstabilidad :: (Estabilidad -> Estabilidad) -> Accion
variarEstabilidad = estabilidadSegunF

-- Funcion 4
volverseFan :: Persona -> Accion
volverseFan influencer = gustosSegunF (copiarGustos influencer)

copiarGustos :: Persona -> [Gusto] -> [Gusto]
copiarGustos influencer gustosFan = gustos influencer ++ gustosFan

-- Funcion 5
esFanaticaDe :: Gusto -> Persona -> Bool
esFanaticaDe gusto = (>3) . length . encontrarGustos gusto . gustos 

encontrarGustos :: Gusto -> [Gusto] -> [Gusto]
encontrarGustos gusto = filter (gusto ==) 

-- Funcion 6
esMiedosa :: Persona -> Bool
esMiedosa = (>1000) . sum . map snd . miedos

