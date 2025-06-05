module Library where
import PdePreludat


anio :: Number -> Bool
anio numero = (mod numero 4 == 0) && (mod numero 100 /= 0 || mod numero 400 == 0) 

-- 1) 

data Artefactos = UnArtefacto {
    nombreArtefacto :: String , 
    rareza :: Number 
} deriving (Show, Eq)


data Heroe = UnHeroe {
    nombreHeroe :: String,
    epiteto :: String, 
    reconocimiento :: Number, 
    artefactos :: [Artefactos], 
    tarea :: [TareaDescrita]
} deriving (Show, Eq)


--2) 
lanzaDelOlimpo :: Artefactos
lanzaDelOlimpo = UnArtefacto "Lanza del Olimpo" 100
xiphos :: Artefactos
xiphos = UnArtefacto "Xiphos" 50
relampago :: Artefactos 
relampago = UnArtefacto "El relampago de Zeus" 500 
pasarHistoria :: Heroe -> Heroe
pasarHistoria heroe
  | reconocimiento heroe > 1000 = heroe { epiteto = "El mítico"}
  | reconocimiento heroe >= 500 = heroe { epiteto = "El magnífico", artefactos = artefactos heroe ++ [lanzaDelOlimpo] }
  | reconocimiento heroe > 100 = heroe { epiteto = "Hoplita", artefactos = artefactos heroe ++ [xiphos] }
  | otherwise = heroe

encontrarArtefacto :: Artefactos -> Heroe -> Heroe
encontrarArtefacto artefacto heroe = heroe {
  reconocimiento = reconocimiento heroe + rareza artefacto,
  artefactos = artefactos heroe ++ [artefacto]
}


escalarOlimpo :: Heroe -> Heroe
escalarOlimpo heroe = 
  heroe {
    reconocimiento = reconocimiento heroe + 500,
    artefactos = filter (\a -> rareza a >= 1000) 
                  (map (\a -> a { rareza = rareza a * 3 }) (artefactos heroe ++ [relampago]))
  }

ayudarCruzarCalle :: Number -> Heroe -> Heroe 
ayudarCruzarCalle cruce heroe = heroe {epiteto = "Gros" ++ replicate cruce 'o'} 

data Bestia = UnaBestia {
    nombreBestia :: String ,
    fuerza :: Number, 
    debilidadArfactacto :: Artefactos 
}deriving (Show, Eq)

minotauro :: Bestia 
minotauro = UnaBestia "Minotauro" 100 (UnArtefacto "Hilo de Ariadna" 50)

matarBestia :: Heroe -> Bestia -> Heroe 
matarBestia heroe bestia 
 | elem (debilidadArfactacto bestia) (artefactos heroe) = heroe {epiteto = "El asesino de " ++ nombreBestia bestia}
 | fuerza bestia < reconocimiento heroe = heroe {epiteto = "el asesino de " ++ nombreBestia bestia}
 | otherwise = heroe {epiteto = "El cobarde", artefactos = tail (artefactos heroe) }

 -- 3) 

data TareaDescrita = UnaTarea {
  descripcion :: String,
  accion :: Heroe -> Heroe
} deriving (Show, Eq)
-- 4) 

heracles :: Heroe
heracles = UnHeroe "Hercules" "Guardian del Olimpo" 700 [UnArtefacto "Pistola" 1000, UnArtefacto "Relampago de Zeus" 700] []

-- 5) 

matarLeonNemea :: Heroe -> Heroe
matarLeonNemea heroe 
 | length (epiteto heroe) >= 20 = heroe { epiteto = "El asesino del Leon de Nemea"}
 | otherwise = heroe { epiteto = "El cobarde" , artefactos = tail (artefactos heroe) }

-- 6) 

hacerTarea :: TareaDescrita -> Heroe -> Heroe 
hacerTarea (UnaTarea desc accion) heroe = (accion heroe) { tarea = tarea heroe ++ [UnaTarea desc accion] }

-- 7)

-- Artefactos de prueba
armadura :: Artefactos
armadura = UnArtefacto "Armadura" 150

escudo :: Artefactos
escudo = UnArtefacto "Escudo" 70

hojaDeOro :: Artefactos
hojaDeOro = UnArtefacto "Hoja de Oro" 200

-- Tareas de prueba
tarea1 :: TareaDescrita
tarea1 = UnaTarea "Derrotar al minotauro" (\heroe -> heroe { reconocimiento = reconocimiento heroe + 300 })

tarea2 :: TareaDescrita
tarea2 = UnaTarea "Rescatar al aldeano" (\heroe -> heroe { reconocimiento = reconocimiento heroe + 100 })

tarea3 :: TareaDescrita
tarea3 = UnaTarea "Escalar el Olimpo" escalarOlimpo

-- Héroes para probar

heroeA :: Heroe
heroeA = UnHeroe {
  nombreHeroe = "Heracles",
  epiteto = "Guerrero Fuerte",
  reconocimiento = 100,
  artefactos = [],
  tarea = []
}

heroeB :: Heroe
heroeB = UnHeroe {
  nombreHeroe = "Perseo",
  epiteto = "Cazador de Monstruos",
  reconocimiento = 100,
  artefactos = [],
  tarea = []
}


sumaRarezas :: Heroe -> Number
sumaRarezas heroe = sum (map rareza (artefactos heroe))

intercambiarTareas :: Heroe -> Heroe -> (Heroe, Heroe)
intercambiarTareas heroe1 heroe2 =
  (foldl (flip hacerTarea) heroe1 (tarea heroe2),
   foldl (flip hacerTarea) heroe2 (tarea heroe1))

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir heroe1 heroe2
  | reconocimiento heroe1 > reconocimiento heroe2 = (heroe1, heroe2)
  | reconocimiento heroe1 < reconocimiento heroe2 = (heroe2, heroe1)
  | sumaRarezas heroe1 > sumaRarezas heroe2 = (heroe1, heroe2)
  | sumaRarezas heroe1 < sumaRarezas heroe2 = (heroe2, heroe1)
  | otherwise = intercambiarTareas heroe1 heroe2 

-- 8) se produciria una recursion infinita 

-- 9) 

realiceLabor :: [TareaDescrita] -> Heroe -> Heroe
realiceLabor [] heroe = heroe
realiceLabor (t:ts) heroe = realiceLabor ts (hacerTarea t heroe)
