module Library where
import PdePreludat


anio :: Number -> Bool
anio numero = (mod numero 4 == 0) && (mod numero 100 /= 0 || mod numero 400 == 0) 