module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =  (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

depto1 = Depto 1 45 5500 "Recoleta"

--Punto 1a)
mayor :: (Ord a, Ord b) => ( a -> b ) -> a -> a -> Bool
mayor funcion val1 val2 = funcion val1 > funcion val2

menor :: (Ord a, Ord b) => ( a -> b ) -> a -> a -> Bool
menor funcion val1 val2 = funcion val1 < funcion val2 

--Punto 1b)
--Para poder hacer uso de la funcion dada ordenarSegun con las funciones creadas es necesario realizar aplicacion parcial y orden superior
-- < ordenarSegun (mayor length) ["que", "estas", "como"]
-- < ["estas","como","que"]

--Punto 2a)

ubicadoEn ::  [Barrio] -> Requisito 
ubicadoEn listaBarrios depto =  any (flip estaEnAlgunBarrio depto) listaBarrios

estaEnAlgunBarrio :: Barrio -> Depto -> Bool
estaEnAlgunBarrio barrio1 =  (== barrio1) . barrio 

--OTRA FORMA:
--ubicadoEn ::  [Barrio] -> Requisito 
--ubicadoEn listaBarrios depto = elem (barrio depto) listaBarrios

--Punto 2b) 
cumpleRango :: (Depto -> Number) -> Number -> Number -> Requisito
cumpleRango funcion valor1 valor2 depto = between (menorEntre valor1 valor2) (mayorEntre valor1 valor2) (funcion depto)

mayorEntre :: Number -> Number -> Number
mayorEntre valor1 valor2 | valor1 > valor2 = valor1
                         | otherwise = valor2

menorEntre :: Number -> Number -> Number
menorEntre valor1 valor2 | valor1 < valor2 = valor1
                         | otherwise = valor2

--Punto 3a)
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto requisitos = all (cumpleRequisito depto) requisitos 

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto 

--Punto 3b)

type Criterio =  Depto -> Depto -> Bool 

buscar :: Busqueda -> Criterio -> [Depto] -> [Depto]   
buscar busqueda criterio listaDeptos = ordenarSegun criterio (filtrarDeptosCumplenBusqueda busqueda listaDeptos)

filtrarDeptosCumplenBusqueda :: Busqueda -> [Depto] -> [Depto]
filtrarDeptosCumplenBusqueda busqueda = filter (flip cumpleBusqueda busqueda) 

--Punto 3C)
--Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, 
--que cumplan con:
--Encontrarse en Recoleta o Palermo 
--Ser de 1 o 2 ambientes 
--Alquilarse a menos de $6000 por mes

cantAmbientes :: Number -> Number -> Requisito
cantAmbientes num1 num2 = (between (menorEntre num1 num2) (mayorEntre num1 num2)) . ambientes

-- < buscar [ubicadoEn ["Recoleta","Palermo"], cantAmbientes 1 2, cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo

--Punto 4
mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto =  mailsPersonas . (filtrarPersonas depto) 

filtrarPersonas :: Depto -> [Persona] -> [Persona]
filtrarPersonas depto  = filter (algunaBusquedaCumple depto) 

algunaBusquedaCumple :: Depto -> Persona -> Bool
algunaBusquedaCumple depto persona = any (cumpleBusqueda depto) (busquedas persona)

mailsPersonas :: [Persona] -> [Mail]
mailsPersonas = map mail 

