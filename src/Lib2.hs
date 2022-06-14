import Text.Show.Functions


-- -------------------- Funciones auxiliares --------------------

alterarSalud :: (Float -> Float) -> Personaje -> Personaje 
alterarSalud modificador personaje = personaje {
    salud = modificador . salud $ personaje
}
aumentar :: Float -> Float -> Float
aumentar cantidad = (+cantidad)

disminuir:: Float -> Float -> Float
disminuir cantidad = (max 0) . (subtract cantidad)

realizarAtaque :: Elemento -> Personaje -> Personaje
realizarAtaque elemento = ataque elemento

data Elemento = Elemento {
    tipo :: String,
    ataque :: (Personaje -> Personaje), -- actúa sobre un rival
    defensa :: (Personaje -> Personaje) --actúa sobre el personaje que tiene el elemento
} deriving Show

data Personaje = Personaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
} deriving Show




-- -------------------- Punto 1 --------------------
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {
    anioPresente = anio
}

meditar ::  Personaje -> Personaje
meditar personaje = alterarSalud  (aumentar (salud personaje * 0.5)) personaje

causarDanio :: Float -> Personaje -> Personaje
causarDanio cantidad personaje = alterarSalud (disminuir cantidad) personaje

-- -------------------- Punto 2 --------------------

--2a
esMalvado :: Personaje -> Bool
esMalvado = any esTipoMaldad . elementos

esTipoMaldad :: Elemento -> Bool
esTipoMaldad = (=="Maldad") . tipo 

--2b
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = (salud personaje) - (saludTrasAtaque elemento personaje)

saludTrasAtaque :: Elemento -> Personaje -> Float
saludTrasAtaque elemento = salud  . realizarAtaque elemento

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (saludEsCero personaje) enemigos

saludEsCero :: Personaje -> Personaje -> Bool
saludEsCero personaje enemigo = any ((==0) . (flip saludTrasAtaque personaje)) (elementos enemigo)

-- -------------------- Punto 3 --------------------
--Elemento tipo ataque defensa

sinAccion = id

concentracion :: Int -> Elemento
concentracion nivel = Elemento {
    tipo = "Magia",
    ataque = sinAccion,
    defensa = foldr1 (.) (replicate nivel meditar)
}

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad Elemento {
    tipo = "Maldad",
    ataque = alterarSalud (disminuir 1),
    defensa = sinAccion
}

jack :: Personaje
jack = Personaje{
    nombre = "Jack",
    salud = 300,
    elementos = [concentracion 3, katanaMagica],
    anioPresente = 200
}
katanaMagica :: Elemento
katanaMagica = Elemento {
    tipo = "Magia",
    ataque = alterarSalud (disminuir 1000),
    defensa = sinAccion
}

aku :: Int -> Float -> Personaje
aku anio' cantidadSalud = Personaje {
    nombre = "Aku",
    salud = cantidadSalud,
    elementos = concentracion 4 : portalAlFuturo anio' cantidadSalud : esbirrosMalvados (100*anio'),
    anioPresente = anio'
}

portalAlFuturo anioAku saludAku = Elemento {
    tipo = "Magia",
    ataque = mandarAlAnio (2800 + anioAku),
    defensa = (aku (2800+anioAku)) . salud
}
-- -------------------- Punto 4 --------------------

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor 
    | saludEsCero atacante = (defensor, atacante)
    | otherwise = luchar proximoAtacante proximoDefensor
    where proximoAtacante = usarElementos ataque defensor  (elementos atacante)
          proximoDefensor = usarElementos defensa atacante (elementos atacante)

usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afecatar personaje funcion = funcion personaje

-- ttPersonaje = Personaje "Gonzalo" 100 [] 2022
-- ttE1 = Personaje "1" 100 [ttElemento, ttElemento2] 2022
-- ttE2 = Personaje "2" 100 [ttElemento] 2022

-- ttElemento = Elemento "" (causarDanio 10) ""
-- ttElemento2 = Elemento "" (causarDanio 90) ""