import Text.Show.Functions

alterarSalud modificador personaje = personaje {
    salud = max 0 . modificador . salud $ personaje
}

aumentar cantidad = (+cantidad)
disminuir cantidad = (subtract cantidad)

data Elemento = Elemento { 
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje) 
} deriving Show

data Personaje = Personaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
} deriving Show

-- Sobre elementos
tieneTipoDeElemento :: String -> Personaje -> Bool
tieneTipoDeElemento unTipo (Personaje _ _ elementos _) = elem unTipo . map tipo $ elementos

realizarAtaqueSobrePersonaje :: Elemento -> Personaje -> Personaje
realizarAtaqueSobrePersonaje elemento = (ataque elemento)

estaVivo :: Personaje -> Elemento -> Bool
estaVivo personaje elemento = (salud personaje) /= (danioQueProduce personaje elemento)

-- -------------------- Punto 1 --------------------

-- 1a
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {
    anioPresente = anio
}

-- 1b
meditar :: Personaje -> Personaje
meditar personaje = alterarSalud (aumentar ((salud personaje) / 2)) personaje

causarDanio :: Float -> Personaje -> Personaje
causarDanio cantidad = alterarSalud (disminuir cantidad)

-- -------------------- Punto 2 --------------------

-- 2a
esMalvado :: Personaje -> Bool
esMalvado = tieneTipoDeElemento "Maldad"

-- 2b
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = (salud personaje ) - (saludTrasAtaque elemento personaje)
    where saludTrasAtaque elemento = salud . (realizarAtaqueSobrePersonaje elemento)

-- 2c
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje = filter (puedeMatarloConUnElemento personaje)

puedeMatarloConUnElemento :: Personaje -> Personaje -> Bool
puedeMatarloConUnElemento personaje = any (not . estaVivo personaje ) . elementos



-- -------------------- Punto 3 --------------------
sinAccion = id

-- 3a
concentracion :: Int -> Elemento
concentracion nivel = Elemento "Magia" sinAccion (foldr1 (.) (replicate nivel meditar))   

-- 3b
esbirro :: Elemento
esbirro = Elemento "Maldad" (alterarSalud (disminuir 1)) sinAccion

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirro

-- 3c
jack :: Personaje
jack = Personaje "Jack" 300 [concentracion 3, katanaMagica] 200

katanaMagica :: Elemento
katanaMagica = Elemento "Magia" (alterarSalud (disminuir 1000)) sinAccion 

-- 3d
aku :: Int -> Float -> Personaje
aku anio cantidadSalud = Personaje {
    nombre = "Aku",
    salud = cantidadSalud,
    elementos = (concentracion 4) : (portalAlFuturo anio cantidadSalud) : (esbirrosMalvados (100*anio)),
    anioPresente = anio
}

portalAlFuturo :: Int -> Float -> Elemento
portalAlFuturo anio cantidadSalud= Elemento "Magia" (mandarAlAnio (anio+2800)) (generarAku (anio+2800))

generarAku :: Int -> Personaje -> Personaje
generarAku anio (Personaje _ salud _ _) = aku anio salud

-- -------------------- Punto 3 --------------------
-- luchar :: Personaje -> Personaje -> (Personaje, Personaje)
-- luchar atacante defensor 
--     | (not . estaVivo)


f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))