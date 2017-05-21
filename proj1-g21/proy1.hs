{- Modulo con datos y funciones relacionadas con Pokemones -}

import Data.List

-- Definición de Tipo de un Monstruo
data Tipo
  = Bug
  | Dark
  | Dragon
  | Electric
  | Fighting
  | Fire
  | Flying
  | Ghost
  | Grass
  | Ground
  | Ice
  | Normal
  | Poison
  | Psychic
  | Rock
  | Steel
  | Water
  deriving(Bounded, Eq, Enum, Read, Show)

-- Definicion de las Estadisticas de un Monstruo
data Estadisticas = Estadisticas { hp :: Int
                                 , atk :: Int
                                 , def :: Int
                                 , sAtk :: Int
                                 , sDef :: Int
                                 , spd :: Int
                                 } deriving(Show)

-- Definicion de la Especie de un Monstruo
data Especie = Especie { nCatalogo :: Int
                       , nombreEs :: String
                       , tipo1 :: Tipo
                       , tipo2 :: Tipo
                       , stats :: Estadisticas
                       , preEvo :: Int
                       , condicion :: String
                       } deriving(Show)

-- Definicion de un Ataque de un Monstruo. Sus atributos
data Ataque = Ataque { nombreAt :: String
                     , tipo :: Tipo
                     , fisico :: Bool
                     , puntos :: Int
                     , pp:: Int
                     } deriving(Show)

-- Definicion de un Monstruo. Sus atributos
data Monstruo = Monstruo { especie :: Especie
                         , alias :: String
                         , lvl :: Int
                         , statsM :: Estadisticas
                         , ataque1 :: Ataque
                         , pp1 :: Int
                         , ataque2 :: Ataque
                         , pp2 :: Int
                         , ataque3 :: Ataque
                         , pp3 :: Int
                         , ataque4 :: Ataque
                         , pp4 :: Int
                         , valorInd :: Int
                         , valorEsf :: Int
                         } deriving(Show)

-- Determina, para un tipo de ataque, cuales tipos son super efectivos,
-- cuales tipos son resistentes y cuales son inmunes.
relacionAtaqueTipo :: Tipo     -- Tipo de ataque a determinar la relación.
                   -> ( [Tipo]  -- Tipos super efectivos a el (2x dano). 
                      , [Tipo]  -- Tipos resistentes a el (0.5x dano).
                      , [Tipo]  -- Tipos inmunes a el (0x dano).
                      )
relacionAtaqueTipo x
  | Bug      <- x = ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost,
                    Steel, Fire], [])
  | Dark     <- x = ([Ghost, Psychic], [Fighting, Steel, Dark], [])
  | Dragon   <- x = ([Dragon], [Steel], [])
  | Electric <- x = ([Flying, Water], [Grass, Electric, Dragon], [Ground])
  | Fighting <- x = ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug,
                    Psychic], [Ghost])
  | Fire     <- x = ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
  | Flying   <- x = ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
  | Ghost    <- x = ([Ghost, Psychic], [Steel, Dark], [Normal])
  | Grass    <- x = ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire,
                    Grass, Dragon], [])
  | Ground   <- x = ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass],
                    [Flying])
  | Ice      <- x = ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
  | Normal   <- x = ([], [Rock, Steel], [Ghost])
  | Poison   <- x = ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
  | Psychic  <- x = ([Fighting, Poison], [Steel, Psychic], [Dark])
  | Rock     <- x = ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
  | Steel    <- x = ([Rock, Ice], [Steel, Fire, Water, Electric], [])
  | Water    <- x = ([Ground, Rock, Fire], [Water, Grass, Dragon], [])

-- Determina para un Monstruo, cual es su HP maximo
maxHp :: Int -> Int -> Int -> Int -> Int
maxHp ivHp baseHp evHp nivel = (div (n*nivel) 100) + 10
  where
    n = (ivHp+ 2*baseHp + div evHp 4) + 100

-- Determina para un Monstruo, el valor de sus Estadisticas
estadistica :: Int -> Int -> Int -> Int -> Int
estadistica iv base ev nivel = (div (n*nivel) 100) + 5
  where
    n = iv+ 2*base + div ev 4

-- Determina para un Monstruo, el dano que le ocasiono un ataque de otro
dano :: Int -> Int -> Int -> Int -> Float -> Integer
dano nivel poder ataque defensa modif = floor $ ((n*p*m/50)+2)*modif
  where
    n = ((2*fromIntegral nivel) / 5) + 2
    m = fromIntegral ataque/fromIntegral defensa
    p = fromIntegral poder


--cat :: FilePath -> String
--cat path = do
--    readFile path
    --procesar $ head (lines contents)
    --putStr "aja"

--procesar :: a -> Maybe b
--procesar a = do
--    x = Especie 

mensajeInicial :: String
mensajeInicial = "Ingrese el nombre de un archivo: \n" 


main = putStr "fdfdf"
