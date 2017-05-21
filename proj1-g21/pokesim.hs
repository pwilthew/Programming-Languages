
import Data.List
import Data.Function
import qualified Data.Text as T
import System.IO
import System.Environment

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
                       , tipo2 :: Maybe Tipo
                       , stats :: Estadisticas
                       , preEvo :: String
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
                         , ataques :: [Ataque]
                         , ppr :: [Int]
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

--Recibe una lista sin comas y regresa un ataque
aAtaque :: [String] -> Ataque
aAtaque a = Ataque nombreAt tipo fisico puntos ppr
  where
    nombreAt  = (a !! 0)
    tipo      = read (a !! 1) :: Tipo
    fisico    = read (a !! 2) :: Bool
    puntos    = read (a !! 3) :: Int
    ppr       = read (a !! 4) :: Int

--Recibe una lista sin comas y regresa una especie
aEspecie :: [[String]] -> [String] -> Especie
aEspecie detEspecies a = Especie nCatalogo nombreEs tipo1 tipo2 stats preEvo
                                 condicion
  where
    nCatalogo = read (a !! 0) :: Int
    nombreEs  = (a !! 1)
    tipo1     = read (a !! 2) :: Tipo
    tipo2     = if ((a !! 3) == "")
                then Nothing else Just (read (a !! 3) :: Tipo)
    stats     = Estadisticas (read (a !! 4) :: Int) (read (a !! 5) :: Int)
                             (read (a !! 6) :: Int) (read (a !! 7) :: Int)
                             (read (a !! 8) :: Int) (read (a !! 9) :: Int)
    preEvo    = if ((a!!10) == "")
                then "" else (detEspecies !! ((read (a !! 10) :: Int) -1)) !! 1
    condicion = (a !! 11)

--Funcion que toma un String y regresa una lista con los elementos sin las
--comas
quitarComas :: String -> [String]
quitarComas a = map T.unpack (T.splitOn (T.singleton ',') (T.pack a))

----abrirArchivo :: String -> [[String]]
--abrirArchivo a = do
--              datos <- readFile a
--              lista <- lines datos
--              map quitarComas lista


main :: IO ()
main = do
        args <- getArgs
--        let arch0 <- args !! 0
--            arch1 <- args !! 1
--            arch2 <- args !! 2
--            arch3 <- args !! 3
        archEsp <- readFile $ args !! 0
        --archAta <- readFile $ args !! 1
--        archEq1 <- readFile $ args !! 2
--        archEq2 <- readFile $ args !! 3
        let listaEsp = lines archEsp
--            listaAta <- lines archAta
--            listaEq1 <- lines archEq1
--            listaEq2 <- lines archEq2
        let datosEsp = map quitarComas listaEsp
--        datosAta <- map quitarComas listaAta
--        datosEq1 <- map quitarComas listaEq1
--        datosEq2 <- map quitarComas listaEq2


