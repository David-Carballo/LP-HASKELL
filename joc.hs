--  David Carballo
--  Programa que permet jugar al quatre en ratlla contra l'ordinador

import System.Random
import Data.Char

-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt :: Int -> Int -> IO Int
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

------ // [ DATA ] // ------

data Board = 
    deriving (Show)

type Board = [[Char]]

type Movement = (Row, Col )

type Row = Char
type Col = Char

----------------------------

------ // [ PROGRAM ] // ------

-- main program that throws two dice.
main :: IO ()
main = do
    putStrLn "Benvinguts al joc -4 en Ratlla-\n"
    ----- SELECT PLAYER ------
    putStrLn "Primer de tot selecciona quin es el jugador que començará la partida:\n 1. Jugador\n 2. IA\n 3. Aleatori\n"
    first <- getLine
    putStrLn ("Comença:" ++ first ++ "\n")

    line <- getLine

    putStrLn $ (line ++ "\n")
    --------------------------
    r1 <- randInt 1 6
    r2 <- randInt 1 6
    print (r1, r2)

--  Estratègia RANDOM

--  Estratègia GREEDY

--  Estratègia SMART

{-

Benvinguts al joc 4 en ratlla

Selecciona qui comença la partida:
    1 - Jugador
    2 - IA
    3 - Aleatori
    putStrLn "Benvinguts al joc 4 en ratlla"
    putStrLn "Primer de tot selecciona quin es el jugador que començará la partida:\n 1. Jugador\n 2. IA\n 3. Aleatori\n"
    --Select player
Ara selecciona el taulell sobre el que jugareu,
introdueix el nombre de files n (columnes = n+1)



-}