--  David Carballo
--  Programa que permet jugar al quatre en ratlla contra l'ordinador

import System.Random
import Data.Char


randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

-------- [ DATA ] --------

--type Player = Maybe [Char]

data Value = Red | Yellow | O
  deriving (Eq)

instance Show Value where
    show Red = "R"
    show Yellow = "Y"
    show O = "O"


data Board = Board [[Value]]
    deriving (Eq)

instance Show Board where
    show (Board a) = (unlines ( map (unwords . map show) (trans a) ))
    --show (Board a) = unlines $ reverse $ map (unwords . map show) a


create:: Int -> Int -> IO Board
create n m = do
    let b = Board (take m $ repeat (take n $ repeat (O)))
    return b

numCols:: Board -> Int
numCols (Board a) = length a

numRows:: Board -> Int 
numRows (Board [a]) = length a

--createBoard:: Board
--createBoard = Board [[O]]

--setNth2:: [Value] -> Int -> Value -> [Value]
--setNth2 list n val
--  | (list !! (n-1)) == O = (take (n-1) list) ++ [val] ++ (drop n list)
--  | otherwise = list

--setNth:: Board -> Int -> Int -> Value -> Board
--setNth (Board list) x y val = 
--  Board ((take (x-1) list) ++ [setCol] ++ (drop (x) list))
--  where column = (list !! (x-1))
--        setCol = setNth2 column y val

--fa la transposada del tauler
trans:: [[Value]] -> [[Value]]
trans ([]:_) = []
trans x = (map head x) : trans (map tail x)

--fa la transposada del tauler
transBoard:: Board -> Board
transBoard (Board x) = Board (trans x)

--afegeix una fitxa a una llista
insert2:: [Value] -> Value -> [Value]
insert2 list x  = do
  let num = ((length $ takeWhile (==O) list)-1)
  news <- (take num $ list) ++ [x] ++ (dropWhile (==O) list)
  return news

--afegeix una fitxa X a una columna del tauler
insert:: Board -> Int -> Value -> Board
insert (Board a) col x = Board ((take (col-1) a) ++ [target] ++ (drop col a))
  where target = insert2 (a !! (col-1)) x

printBoard:: Board -> IO ()
printBoard = print

countVal2:: [Value] -> Value -> Int
countVal2 x val= length $ filter (==val) x

countVal:: Board -> Value -> Int
countVal (Board a) val= foldl (+) 0 (map (flip countVal2 val) a)

--devuelve el indice del maximo de una lista
getMaxInd::[Int] -> Int -> Int
getMaxInd list pos
  | (head list == maximum list) = pos+1
  | otherwise = getMaxInd (tail list) (pos+1)

--busca la columna que mas fichas en ralla tenga 
searchCol:: Board -> Value -> Int
searchCol (Board a) p = getMaxInd (map (flip countVal2 p) list) 0
  where list = (map (takeWhile (==p)) (map (dropWhile (==O)) a))

turn:: Board -> Value
turn (Board a)
  | red > yellow = Yellow
  | otherwise = Red
  where
    yellow = countVal (Board a) Yellow
    red = countVal (Board a) Red

--agrupa els elements consecutius d'una llista
group:: Eq a=>[a] -> [[a]]
group [] = []
group (x:xs) = (x:ys) : group zs
                  where (ys,zs) = (takeWhile (==x) xs, dropWhile (==x) xs)

--retorna si una llista te N valors(val) consecutius  
takeConsecutives:: Eq a=> [a] -> a -> Int-> Bool
takeConsecutives x val n= any (==n) (map (length) list)
    where list = filter (elem val) (group x)

--retorna el nom del jugador que ha començat la partida
firstPlay:: String -> String
firstPlay f
    | (f == "1") = " Jugador"
    | (f == "2") = " Ordinador"

--canvi de color
changeColor:: Value -> Value
changeColor c
    | c == Red = Yellow
    | otherwise = Red

--canvi de jugador
changePlayer:: String -> String
changePlayer p
    | p == "Jugador" = "Ordinador"
    | otherwise = "Jugador"


--comproba si a alguna fila hi han 4 fitxes consecutives del player val
checkV:: Board -> Value -> Bool
checkV (Board a) val = any (==True) ([takeConsecutives x val 4 | x <- a])

--comproba si a alguna columna hi han 4 fitxes consecutives del player val
checkH:: Board -> Value -> Bool
checkH (Board a) val = any (==True) ([takeConsecutives x val 4| x <- trans a])

--obte les diagonals del tauler
getDiag [] = []
getDiag (x:xs) = takeWhile (not . null) $ zipWith (++) (map (:[]) x ++ repeat []) ([]:getDiag xs)

--comproba si a alguna diagonal hi han 4 fitxes consecutives del player val
checkD:: Board -> Value -> Bool
checkD (Board a) val = (any (==True) ([takeConsecutives x val 4| x <- getDiag a])) || (any (==True) ([takeConsecutives x val 4| x <- getDiag (trans a)]))

--comproba si el player ha guanyat
checkWin:: Board -> Value -> IO Bool
checkWin (Board a) val = return ((checkH (Board a) val) || checkV (Board a) val || checkD (Board a) val) 

--comproba si el tauler esta ple
checkFill:: Board -> Bool
checkFill (Board a) = not (any (==True) (map (any (==O)) a))

--funcio per fer estrategia greedy del ordinador
playGreedy:: Board -> String -> Value -> IO ()
playGreedy a player color = do
    putStrLn ("Mou: [" ++ (player) ++ "]")
    if(player == "Jugador") then do
        putStr "> En quina columna vols introduir la ficha? "
        col <- getLine
        let board = (insert a (read col) color)
        winner <- checkWin board color
        printBoard board
        if winner then do
            putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
        else do
            if (checkFill board) then do
                putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
            else
                playGreedy board (changePlayer player) (changeColor color)
    else do
        if (checkV a (changeColor color)) then do
            let rival1 = searchCol a (changeColor color)
            let board = (insert a rival1 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playGreedy board (changePlayer player) (changeColor color) 
        else if (checkH (transBoard a) (changeColor color)) then do
            let rival2 = searchCol (transBoard a) (changeColor color)
            let board = (insert a rival2 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playGreedy board (changePlayer player) (changeColor color) 
        else if (checkV a color) then do
            let rival1 = searchCol a color
            let board = (insert a rival1 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playGreedy board (changePlayer player) (changeColor color)
        else if (checkH (transBoard a) color) then do
            let rival2 = searchCol (transBoard a) color
            let board = (insert a rival2 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playGreedy board (changePlayer player) (changeColor color)
        else do
            r1 <- randInt 1 (numCols a)
            let board = (insert a r1 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playGreedy board (changePlayer player) (changeColor color)
        

--funcio per fer estrategia random del ordinador
playRandom::Board -> String -> Value -> IO ()
playRandom a player color = do
    putStrLn ("Mou: [" ++ (player) ++ "]")
    if(player == "Jugador") then do
        putStr "> En quina columna vols introduir la ficha? "
        col <- getLine
        let board = (insert a (read col) color)
        winner <- checkWin board color
        printBoard board
        if winner then do
            putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
        else do
            if (checkFill board) then do
                putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
            else
                playRandom board (changePlayer player) (changeColor color) 
    else do
        r1 <- randInt 1 (numCols a)
        let board = (insert a r1 color)
        winner <- checkWin board color
        printBoard board
        if winner then do
            putStrLn "--GAME OVER--\n L'ordinador guanya la partida"
        else do
            if (checkFill board) then do
                putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
            else
                playRandom board (changePlayer player) (changeColor color)   

--funcio per fer estrategia smart del ordinador
playSmart:: Board -> String -> Value -> IO ()
playSmart a player color = do
    putStrLn ("Mou: [" ++ (player) ++ "]")
    if(player == "Jugador") then do
        putStr "> En quina columna vols introduir la ficha? "
        col <- getLine
        let board = (insert a (read col) color)
        winner <- checkWin board color
        printBoard board
        if winner then do
            putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
        else do
            if (checkFill board) then do
                putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
            else
                playSmart board (changePlayer player) (changeColor color)
    else do
        if (checkV a (changeColor color)) then do
            let rival1 = searchCol a (changeColor color)
            let board = (insert a rival1 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playSmart board (changePlayer player) (changeColor color) 
        else if (checkH (transBoard a) (changeColor color)) then do
            let rival2 = searchCol (transBoard a) (changeColor color)
            let board = (insert a rival2 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playSmart board (changePlayer player) (changeColor color) 
        else if (checkV a color) then do
            let rival1 = searchCol a color
            let board = (insert a rival1 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playSmart board (changePlayer player) (changeColor color)
        else if (checkH (transBoard a) color) then do
            let rival2 = searchCol (transBoard a) color
            let board = (insert a rival2 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playSmart board (changePlayer player) (changeColor color)
        else do
            r1 <- randInt 1 (numCols a)
            let board = (insert a r1 color)
            winner <- checkWin board color
            printBoard board
            if winner then do
                putStrLn "--Enhorabona!!!--\n Has guanyat la partida"
            else do
                if (checkFill board) then do
                    putStrLn "--Empat!!!--\n La partida ha finalitzat en taules" 
                else
                    playSmart board (changePlayer player) (changeColor color)


------ // [ PROGRAM ] // ------

-- main program that throws two dice.
main :: IO ()
main = do
    putStrLn "\n\t|   Benvinguts al joc\t| \n\t|    -4 en Ratlla-\t|\n"
        ----- SELECT DIFF --------
    putStrLn "> Selecciona la dificultat del joc: \n 1. Facil\t 2. Normal\t3. Avançat"
    diff <- getLine
        ----- SELECT BOARD -------
    putStr "> Indica la mesura del tauler:\nFILES: "
    rows <- getLine
    putStr "COLUMNES: "
    cols <- getLine
    putStrLn ""
    board <- create (read rows) (read cols)
    printBoard board
        ----- SELECT PLAYER ------
    putStrLn "> Primer de tot selecciona quin es el jugador que començará la partida:\n\n\t1. Jugador\t 2. Ordinador\n"
    first <- getLine
    let player = firstPlay first
    putStrLn ("El primer en moure es:" ++ player ++ "\n")
        ----- SELECT COLOR -------
    putStrLn "> Jugador, quin color de fitxa vols?\n\n\tR. Vermell\tY. Groc\n"
    color <- getLine
    let c | (color == "R") || (color == "r") = Red
          | otherwise = Yellow
        ---- START GAME -----
    if (first == "1") then do
        if(diff == "1") then do
            playRandom board "Jugador" c
        else if(diff == "2") then do
            playGreedy board "Jugador" c
        else 
            playRandom board "Jugador" c
    else
        if(diff == "1") then do
            playRandom board "Ordinador" (changeColor c)
        else if(diff == "2") then do
            playGreedy board "Ordinador" (changeColor c)
        else 
            playRandom board "Ordinador" (changeColor c)