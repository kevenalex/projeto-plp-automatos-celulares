module Controllers.GridController where

    import Models.Cell
    import Models.Grid
    import Models.Rule
    import Data.Matrix
    import qualified Data.Vector as V
    import Data.List (intercalate)
    import Data.Maybe
    import Data.Char (intToDigit)
    

    -- Funções que gerem a impressão da Matrix (Maybe Cell)
    -- ------------------------------------------------------------------------------------------------

    -- Função que imprime uma Matrix (Maybe Cell) com os números correspondentes a cada linha e coluna, a mesma recebe uma lista de Maybe Cell, que pode ser facilmente obtida através do
    -- método gridToLists aplicado a uma Matrix (Maybe Cell). Sempre inicie o método com o n igual a 0, e procure não utilizar uma matriz
    -- com mais de 2 casas decimais para não quebrar a formatação

    printGridWithNumbers :: [[Maybe Cell]] -> Int ->  IO ()
    printGridWithNumbers [] n =  return ()
    printGridWithNumbers (x:xs) n = 
        if n == 0 then do
            putStrLn $ "  " ++ buildLineWithNumber (length x)
            printGridWithNumbers (x:xs) (n + 1)

        else do
            putStrLn $ nStr ++ buildLine x
            printGridWithNumbers xs (n + 1)

        where
            nStr = if n < 10 then " " ++ show n else show n
               
    -- Função que imprime uma Matrix (Maybe Cell) sem os números correspondentes a cada linha e coluna, a mesma recebe uma lista de Maybe Cell, que pode ser facilmente obtida através do
    -- método gridToLists aplicado a uma Matrix (Maybe Cell). Sempre inicie o método com o n igual a 0, e procure não utilizar uma matriz
    -- com mais de 2 casas decimais para não quebrar a formatação

    printGrid :: [[Maybe Cell]] -> Int -> IO ()
    printGrid [] _ = return ()
    printGrid (x:xs) n = do 
        if n == 0 then do
            putStrLn ""
            printGrid (x:xs) (n + 1)
        else do
            putStrLn $ "  " ++ buildLine x
            printGrid xs n

    -- Função que retorna a String formatada de cada linha de uma Matrix (Maybe Cell)
    buildLine :: [Maybe Cell] -> String
    buildLine cellRow = intercalate "" cells
        where 
            cells = [if isAlive cell then color (fromJust cell) else "⬛ " | cell <- cellRow]

    -- Função que retorna a String formatada dos números correspondentes a cada coluna de uma Matrix (Maybe Cell)
    buildLineWithNumber :: Int -> String
    buildLineWithNumber n = intercalate " " list ++ " "
        where list = [if c > 9 then show c else " " ++ show c | c <-[1..n]]
    -- ------------------------------------------------------------------------------------------------

    simulate :: Matrix (Maybe Cell) -> IO()
    simulate matrix = do
        -- _ <- system "clear"
        printGrid (gridToLists matrix) 0
        putStrLn "digite G para iniciar a geração"
        putStrLn "digite N para simular só o próximo estágio"
        putStrLn "digite I para inserir células"
        option <- getLine
        actionChooser matrix option

    actionChooser :: Matrix (Maybe Cell) -> String -> IO()
    actionChooser grid option 
        | option == "G" = startGeneration grid
        | option == "N" = nextStep grid
        | option == "I" = insertion grid
        | otherwise = simulate grid

    startGeneration :: Matrix (Maybe Cell) -> IO()
    startGeneration grid = putStrLn "foda-se"

    nextStep :: Matrix (Maybe Cell) -> IO()
    nextStep grid = putStrLn "foda-se"

    insertion :: Matrix (Maybe Cell) -> IO()
    insertion grid = putStrLn "gay"