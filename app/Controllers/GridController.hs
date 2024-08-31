module Controllers.GridController where

    import Models.Cell
    import Models.Grid
    import Models.Rule
    import Data.Matrix hiding (matrix)
    import Data.List (intercalate)
    import Data.Maybe
    import Data.Char (toUpper)
    import System.Process (system)
    import System.Console.ANSI
    import System.IO
    import GHC.Conc
    

--     -- Funções que gerem a impressão da Matrix (Maybe Cell)
--     -- ------------------------------------------------------------------------------------------------

--     -- Função que imprime uma Matrix (Maybe Cell) com os números correspondentes a cada linha e coluna, a mesma recebe uma lista de Maybe Cell, que pode ser facilmente obtida através do
--     -- método gridToLists aplicado a uma Matrix (Maybe Cell). Sempre inicie o método com o n igual a 0, e procure não utilizar uma matriz
--     -- com mais de 2 casas decimais para não quebrar a formatação

--     -- printGridWithNumbers :: [[Maybe Cell]] -> Int ->  IO ()
--     -- printGridWithNumbers [] n =  return ()
--     -- printGridWithNumbers (x:xs) n = 
--     --     if n == 0 then do
--     --         putStrLn $ "  " ++ buildLineWithNumber (length x)
--     --         printGridWithNumbers (x:xs) (n + 1)

--     --     else do
--     --         putStrLn $ nStr ++ buildLine x
--     --         printGridWithNumbers xs (n + 1)

--     --     where
--     --         nStr = if n < 10 then " " ++ show n else show n
               
--     -- Função que imprime uma Matrix (Maybe Cell) sem os números correspondentes a cada linha e coluna.

--     printGrid :: Matrix (Maybe Cell) -> IO ()
--     printGrid grid = do
--         clearScreen
--         mapM_ printRow (gridToLists grid)
--         hFlush stdout

--     printRow :: [Maybe Cell] -> IO ()
--     printRow row = do
--         mapM_ printCell row
--         putStrLn ""

    printCell :: Maybe Cell -> IO ()
    printCell cell =
        case cell of 
            Nothing -> do
                setSGR [SetColor Foreground Vivid Black]
                putStr "██"
                setSGR [Reset]
            Just cell -> do
                setColor (color cell)
                putStr "██"
                setSGR [Reset]

    setColor :: String -> IO()
    setColor str =
        case str of 
            "VERMELHO" -> setSGR [SetColor Foreground Dull Red]
            "VERMELHO  BRILHANTE" -> setSGR [SetColor Foreground Vivid Red]
            "VERDE" -> setSGR [SetColor Foreground Dull Green]
            "VERDE BRILHANTE" -> setSGR [SetColor Foreground Vivid Green]
            "AMARELO" -> setSGR [SetColor Foreground Dull Yellow]
            "AMARELO BRILHANTE" -> setSGR [SetColor Foreground Vivid Yellow]
            "AZUL" -> setSGR [SetColor Foreground Dull Blue]
            "AZUL BRILHANTE" -> setSGR [SetColor Foreground Vivid Blue]
            "MAGENTA" -> setSGR [SetColor Foreground Dull Magenta]
            "MAGENTA BRILHANTE" -> setSGR [SetColor Foreground Vivid Magenta]
            "CIANO" -> setSGR [SetColor Foreground Dull Cyan]
            "CIANO BRILHANTE" -> setSGR [SetColor Foreground Vivid Cyan]
            "BRANCO" -> setSGR [SetColor Foreground Dull White]
            "BRANCO BRILHANTE" -> setSGR [SetColor Foreground Vivid White]
            _ -> return ()

    toColor :: String -> Maybe Color
    toColor color = case color of
        "PRETO" -> Just Black
        "VERMELHO" -> Just Red
        "VERDE" -> Just Green
        "AMARELO" -> Just Yellow
        "AZUL" -> Just Blue
        "MAGENTA" -> Just Magenta
        "CIANO" -> Just Cyan
        "BRANCO" -> Just White
        _ -> Nothing



--     -- Função que retorna a String formatada dos números correspondentes a cada coluna de uma Matrix (Maybe Cell)
--     buildLineWithNumber :: Int -> String
--     buildLineWithNumber n = intercalate " " list ++ " "
--         where list = [if c > 9 then show c else " " ++ show c | c <-[1..n]]
--     -- ------------------------------------------------------------------------------------------------

--     prepareSimulate :: Matrix (Maybe Cell) -> FilePath -> IO()
--     prepareSimulate matrix arq = do 
--         cellsJayzon <- readCells arq
--         case decode cellsJayzon :: Maybe [Cell] of 
--             Nothing -> putStrLn "vaitomacu"
--             Just cells -> simulate cells matrix 0


--     simulate :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
--     simulate cells matrix count = do
--         _ <- system "clear"
--         printGrid  matrix
--         putStrLn "Numero de passos dados ate agora: " ++ count
--         putStrLn "digite G para iniciar a geração"
--         putStrLn "digite N para simular só o próximo estágio"
--         putStrLn "digite I para inserir células"
--         option <- getLine
--         actionChooser cells matrix count option

--     actionChooser :: [Cell] -> Matrix (Maybe Cell) -> Int -> String -> IO()
--     actionChooser cells grid count opt
--         | option == 'G' = runLoop grid count
--         | option == 'N' = nextStep grid count
--         | option == 'I' = insertion cells grid count
--         | otherwise = simulate grid count

--         where
--             option = toUpper $ head opt

--     runLoop :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO ()
--     runLoop cells grid count = do
--         hSetBuffering stdin NoBuffering -- Desabilita o buffer do input
--         hSetEcho stdin False            -- Desabilita a ecoação do input no terminal
--         let checkInput = do
--                 inputAvaliable <- hReady stdin
--                 if inputAvaliable then do 
--                   return() 
--                 else do
--                   loopFunction grid
--                   runLoop cells (gridUpdate grid) (count + 1)
--         checkInput
    
--     loopFunction ::Matrix (Maybe Cell) -> IO()
--     loopFunction grid = do
--         printGrid  grid
--         threadDelay 500000

    
--     nextStep :: [Cell] -> Matrix (Maybe Cell) -> Int-> IO()
--     nextStep cells grid count = simulate cells (gridUpdate grid) (count + 1)

--     insertion :: [Cell] -> Matrix (Maybe Cell) -> IO()
--     insertion cells grid = do
--         _ <- system "clear"
--         putStrLn "Qual celula deseja inserir ?"
--         printCels cells
--         option <- getLine
--         selectCell option


--     printCels :: [Cell] -> Int -> IO()
--     printCels [] _ = return ()
--     printCels (x:xs) n = do
--         putStrLn $ "    "show n ++ " - " ++ show x
--         printCels xs (n + 1)

--     selectCell :: [Cell] -> Int -> Cell
--     selectCell cells n = cells[n]