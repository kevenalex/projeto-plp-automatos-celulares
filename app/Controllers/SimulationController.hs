module Controllers.SimulationController where

    import Models.Cell
    import Models.Grid
    import Models.Rule
    import Data.Matrix hiding (matrix)
    import Data.List (intercalate)
    import Data.Maybe
    import Data.Char (toUpper)
    import Data.Aeson
    import System.Process (system)
    import System.Console.ANSI
    import System.IO
    import GHC.Conc
    import Files.Cell
    import Models.Cell
    

    -- Funções que gerem a impressão da Matrix (Maybe Cell)
    -- ------------------------------------------------------------------------------------------------

    -- Função que imprime uma Matrix (Maybe Cell) com os números correspondentes a cada linha e coluna, a mesma recebe uma lista de Maybe Cell, que pode ser facilmente obtida através do
    -- método gridToLists aplicado a uma Matrix (Maybe Cell). Sempre inicie o método com o n igual a 0, e procure não utilizar uma matriz
    -- com mais de 2 casas decimais para não quebrar a formatação

    -- printGridWithNumbers :: [[Maybe Cell]] -> Int ->  IO ()
    -- printGridWithNumbers [] n =  return ()
    -- printGridWithNumbers (x:xs) n = 
    --     if n == 0 then do
    --         putStrLn $ "  " ++ buildLineWithNumber (length x)
    --         printGridWithNumbers (x:xs) (n + 1)

    --     else do
    --         putStrLn $ nStr ++ buildLine x
    --         printGridWithNumbers xs (n + 1)

    --     where
    --         nStr = if n < 10 then " " ++ show n else show n
               
    -- Função que imprime uma Matrix (Maybe Cell) sem os números correspondentes a cada linha e coluna.

    printGrid :: Matrix (Maybe Cell) -> IO ()
    printGrid grid = do
        clearScreen
        mapM_ printRow (gridToLists grid)
        hFlush stdout


    printRow :: [Maybe Cell] -> IO ()
    printRow row = do
        -- setCursorForPrintGrid 
        mapM_ printCell row
        putStrLn ""

    printCell :: Maybe Cell -> IO ()
    printCell cell =
        case cell of 
            Nothing -> do
                setSGR [SetColor Foreground Vivid Black]
                putStr "██"
                setSGR [Reset]
            Just cell -> do
                setSGR $ setColor (color cell)
                putStr "██"
                setSGR [Reset]



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



    -- Retorna a String formatada dos números correspondentes a cada coluna de uma Matrix (Maybe Cell)
    buildLineWithNumber :: Int -> String
    buildLineWithNumber n = intercalate " " list ++ " "
        where list = [if c > 9 then show c else " " ++ show c | c <-[1..n]]

    -- buildLine :: [Maybe Cell] -> IO()
    -- buildLine cellRow = [printCell cell | cell <- cellRow]
    ------------------------------------------------------------------------------------------------

    prepareSimulate :: Matrix (Maybe Cell) -> FilePath -> IO()
    prepareSimulate matrix arq = do 
        cellsJayzon <- readCells arq
        case decode cellsJayzon :: Maybe [Cell] of 
            Nothing -> putStrLn "vaitomacu"
            Just cells -> do 
                hSetBuffering stdout( BlockBuffering Nothing) -- Ligando o buffer
                simulate cells matrix 0
                hSetBuffering stdout NoBuffering


    simulate :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    simulate cells matrix count = do
        _ <- system "clear"
        printGrid  matrix
        putStrLn $ "Numero de passos dados ate agora: " ++ show count
        putStrLn "digite G para iniciar a geração"
        putStrLn "digite N para simular só o próximo estágio"
        putStrLn "digite I para inserir células"
        hFlush stdout
        option <- getLine
        actionChooser cells matrix count option

    actionChooser :: [Cell] -> Matrix (Maybe Cell) -> Int -> String -> IO()
    actionChooser cells grid count opt
        | option == 'G' = runLoop cells grid count
        | option == 'N' = nextStep cells grid count
        | option == 'I' = insertion cells grid count
        | otherwise = simulate cells grid count

        where
            option = toUpper $ head opt

    runLoop :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO ()
    runLoop cells grid count = do
        hSetBuffering stdin NoBuffering -- Desabilita o buffer do input
        hSetEcho stdin False            -- Desabilita a ecoação do input no terminal
        let checkInput = do
                inputAvaliable <- hReady stdin
                if inputAvaliable then do 
                    simulate cells grid count
                else do
                  loopFunction grid
                  runLoop cells (gridUpdate grid) (count + 1)
        checkInput
    
    loopFunction ::Matrix (Maybe Cell) -> IO()
    loopFunction grid = do
        printGrid  grid
        threadDelay 500000

    
    nextStep :: [Cell] -> Matrix (Maybe Cell) -> Int-> IO()
    nextStep cells grid count = simulate cells (gridUpdate grid) (count + 1)

    insertion :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    insertion cells grid count = do
        position <- getPositionInsertion
        _ <- system "clear"
        putStrLn "Qual celula deseja inserir ?"
        printCelsJson cells 1
        option <- readLn :: IO Int
        let celula = cells !! (option - 1)
        let newGrid = insertCell grid celula position
        simulate cells newGrid 0

    printCelsJson :: [Cell] -> Int -> IO()
    printCelsJson [] _ = return ()
    printCelsJson (x:xs) n = do
        putStrLn $ "    " ++ show n ++ " - " ++ show x
        printCelsJson xs (n + 1)

    getPositionInsertion :: IO (Int, Int)
    getPositionInsertion = do
        putStrLn "Digite a linha que deseja inserir a celula:"
        linha <- readLn :: IO Int
        putStrLn "Digite a coluna que deseja inserir a celula:"
        coluna <- readLn :: IO Int
        return (linha, coluna)