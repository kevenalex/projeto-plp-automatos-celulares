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
    import Files.Scene
    import Text.Read (readMaybe)
    import Utils.Render
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

    emptyScene :: FilePath -> IO ()
    emptyScene path = do
        putStrLn "Vamos criar a sua matrix"
        putStrLn "Qual tamanho voce deseja ?"
        putStr "Digite o tamanho da altura:"
        hFlush stdout
        altura <- readLn :: IO Int
        putStr "Digite o tamanho da largura:"
        hFlush stdout
        largura <- readLn :: IO Int
        clearScreen
        prepareSimulate (gridGenerate altura largura) path

    prepareSimulate :: Matrix (Maybe Cell) -> FilePath -> IO()
    prepareSimulate matrix arq = do 
        cellsJayzon <- readCells arq
        case decode cellsJayzon :: Maybe [Cell] of 
            Nothing -> putStrLn "faltou o array de celulas"
            Just cells -> do 
                hSetBuffering stdout (BlockBuffering Nothing) -- Ligando o buffer
                hSetBuffering stdin NoBuffering
                simulate cells matrix 0
                hSetBuffering stdin LineBuffering
                hSetBuffering stdout NoBuffering

    simulate :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    simulate cells matrix count = do
        clearScreen
        printGrid  matrix
        printSimulationMenu count
        hFlush stdout
        option <- getLine
        actionChooser cells matrix count option

    actionChooser :: [Cell] -> Matrix (Maybe Cell) -> Int -> String -> IO()
    actionChooser cells grid count opt
        | option == '1' = runLoop cells grid count
        | option == '2' = nextStep cells grid count
        | option == '3' = insertion cells grid count
        | option == '4' = saveScene cells grid count
        | option == '5' = return ()
        | otherwise = simulate cells grid count

        where
            option = toUpper $ head opt

-----------------------------------------------------------------------------------------------------------

    runLoop :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO ()
    runLoop cells grid count = do
        hSetBuffering stdin NoBuffering -- Desabilita o buffer do input
        hSetEcho stdin False            -- Desabilita a ecoação do input no terminal
        let checkInput = do
                inputAvaliable <- hReady stdin
                if inputAvaliable then do 
                    hSetEcho stdin True
                    simulate cells grid count
                else do
                  loopFunction grid
                  runLoop cells (gridUpdate grid) (count + 1)
        checkInput
    
    loopFunction ::Matrix (Maybe Cell) -> IO()
    loopFunction grid = do
        printGrid  grid
        putStrLn "Aperte qualquer tecla para para a simulacao"
        threadDelay 500000

---------------------------------------------------------------------------------------------------------

    
    nextStep :: [Cell] -> Matrix (Maybe Cell) -> Int-> IO()
    nextStep cells grid count = simulate cells (gridUpdate grid) (count + 1)


-----------------------------------------------------------------------------------------------------------


    insertion :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    insertion cells grid count = do
        hSetBuffering stdin LineBuffering

        putStrLn "Qual celula voce deseja adicionar ?"
        hFlush stdout
        _ <- printCelsJson cells 1
        cell <- readLn :: IO Int
        
        putStrLn "Em pares de numeros separados por espacos e virgulas, digite onde deseja adicionar essa celula"
        putStrLn "Por exemplo: '1 3,3 1' adicionara celulas na posicao linha 1 coluna 3 e na posicao linha 3 coluna 1"
        hFlush stdout
        coordernates <- getLine        
        let coordinates = parsePairs coordernates
        let newGrid = insertCells grid (cells !! (cell-1)) coordinates

        simulate cells newGrid 0

    printCelsJson :: [Cell] -> Int -> IO()
    printCelsJson [] _ = return ()
    printCelsJson (x:xs) n = do
        putStrLn $ "    " ++ show n ++ " - " ++ show x
        printCelsJson xs (n + 1)

    parsePairs :: String -> [(Int, Int)]
    parsePairs "" = []
    parsePairs s = 
        let (pairStr, rest) = break (== ',') s  -- Quebra a string ao encontrar uma vírgula
            pair = parsePair pairStr
        in case pair of
            Just p  -> p : parsePairs (dropWhile (== ' ') (drop 1 rest))
            Nothing -> error $ "Entrada inválida: " ++ pairStr

    parsePair :: String -> Maybe (Int, Int)
    parsePair s = 
        let (x, rest) = break (== ' ') s  -- Quebra a string ao encontrar um espaço
            y = dropWhile (== ' ') rest   -- Remove espaços adicionais
        in do
            n1 <- readMaybe x
            n2 <- readMaybe y
            return (n1, n2)


---------------------------------------------------------------------------------------------------


    saveScene :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    saveScene cells grid count = do
        putStrLn "Digite um nome para essa cena:"
        hFlush stdout
        nome <- getLine 
        let scene = Scene nome (nrows grid) (ncols grid) (toList grid)
        addScene "./app/storage/scenes.json" scene
        simulate cells grid count


-----------------------------------------------------------------------------------------------------

    printSimulationMenu :: Int -> IO()
    printSimulationMenu count = do
        printEmptyLines 1
        putStrLn $ "Numero de passos dados ate agora: " ++ show count
        putStrLn $ "                                     1) Iniciar simulacao   2) Simular 1 passo   3) Inserir celulas   4) Salvar a cena   5) Voltar"