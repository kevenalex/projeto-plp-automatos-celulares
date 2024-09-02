{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Controllers.SimulationController where

    import Models.Cell
    import Models.Grid
    import Data.Matrix hiding (matrix)
    import Data.List (intercalate)
    import Data.Char (toUpper)
    import Data.Aeson
    import System.Console.ANSI
    import System.IO
    import GHC.Conc
    import Files.Cell
    import Files.Scene
    import Text.Read (readMaybe, Lexeme (String))
    import Utils.Render


    -- Funções que gerem a impressão da Matrix (Maybe Cell)
    -- ------------------------------------------------------------------------------------------------

    -- Função que imprime uma Matrix (Maybe Cell) com os números correspondentes a cada linha e coluna, a mesma recebe uma lista de Maybe Cell, que pode ser facilmente obtida através do
    -- método gridToLists aplicado a uma Matrix (Maybe Cell). Sempre inicie o método com o n igual a 0, e procure não utilizar uma matriz
    -- com mais de 2 casas decimais para não quebrar a formatação

    printGridWithNumbers :: Matrix (Maybe Cell) ->  IO ()
    printGridWithNumbers matrix = do
        clearScreen
        setCursorColumn $ 103 - ncols matrix
        mapM_ (\i -> if even i then do
            
                    setSGR [SetPaletteColor Foreground 255]
                    putStr $ formatLowNumber i
                    setSGR [Reset]

                else do 

                    setSGR [SetPaletteColor Foreground 247]
                    putStr $ formatLowNumber i
                    setSGR [Reset]


            ) [1..(ncols matrix)]

        putStrLn ""

        mapM_ (\i -> if even i then do 

                    setCursorColumn $ 100 - ncols matrix
                    setSGR [SetPaletteColor Foreground 255]
                    putStr $ formatLowNumber i ++ " "
                    printRowNoSpace $ gridToLists matrix !! (i - 1)
                    setSGR [Reset]
                    
                else do 

                    setCursorColumn $ 100 - ncols matrix
                    setSGR [SetPaletteColor Foreground 247]
                    putStr $ formatLowNumber i ++ " "
                    printRowNoSpace $ gridToLists matrix !! (i - 1)
                    setSGR [Reset]

            ) [1..(nrows matrix)]

    formatLowNumber :: Int -> String
    formatLowNumber n = if n < 10 then " " ++ show n
                        else show n



    printGrid :: Matrix (Maybe Cell) -> IO ()
    printGrid grid = do
        clearScreen
        mapM_ printRow (gridToLists grid)
        hFlush stdout

    printRow :: [Maybe Cell] -> IO ()
    printRow row = do
        setCursorColumn $ 100 - length row
        mapM_ printCell row
        putStrLn ""

    printRowNoSpace :: [Maybe Cell] -> IO ()
    printRowNoSpace row = do
        mapM_ printCell row
        putStrLn ""

    printCell :: Maybe Cell -> IO ()
    printCell cell = do
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

    -- buildLine :: [Maybe Cell] -> IO()
    -- buildLine cellRow = [printCell cell | cell <- cellRow]
    ------------------------------------------------------------------------------------------------

    emptyScene :: FilePath -> IO ()
    emptyScene path = do
        printMidScreen "VAMOS CRIAR A SUA MATRIX"
        printMidScreen "QUAIS AS DIMENSOES DA SUA MATRIX? (LINHAS,COLUNAS)"
        hFlush stdout
        setCursorInput
        input <- getLine :: IO String

        let inputList = words input

        if length inputList /= 2 then do
            printMidScreen "TENTE INSERIR (LINHAS,COLUNAS)"
            threadDelay 930000
            return ()
        else do

            let dimensoes = (,) <$> readMaybe (inputList !! 0) <*> readMaybe (inputList !! 1)

            case dimensoes of
                Just (rows, cols) -> do
                    clearScreen
                    prepareSimulate (gridGenerate rows cols) path
                    
                Nothing -> do 
                    printMidScreen "TENTE INSERIR (LINHAS,COLUNAS)"
                    threadDelay 930000
                    return ()
       

    prepareSimulate :: Matrix (Maybe Cell) -> FilePath -> IO()
    prepareSimulate matrix arq = do
        cellsJayzon <- readCells arq
        case decode cellsJayzon :: Maybe [Cell] of
            Nothing -> do 
                printMidScreen "CRIE CELULAS PRIMEIRO!"
                threadDelay 930000
            Just cells -> do
                hSetBuffering stdout (BlockBuffering Nothing) -- Ligando o buffer
                hSetBuffering stdin NoBuffering
                simulate cells matrix 0
                hSetBuffering stdin LineBuffering
                hSetBuffering stdout NoBuffering
                printMidScreen "REINICIAR CENA? (S)IM ou (N)ÃO"
                option <- getLine
                if option == "S" || option == "s" 
                    then 
                        prepareSimulate matrix arq
                    else return ()
        
    simulate :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    simulate cells matrix count = do
        clearScreen
        printGrid  matrix
        printEmptyLines 2
        printSimulationMenu count
        hFlush stdout
        setCursorInput
        option <- getLine
        actionChooser cells matrix count option

    actionChooser :: [Cell] -> Matrix (Maybe Cell) -> Int -> String -> IO()
    actionChooser cells grid count opt =
        if null opt
            then simulate cells grid count

        else case head opt of
            '1' -> runLoop cells grid count
            '2' -> nextStep cells grid count
            '3' -> insertion cells grid count
            '4' -> saveScene cells grid count
            '5' -> return ()
            _ -> simulate cells grid count

-----------------------------------------------------------------------------------------------------------

    runLoop :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO ()
    runLoop cells grid count = do
        hSetBuffering stdin NoBuffering -- Desabilita o buffer do input
        hSetEcho stdin False            -- Desabilita a ecoação do input no terminal
        let checkInput = do
                setCursorInput
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
        printEmptyLines 2
        printMidScreen "Aperte qualquer tecla para para a simulacao"
        hFlush stdout
        threadDelay 500000

---------------------------------------------------------------------------------------------------------


    nextStep :: [Cell] -> Matrix (Maybe Cell) -> Int-> IO()
    nextStep cells grid count = simulate cells (gridUpdate grid) (count + 1)


-----------------------------------------------------------------------------------------------------------


    insertion :: [Cell] -> Matrix (Maybe Cell) -> Int -> IO()
    insertion cells grid count = do
        hSetBuffering stdin LineBuffering

        clearScreen
        printGridWithNumbers grid
        printEmptyLines 2

        printMidScreen "Qual celula voce deseja adicionar ?"
        _ <- printCelsJson cells 1
        hFlush stdout
        setCursorInput
        cell <- readLn :: IO Int

        printMidScreen "Em pares de numeros separados por espacos e virgulas, digite onde deseja adicionar essa celula"
        printMidScreen "Por exemplo: '1 3,3 1' adicionara celulas na posicao linha 1 coluna 3 e na posicao linha 3 coluna 1"
        hFlush stdout
        setCursorInput
        coordernates <- getLine
        let coordinates = parsePairs coordernates
        let newGrid = insertCells grid (cells !! (cell-1)) coordinates

        simulate cells newGrid 0



    printCelsJson :: [Cell] -> Int -> IO()
    printCelsJson [] _ = return ()
    printCelsJson (x:xs) n = do
        setCursorColumn 85
        putStrLn $ "    " ++ show n ++ " - " ++ show x
        printCelsJson xs (n + 1)


-----------------------------------------------------------------------------------------------------
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
        printMidScreen "Digite um nome para essa cena:"
        hFlush stdout
        setCursorInput
        nome <- getLine
        let scene = Scene nome (nrows grid) (ncols grid) (toList grid)
        addScene "./app/storage/scenes.json" scene
        simulate cells grid count


-----------------------------------------------------------------------------------------------------

    printSimulationMenu :: Int -> IO()
    printSimulationMenu count = do
        printEmptyLines 1
        setCursorColumn 100
        printMidScreen $ "Numero de passos dados ate agora: " ++ show count
        setCursorColumn 75
        printMidScreen "1) Iniciar simulacao   2) Simular 1 passo   3) Inserir celulas   4) Salvar a cena   5) Voltar"
