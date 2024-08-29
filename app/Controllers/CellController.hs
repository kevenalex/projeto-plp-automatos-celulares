module Controllers.CellController where
    import Files.Cell
    import Data.Aeson
    import Models.Cell
    import Models.Rule (Rule (Rule))
    import Data.Char (toUpper)
    import System.Console.ANSI
    import Data.Maybe
    import System.Process(system)
    import Utils.Logo
    import Data.Vector.Generic.Mutable (clear)
    import Control.Concurrent(threadDelay)

    
    menuCells :: FilePath -> IO()
    menuCells path = do

        cellsJSON <- readCells path
        
        case decode cellsJSON :: Maybe [Cell] of 

            Nothing -> error "ARQUIVO app/storage/cells.json MODIFICADO OU CORROMPIDO\nAPAGUE O CONTEUDO DO MESMO E INSIRA [] NO SEU CONTEUDO\nTENTE EXECUTAR O PROGRAMA NOVAMENTE"

            Just cells -> do 
                
                if null cells then printTextFileWithClear "app/storage/ruleController/screenNoCells.txt"
                else do 
                    printTextFileWithClear "app/storage/ruleController/listOfCells.txt"

                    printEmptyLines 2

                    printCells cells 1

                    printEmptyLines 1

                    printTextFileNoClear "app/storage/ruleController/ruleMenuOptions.txt"

        option <- getLine

        case option of
            "1" -> do addAutomata path; menuCells path;
            "2" -> do removeAutomata path; menuCells path
            "3" -> return ()
            _ -> menuCells path

    addAutomata ::FilePath ->  IO()
    addAutomata path = do

        printTextFileWithClear "app/storage/ruleController/nameCellQuestion.txt"

        nameCellNT <- getLine
        let nameCellT = map toUpper nameCellNT

        printTextFileWithClear "app/storage/ruleController/birthRule.txt"

        nascStr <- getLine
        let nascList = map (\x -> read [x] :: Int) nascStr
        
        printTextFileWithClear "app/storage/ruleController/stayRule.txt"
        
        stayStr <- getLine
        let stayList = map (\x -> read [x] :: Int) stayStr -- Todo verificar que esses números tão entre 1 e 8, e se são numerinhos

        let regra = Rule nascList stayList

        printTextFileWithClear "app/storage/ruleController/colorMenu.txt"

        colorI <- getLine

        if isNothing $ selectColor colorI then do printTextFileWithClear "app/storage/ruleController/ruleMenuColorError.txt"; threadDelay 200000;
        else do 
            let cell = Cell nameCellT regra $ fromJust $ selectColor colorI
            addCell path cell

    -- printBuildCellQuestion :: String ->  IO ()
    -- printBuildCellQuestion qst = do
    --     clearScreen
    --     printEmptyLines 21
    --     putStrLn "                                                                   CRIANDO UMA CELULA"
    --     printEmptyLines 4
    --     putStrLn $ "                                                                   " ++ qst
    --     printEmptyLines 21

    -- printBuildCellWithName :: String -> IO ()
    -- printBuildCellWithName name = do
    --     printTopBuildCell
    --     putStrLn ""
    --     putStrLn $ "NOME: " ++ name


    -- printBuildCellWithBirthRule :: String -> [Int] -> IO ()
    -- printBuildCellWithBirthRule name bRule = do
    --     printBuildCellWithName name
    --     putStrLn ""
    --     putStrLn $ "NASCIMENTO: " ++ bRule

    -- printBuildCellWithStayRule :: String -> [Int] -> [Int] -> IO ()
    -- printBuildCellWithStayRule name bRule sRule = do
    --     printBuildCellWithBirthRule name bRule
    --     putStrLn ""
    --     putStrLn $ "PERMANENCIA: " ++ sRule

    -- printBuildCellWithColor :: String -> [Int] -> [Int] -> Color -> IO ()
    -- printBuildCellWithColor name bRule sRule color = do
    --     printBuildCellWithStayRule name bRule sRule
    --     putStrLn ""
    --     do putStr "COR: "; setSGR [SetColor Foreground Vivid color]; putStrLn "██"; setSGR [Reset]

    -- colorMenu :: IO String
    -- colorMenu = do
    --     colorI <- getLine
    --     if isNothing $ selectColor colorI then colorMenu
    --     else return $ fromJust (selectColor colorI)

    selectColor :: String -> Maybe String
    selectColor color = case color of
        "1" -> Just "VERMELHO"
        "2" -> Just "VERDE"
        "3" -> Just "AMARELO"
        "4" -> Just "AZUL"
        "5" -> Just "MAGENTA"
        "6" -> Just "CIANO"
        "7" -> Just "BRANCO"
        _ -> Nothing
       
    -- printColorMenu :: IO ()
    -- printColorMenu = do
    --     putStrLn ""
    --     putStrLn "Escolha uma cor para a sua célula: "
    --     putStrLn "\n1) Preto\n2) Vermelho\n3) Verde\n4) Amarelo\n5) Azul\n6) Magenta\n7) Ciano\n8) Branco\n"
    --     putStr "Escolha: "


    removeAutomata :: FilePath -> IO()
    removeAutomata path = do 

        cellsJSON <- readCells path
        
        case decode cellsJSON :: Maybe [Cell] of 

            Nothing -> error "ARQUIVO app/storage/cells.json MODIFICADO OU CORROMPIDO\nAPAGUE O CONTEUDO DO MESMO E INSIRA [] NO SEU CONTEUDO\nTENTE EXECUTAR O PROGRAMA NOVAMENTE"

            Just cells -> do 
                
                if null cells then return ()
                else do
                    printTextFileWithClear "app/storage/ruleController/listOfCells.txt"

                    printEmptyLines 2

                    printCells cells 1

                    printEmptyLines 1

                    printTextFileNoClear "app/storage/ruleController/removeCellMenu.txt"

                    nameCellI <- getLine
                    let nameCellT = map toUpper nameCellI 
                    deleteCell path nameCellT


    printCells :: [Cell] -> Int -> IO()
    printCells [] n = return ()
    printCells (x:xs) n = do
        putStrLn $ "                                                                     " ++ show n ++ " - " ++ show x
        printCells xs (n + 1)

    