module Controllers.MenuEditarCellsController where
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
    
    
    menuCells :: FilePath -> IO()
    menuCells path = do
        
        clearScreen

        printEmptyLines 21

        cellsJSON <- readCells path
        
        case decode cellsJSON :: Maybe [Cell] of 

            Nothing -> do

                screen <- readFile "app/storage/screenNoCells.txt"

                let linhas = lines screen
        
                mapM_ printLessDelay linhas 
                
            Just cells -> do 

                printCells cells 1

                printEmptyLines 2

                putStrLn "                                                       1) ADICIONAR CELULA   2) DELETAR CELULA   3) VOLTAR"

        printEmptyLines 21

        option <- getLine

        case option of
            "1" -> do addAutomata path; menuCells path;
            "2" -> do removeAutomata path; menuCells path
            "3" -> return ()
            _ -> menuCells path 

    addAutomata ::FilePath ->  IO()
    addAutomata path = do

        printBuildCellQuestion "QUAL O NOME DA CELULA?" 

        nameCell <- getLine

        printBuildCellQuestion "QUAL A SUA REGRA DE NASCIMENTO?"

        nascStr <- getLine
        let nascList = map (\x -> read [x] :: Int) nascStr
        
        printBuildCellQuestion "QUAL A SUA REGRA DE PERMANENCIA?"
        
        stayStr <- getLine
        let stayList = map (\x -> read [x] :: Int) stayStr -- Todo verificar que esses números tão entre 1 e 8, e se são numerinhos

        let regra = Rule nascList stayList

        printBuildCellQuestion "QUAL A COR DA SUA CELULA?\n\n                                                           1) VERMELHO  2) VERDE  3) AMARELO  4) AZUL\n                                                           5) MAGENTA   6) CIANO  7) BRANCO"

        colorI <- getLine

        if isNothing $ selectColor colorI then do printRuleMenuColorError; return ();
        else do 
            let cell = Cell nameCell regra $ fromJust $ selectColor colorI
            addCell path cell
            return ()

    printBuildCellQuestion :: String ->  IO ()
    printBuildCellQuestion qst = do
        clearScreen
        printEmptyLines 21
        putStrLn "                                                                   CRIANDO UMA CELULA"
        printEmptyLines 4
        putStrLn $ "                                                                   " ++ qst
        printEmptyLines 21


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
        "1" -> Just "Vermelho"
        "2" -> Just "Verde"
        "3" -> Just "Amarelo"
        "4" -> Just "Azul"
        "5" -> Just "Magenta"
        "6" -> Just "Ciano"
        "7" -> Just "Branco"
        _ -> Nothing
       
    -- printColorMenu :: IO ()
    -- printColorMenu = do
    --     putStrLn ""
    --     putStrLn "Escolha uma cor para a sua célula: "
    --     putStrLn "\n1) Preto\n2) Vermelho\n3) Verde\n4) Amarelo\n5) Azul\n6) Magenta\n7) Ciano\n8) Branco\n"
    --     putStr "Escolha: "


    removeAutomata :: FilePath -> IO()
    removeAutomata path = do 

        clearScreen

        cellsJSON <- readCells path
        
        case decode cellsJSON :: Maybe [Cell] of 

            Nothing -> do

                screen <- readFile "app/storage/screenNoCells.txt"

                let linhas = lines screen
        
                mapM_ printLessDelay linhas 
                
            Just cells -> do 

                printEmptyLines 21

                putStrLn ""

                printCells cells 1

                putStrLn ""

        putStrLn ""
        
        putStrLn "                                                                     QUAL CELULA VOCE QUER REMOVER?"

        printEmptyLines 21

        nameCell <- getLine
        deleteCell path nameCell


    printCells :: [Cell] -> Int -> IO()
    printCells [] n = return ()
    printCells (x:xs) n = do
        putStrLn $ "                                                                     " ++ show n ++ "- " ++ show x
        printCells xs (n + 1)

    