module Controllers.AutomataController where
    import Files.Cell
    import Data.Aeson
    import Models.Cell
    import Models.Rule (Rule (Rule))
    import Data.Char (toUpper)
    import System.Console.ANSI
    import Data.Maybe
    import System.Process(system)
    
    
    menuAutomatas :: FilePath -> IO()
    menuAutomatas path = do
        cellsJSON <- readCells path
        
        case decode cellsJSON :: Maybe [Cell] of 
            Nothing -> do 
                putStrLn "Nenhum automato adicionado"
                putStrLn ""
                
            Just cells -> do 
                putStrLn "Lista de células existentes:"
                putStrLn ""
                printCells cells
                putStrLn ""

        putStrLn " Opções:"
        putStrLn ""
        putStrLn " - (A)dicionar célula\n - (D)eletar célula\n - (R)etornar ao menu principal"
        putStrLn ""
        putStr "Escolha: "
        option <- getLine

        case toUpper (head option) of
            'A' -> do addAutomata path; menuAutomatas path;
            'D' -> do removeAutomata path; menuAutomatas path
            'R' -> putStr ""
            _ -> menuAutomatas path 

    addAutomata ::FilePath ->  IO()
    addAutomata path = do
        putStrLn ""
        putStr "Digite o nome da sua célula: "
        nameCell <- getLine

        putStrLn ""
        putStrLn "Digite uma sequência de números representando a quantidade de vizinhos para instanciar a regra"
        putStrLn ""

        putStr "Regra de nascimento: "
        nascStr <- getLine
        let nascList = map (\x -> read [x] :: Int) nascStr

        putStr "Regra de permanência: "
        stayStr <- getLine
        let stayList = map (\x -> read [x] :: Int) stayStr -- Todo verificar que esses números tão entre 1 e 8, e se são numerinhos

        let regra = Rule nascList stayList
        
        colorI <- colorMenu

        let cell = Cell nameCell regra colorI
        addCell path cell

        putStrLn $ "Célula " ++  name cell ++ "adicionada"
        menuAutomatas path

    colorMenu :: IO String
    colorMenu = do
        printColorMenu
        colorI <- getLine
        putStr ""
        if isNothing $ selectColor colorI then colorMenu
        else return $ fromJust (selectColor colorI)

    selectColor :: String -> Maybe String
    selectColor color = case color of
        "1" -> Just "Preto"
        "2" -> Just "Vermelho"
        "3" -> Just "Verde"
        "4" -> Just "Amarelo"
        "5" -> Just "Azul"
        "6" -> Just "Magenta"
        "7" -> Just "Ciano"
        "8" -> Just "Branco"
        _ -> Nothing
       
    printColorMenu :: IO ()
    printColorMenu = do
        putStrLn ""
        putStrLn "Escolha uma cor para a sua célula: "
        putStrLn "\n1) Preto\n2) Vermelho\n3) Verde\n4) Amarelo\n5) Azul\n6) Magenta\n7) Ciano\n8) Branco\n"
        putStr "Escolha: "


    removeAutomata :: FilePath -> IO()
    removeAutomata path = do 
        putStrLn ""
        putStr "Qual célula você quer remover: "
        nameCell <- getLine
        deleteCell path nameCell


    printCells :: [Cell] -> IO()
    printCells [] = return ()
    printCells (x:xs) = do
        putStrLn $ "- " ++ show x
        printCells xs