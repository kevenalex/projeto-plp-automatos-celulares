module Controllers.AutomataController where
    import Files.Cell
    import Data.Aeson
    import Models.Cell
    import Models.Rule (Rule (Rule))
    import Data.Char (toUpper)
    
    menuAutomatas :: FilePath -> IO()
    menuAutomatas path = do
        cellsJSON <- readCells path
        
        case decode cellsJSON :: Maybe [Cell] of 
            Nothing -> putStrLn "Nenhum automato adicionado ainda"
            Just cells -> printCells cells
        
        putStrLn " - (A)dicionar\n - (D)eletar automato\n - (R)etornar"
        option <- getLine
        case  toUpper (head option) of
            'A' -> do addAutomata path; menuAutomatas path
            'D' -> do removeAutomata path; menuAutomatas path
            'R' -> putStr ""
            _ -> menuAutomatas path 

    addAutomata ::FilePath ->  IO()
    addAutomata path = do
        
        putStr "Digite o nome da sua célula: "
        nameCell <- getLine

        putStrLn ""
        putStrLn "Digite uma sequência de números representando a quantidade de vizinhos para instanciar a regra"

        putStr "Regra de nascimento: "
        nascStr <- getLine
        let nascList = map (\x -> read [x] :: Int) nascStr

        putStr "Regra de permanênmcia: "
        stayStr <- getLine
        let stayList = map (\x -> read [x] :: Int) stayStr -- Todo verificar que esses números tão entre 1 e 8, e se são numerinhos

        let regra = Rule nascList stayList
        
        putStrLn "Escolha uma cor para a sua célula"
        putStrLn "Escolhe uma boa pfvr só tem tipo 14 escolhas e 7 são iguais"
        let color = "vermei"

        let cell = Cell nameCell regra color
        addCell path cell

        putStrLn $ "Celula " ++  name cell ++ "Adicionada"
        menuAutomatas path

    removeAutomata :: FilePath -> IO()
    removeAutomata path = do 
        putStr "Qual célula você quer remover: "
        nameCell <- getLine
        deleteCell path nameCell


    printCells :: [Cell] -> IO()
    printCells [] = return ()
    printCells (x:xs) = do
        putStrLn $ "- " ++ show x
        printCells xs