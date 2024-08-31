module Controllers.CellController where
    import Files.Cell
    import Data.Aeson
    import Data.Char (toUpper)
    import Data.Char
    import Data.Maybe

    import Models.Cell ( Cell(Cell) )
    import Models.Rule (Rule (Rule))

    import System.Console.ANSI
    import Utils.Render

    import Control.Concurrent(threadDelay)
    import qualified Data.ByteString.Lazy as B

    menuCells :: FilePath -> IO()
    menuCells path = do

        cellsJSON <- readCells path
        
        let cells = case decode cellsJSON :: Maybe [Cell] of 
                Nothing -> do  
                        if B.length cellsJSON > 2 -- Testando se o arquivo tá vazio ou não. B.null não funciona pra isso, 
                            -- e esse teste só existe pra evitar sobrepor um arquivo parcialmente corrompido 
                            -- que o usuario poderia querer corrigir na mão
                            then 
                                error "ARQUIVO app/storage/cells.json MODIFICADO OU CORROMPIDO\nAPAGUE-O\nE TENTE EXECUTAR O PROGRAMA NOVAMENTE"
                            else
                                []
                Just c -> c
        listCells cells
                
        option <- getLine

        case option of
            "1" -> do addAutomata path; menuCells path;
            "2" -> do removeAutomata path cells; menuCells path
            "3" -> return ()
            _ -> menuCells path



    listCells :: [Cell] -> IO()
    listCells cells = 
        if null cells then printScreen "app/storage/ruleController/screenNoCells.txt" True False;
                    else do 
                        printScreen "app/storage/ruleController/listOfCells.txt" True False

                        printEmptyLines 2

                        printCells cells 1

                        printEmptyLines 1

                        printScreen "app/storage/ruleController/ruleMenuOptions.txt" False False

    addAutomata :: FilePath ->  IO()
    addAutomata path = do

        printScreen "app/storage/ruleController/nameCellQuestion.txt" True False

        nameCellNT <- getLine
        let nameCellT = map toUpper nameCellNT

        printScreen "app/storage/ruleController/birthRule.txt" True False

        addBirthRule path nameCellT
    
    -- Criação da Regra de Nascimento da Celula, o usuario pode inserir de 0 a 8 digitos entre 1 e 8.
    addBirthRule :: FilePath -> String -> IO()
    addBirthRule path nameCellT = do
        nascStr <- getLine
        if handleBornAndStayRule nascStr then do
            let nascList = map (\x -> read [x] :: Int) nascStr
            printScreen "app/storage/ruleController/stayRule.txt" True False
            addStayRule path nameCellT nascList
        
        else do
            printScreen "app/storage/ruleController/birthRuleError.txt" True False
            addBirthRule path nameCellT
    
    -- Criação da Regra de Permanencia da Celula, o usuario pode inserir de 0 a 8 digitos entre 1 e 8.
    addStayRule :: FilePath -> String -> [Int] -> IO()
    addStayRule path nameCellT nascList = do
        stayStr <- getLine
        if handleBornAndStayRule stayStr then do
            let stayList = map (\x -> read [x] :: Int) stayStr
            let regra = Rule nascList stayList
            printScreen "app/storage/ruleController/colorMenu.txt" True False
            addColor path nameCellT regra
        
        else do
            printScreen "app/storage/ruleController/stayRuleError.txt" True False
            addStayRule path nameCellT nascList
    
    -- Criação da Cor da Celula, o usuario pode inserir uma das 7 opções de cor que o terminal disponibiliza.
    addColor :: FilePath -> String -> Rule -> IO()
    addColor path nameCellT regra = do
        colorI <- getChar
        if handleColorChoice colorI then do
            let cell = Cell nameCellT regra $ fromJust $ selectColor colorI
            addCell path cell
        else do
            printScreen "app/storage/ruleController/colorMenuError.txt" True False
            addColor path nameCellT regra

    removeAutomata :: FilePath -> [Cell] -> IO()
    removeAutomata path cells = do 
        
        if null cells 
            then
                return ()
            else do
            
            printScreen "app/storage/ruleController/listOfCells.txt" True False

            printEmptyLines 2

            printCells cells 1

            printEmptyLines 1

            printScreen "app/storage/ruleController/removeCellMenu.txt" False False

            nameCellI <- getLine
            let nameCellT = map toUpper nameCellI 
            deleteCell path nameCellT

    printCells :: [Cell] -> Int -> IO()
    printCells [] _ = return ()
    printCells (x:xs) n = do
        putStrLn $ "                                                                     " ++ show n ++ " - " ++ show x
        printCells xs (n + 1)

    
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

    -- Faz o tratemento das entradas das regras de Nascimento e Permanência
    handleBornAndStayRule :: String -> Bool
    handleBornAndStayRule regra = length regra <= 8 && all (`elem` "12345678") regra

    -- Faz o tratamento da entrada de cor, verifica se ela não é um 'Enter' e se é um char entre '1' e '7'
    handleColorChoice :: Char -> Bool
    handleColorChoice cor = cor /= '\n' && isPrint cor && cor `elem` ['1'..'7']