module Controllers.CellController where
    import Files.Cell
    import Data.Aeson
    import Data.Char (toUpper)
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
        if null cells then printTextFileWithClear "app/storage/ruleController/screenNoCells.txt"
                    else do 
                        printTextFileWithClear "app/storage/ruleController/listOfCells.txt"

                        printEmptyLines 2

                        printCells cells 1

                        printEmptyLines 1

                        printTextFileNoClear "app/storage/ruleController/ruleMenuOptions.txt"


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
        let stayList = map (\x -> read [x] :: Int) $ trim stayStr -- Todo verificar que esses números tão entre 1 e 8, e se são numerinhos

        let regra = Rule nascList stayList

        printTextFileWithClear "app/storage/ruleController/colorMenu.txt"

        colorI <- getLine

        if isNothing $ selectColor colorI then do printTextFileWithClear "app/storage/ruleController/ruleMenuColorError.txt"; threadDelay 200000;
        else do 
            let cell = Cell nameCellT regra $ fromJust $ selectColor colorI
            addCell path cell


    removeAutomata :: FilePath -> [Cell] -> IO()
    removeAutomata path cells = do 
        
        if null cells 
            then
                return ()
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
       