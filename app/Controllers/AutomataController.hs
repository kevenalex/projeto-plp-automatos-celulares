module Controllers.AutomataController where
    import Files.Cell
    import Data.Aeson
    import Models.Cell
    import Data.Maybe (fromJust)
    
    main :: IO()
    main = do
        let path = "./app/storage/cells.json"
        
        cellsJSON <- readCells path
        let cells = decode cellsJSON :: Maybe [Cell]
        print $ fromJust cells
        
        putStrLn " - (A)dicionar\n - (R)emover automato\n - (V)oltar"
        option <- getLine
        case option of
            -- "A" -> addAutomata path
            "R" -> removeAutomata path
            "V" -> putStrLn "Xau"
            _ -> putStrLn "vsf"

    removeAutomata :: FilePath -> IO()
    removeAutomata path = do
        putStrLn "Digite o nome da automato que você quer remover:"
        
        

    -- addAutomata ::FilePath ->  IO()
    -- addAutomata path = do