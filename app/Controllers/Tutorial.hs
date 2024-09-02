module Controllers.Tutorial where

    import Utils.Render

    import Models.Grid
    import Models.Cell
    import Models.Rule
        
    import Controllers.SimulationController (printGrid, printRow)
    import System.Console.ANSI 
    import Data.Matrix ( Matrix )
    import System.IO    
    import GHC.Conc
    
    conways :: Cell
    conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE"

    highLife :: Cell
    highLife = Cell "High Life" (Rule [3, 6] [2,3]) "VERMELHO"

    square :: Matrix (Maybe Cell)
    square = gridGenerate 3 3
    
    tutorial :: IO()
    tutorial = do
        hSetEcho stdin False
        hideCursor

        printScreen "app/storage/tutorial/intro.txt" True False
        _ <- getLine

        printScreen "app/storage/tutorial/apresentaRegras.txt" True False
        setCursorColumn 90
        print conways
        printEmptyLinesWithDelay 25
        printMidScreen "aperte Enter para continuar..."
        _ <- getLine

        printMidScreen $ show conways

        printScreen "app/storage/tutorial/explicaBirth.txt" True False
        let gridInicial = insertCells square conways [(1,1), (1,3), (3,2)]
        printGridTutorial gridInicial
        printEmptyLinesWithDelay 20
        _ <- getLine

        printGridTutorial gridInicial
        printEmptyLinesWithDelay 1
        printMidScreen "|"
        printMidScreen "V"
        printEmptyLinesWithDelay 1
        printGridTutorial $ gridUpdate gridInicial

        printEmptyLinesWithDelay 1
        setCursorColumn 90
        print conways
        printScreen "app/storage/tutorial/explicaStay.txt" False False
        
        printEmptyLinesWithDelay 18
        printMidScreen "aperte Enter para continuar..."
        _ <- getLine
        
        clearScreen   
        printMidScreen "No caso abaixo, todas as células estão confortavelmente juntas. Cada uma tem exatamente 3 vizinhos."

        let grid2 = insertCells square conways [(1,1), (1,2),(2,1), (2,2)]

        printGridTutorial grid2
        printEmptyLinesWithDelay 1
        setCursorColumn 101
        printMidScreen "|"
        printMidScreen "V"
        printEmptyLinesWithDelay 1
        printGridTutorial $ gridUpdate grid2
        printMidScreen "aperte Enter para ver nada catastrófico..."
        printEmptyLinesWithDelay 30

        _ <- getLine
        let grid3 = insertCells (gridGenerate 10 25) conways [(4, 20), (5,20), (4, 21), (5,21)]
        let gridAtaque = insertCells grid3 highLife [(4,1), (5,1), (4,2), (5,2), (6,2), (3,3), (5,3), (6,3), (3,4), (4,4), (5,4), (4,5)]
        setCursorColumn 47
        putStrLn $ "Meu Deus! Um pato do tipo " ++ show highLife ++ " está atacando nosso quadrado " ++ show conways ++ "!"
        printGridTutorial gridAtaque
        printMidScreen "aperte Enter para continuar!"
        _ <- getLine
        hSetBuffering stdout (BlockBuffering Nothing)
        ataque gridAtaque 0
        hSetBuffering stdout NoBuffering
        printScreen "app/storage/tutorial/final.txt" False False
        printMidScreen "aperte Enter para voltar!"
        _ <- getLine

        hSetEcho stdin True
        showCursor


        
    ataque :: Matrix (Maybe Cell) -> Int -> IO()
    ataque _ 65 = return()
    ataque grid n = do 
        printGrid grid
        setCursorColumn 81
        printMidScreen "aperte Enter para continuar! Ou A e Enter para passar automáticamente!"
        printEmptyLines 20
        hFlush stdout
        a <- getLine
                
        if null a then ataque (gridUpdate grid) (n + 1)
        else  
            if head a == 'A' || head a == 'a'
                then do
                    ataqueAutomatico (gridUpdate grid) (n + 1)
                    return () 
                else
                    ataque (gridUpdate grid) (n + 1)

    ataqueAutomatico :: Matrix (Maybe Cell) -> Int -> IO ()
    ataqueAutomatico _ 65 = return()
    ataqueAutomatico grid n = do
        printGrid grid
        printEmptyLines 20
        setCursorColumn 81
        hFlush stdout
        threadDelay 400000
        ataqueAutomatico (gridUpdate grid) (n + 1)

    printGridTutorial :: Matrix (Maybe Cell) -> IO()
    printGridTutorial grid =  mapM_ printRow (gridToLists grid)