module Controllers.Tutorial where

    import Utils.Render

    import Models.Grid
    import Models.Cell
    import Models.Rule
        
    import Controllers.CellController
    import Controllers.SimulationController (printGrid)
    import System.Console.ANSI 
    import Data.Matrix ( Matrix )
    
    conways :: Cell
    conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE"

    square :: Matrix (Maybe Cell)
    square = gridGenerate 3 3
    
    tutorial :: IO()
    tutorial = do
        printScreen "app/storage/tutorial/intro.txt" True False
        _ <- getLine

        printScreen "app/storage/tutorial/apresentaRegras.txt" True False
        setCursorColumn 87
        print conways
        printEmptyLines 25
        putStrLn "aperte Enter para continuar..."
        _ <- getLine

        setCursorColumn 87
        print conways
        printScreen "app/storage/tutorial/explicaBirth.txt" True False

        _ <- getLine
