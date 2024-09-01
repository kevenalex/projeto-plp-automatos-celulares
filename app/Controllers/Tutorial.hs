module Controllers.Tutorial where

    import Utils.Render

    import Models.Grid
    import Models.Cell
    import Models.Rule
        
    import Controllers.CellController
    import Controllers.SimulationController (printGrid)
    import System.Console.ANSI 
    
    conways :: Cell
    conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE"
    
    tutorial :: IO()
    tutorial = do
        printScreen "app/storage/tutorial/intro.txt" True False
        printScreen "app/storage/tutorial/apresentaRegras.txt" True False
        setCursorColumn 79
        print conways