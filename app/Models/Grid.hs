module Models.Grid where
    import System.IO
    import System.IO.Unsafe (unsafeDupablePerformIO)
    import Data.Matrix
    import Models.Cell

    data Grid = Grid {
        height :: Int,
        width :: Int,
        grade :: Matrix Cell
    }


    gridGenerate :: Int -> Int -> Cell -> Grid
    gridGenerate height width a = Grid height width (matrix height width (\_ -> a))

    insertCell :: Grid -> Cell -> (Int, Int) -> Grid
    insertCell grid cell (x,y) = Grid (height grid) (width grid) (setElem cell (x,y) (grade grid))


    -- gridUpdate :: Grid -> Grid
    -- gridUpdate = 