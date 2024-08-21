module Models.Grid where
    import Data.Matrix
    import Models.Cell
    import Data.Maybe

    data Grid = Grid {
        height :: Int,
        width :: Int,
        cells :: Matrix Cell
    }


    gridGenerate :: Int -> Int -> Cell -> Grid
    gridGenerate height width a = Grid height width (matrix height width (const a))

    insertCell :: Grid -> Cell -> (Int, Int) -> Grid
    insertCell grid cell (x,y) = Grid (height grid) (width grid) (setElem cell (x,y) (cells grid))

    numOfLiveNeighbors :: (Int, Int) -> Grid -> Int
    numOfLiveNeighbors (x,y) grid = length [uv | uv <- validCoord (x,y) grid, status (getCell uv (cells grid)) == Live]

    numOfDeadNeighbors :: (Int, Int) -> Grid -> Int
    numOfDeadNeighbors (x,y) grid = 8 - numOfLiveNeighbors (x,y) grid

    getCell :: (Int, Int) -> Matrix Cell -> Cell
    getCell (x,y) cells = getElem x y cells

   --- gridUpdate :: Grid -> Grid
   --- gridUpdate grid = Grid rows cols (fromList rows cols newCells)
      ---  where
      ---      newCells = [nextCell (row,col) (getCell (row,col) (cells grid)) grid | row <- [1..rows], col <- [1..cols]]
      ---      rows = height grid
      ---      cols = width grid


   --- nextCell :: (Int,Int) -> Cell -> Grid -> Cell
   --- nextCell (x,y) cell grid = do
    ---    if status cell == Live then
   ---         if numLiveNeighbors `elem` stay regra then liveCell
      ---      else deadCell
     ---   else
      ---      if numOfLiveNeighbors `elem` birth regra then liveCell
      ---      else deadCell 

     ---   where 
      ---      regra = rule cell
      ---      live = stay (rule cell)
       ---     numLiveNeighbors = numOfLiveNeighbors (x,y) grid
        ---    liveCell = Cell Live (rule cell) (color cell)
        ---    deadCell = Cell Dead (rule cell) (color cell)

    --- -----------------------------------------------------------------------------------------------------------------------

    --- UM MOI DE FUNCAO QUE CALCULA A POSICAO DOS VIZINHOS DE CADA CELULA

    validCoord :: (Int, Int) -> Grid -> [(Int, Int)]
    validCoord (x,y) grid = [fromJust x | x <- list, isJust x]
        where
            list = listOfCoord (x,y) grid

    listOfCoord :: (Int,Int) -> Grid -> [Maybe  (Int, Int)]
    listOfCoord (u,v) grid = [coordOnTopLeft (u,v) grid, coordOnTop (u,v) grid, coordOnTopRight (u,v) grid,
                         coordInLeft (u,v) grid, coordInRight (u,v) grid,
                         coordInBelowLeft (u,v) grid, coordInBelow (u,v) grid, coordInBelowRight (u,v) grid]


    coordOnTop :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordOnTop (x,y) grid

        | u >= 1 && u <= height grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x-1, y)

    coordInBelow :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordInBelow (x,y) grid

        | u >= 1 && u <= height grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x+1, y)

    coordInRight :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordInRight (x,y) grid

        | v >= 1 && v <= width grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x, y + 1)

    coordInLeft :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordInLeft (x,y) grid

        | v >= 1 && v <= width grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x, y - 1)

    coordOnTopRight :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordOnTopRight (x,y) grid

        | u >= 1 && u <= height grid  && v >= 1 && v <= width grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x - 1, y + 1)

    coordInBelowRight :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordInBelowRight (x,y) grid

        | u >= 1 && u <= height grid  && v >= 1 && v <= width grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x + 1, y + 1)

    coordOnTopLeft :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordOnTopLeft (x,y) grid

        | u >= 1 && u <= height grid  && v >= 1 && v <= width grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x - 1, y - 1)

    coordInBelowLeft :: (Int, Int) -> Grid -> Maybe (Int, Int)
    coordInBelowLeft (x,y) grid

        | u >= 1 && u <= height grid  && v >= 1 && v <= width grid = Just (u,v)
        | otherwise = Nothing

        where
            (u,v) = (x + 1, y - 1)

    --- -----------------------------------------------------------------------------------------------------------------------