module Models.Grid where
    import Data.Matrix
    import Models.Cell
    import Data.Maybe
    import Models.Rule

    import qualified Data.Set as Set

    ---  Recebe a proporção de uma matriz - Linhas e Colunas - e cria uma matriz de Células Mortas
    gridGenerate :: Int -> Int -> Matrix (Maybe Cell)
    gridGenerate rows cols = matrix rows cols (const Nothing)

    ---  Recebe a proporção de uma matriz - Linhas e Colunas - e uma lista de células
    gridGenerateFromList :: Int -> Int -> [Maybe Cell] -> Matrix (Maybe Cell)
    gridGenerateFromList = fromList

    ---  Recebe uma matriz de células, uma célula e a posição ao qual a mesma será adicionada na matriz(indexada em 1)
    insertCell :: Matrix (Maybe Cell) -> Cell -> (Int, Int) -> Matrix (Maybe Cell)
    insertCell grid cell (x,y) = setElem (Just cell) (x,y) grid

    -- recebe uma matriz, a célula a ser inserida e uma lista de coordenadas onde ela vai ser inserida
    insertCells :: Matrix (Maybe Cell) -> Cell -> [(Int,Int)] -> Matrix (Maybe Cell)
    insertCells grid cell (h:t) = insertCells (insertCell grid cell h) cell t
    insertCells grid _ [] = grid

    removeCells :: Matrix (Maybe Cell) -> [(Int,Int)] -> Matrix (Maybe Cell)
    removeCells grid [] = grid
    removeCells grid (h:t) = removeCells (removeCell grid h) t

    removeCell :: Matrix (Maybe Cell)  -> (Int, Int) -> Matrix (Maybe Cell)
    removeCell grid (x,y) = setElem Nothing (x,y) grid

    ---  Retorna a quantidade de células existentes na vizinhança de uma célula da grade. Esta função recebe a posição 
    --- da célula a ser analisada, e a matriz de celulas correspondente.
    numOfLiveNeighbors :: (Int, Int) -> Matrix (Maybe Cell) -> Int
    numOfLiveNeighbors (x,y) grid = length $ lifeCellsCoord (x,y) grid

    ---  Retorna a quantidade de células mortas existentes na vizinhança de uma célula da grade. Esta função recebe
    --- a posição da célula a ser analisada, e a matriz de células correspondentes.
    numOfDeadNeighbors :: (Int, Int) -> Matrix (Maybe Cell) -> Int
    numOfDeadNeighbors (x,y) grid = 8 - numOfLiveNeighbors (x,y) grid

    ---  Retorna uma Cell ou Nothing de uma matriz de Cell. A mesma recebe a posição da Cell e a matriz de células a ser
    --- trabalhada.
    getCell :: (Int, Int) -> Matrix (Maybe Cell) -> Maybe Cell
    getCell (x,y) = getElem x y

    ---  Retorna uma lista com as coordenadas de todas as células vizinhas a determinada célula, que sejam válidas(dentro do
    -- escopo da matriz, ou seja, existe o tratamento para casos de coordenadas de borda)
    lifeCellsCoord :: (Int, Int) -> Matrix (Maybe Cell) -> [(Int, Int)]
    lifeCellsCoord (x,y) grid = [(row,col) | (row,col) <- list, validCoord (row,col) rowLimit colLimit && isAlive (getCell (row,col) grid)]
        where
            rowLimit = nrows grid
            colLimit = ncols grid
            list = listOfCoord (x,y) grid

    --  Verifica se uma determinada coordenada faz parte do escopo de coordenadas de uma matriz
    validCoord :: (Int,Int) -> Int -> Int -> Bool
    validCoord (row,col) rowLimit colLimit
        | row < 1 || row > rowLimit = False
        | col < 1 || col > colLimit = False
        | otherwise = True

    --  Lista todas as coordenadas que estão na vizinhança de determinada coordenada (INDEPENDENTE DO ESCOPO DA MATRIZ)
    listOfCoord :: (Int,Int) -> Matrix (Maybe Cell) -> [(Int, Int)]
    listOfCoord (u,v) grid = [
                                coordOnTopLeft (u,v), coordOnTop (u,v), coordOnTopRight (u,v),
                                coordInLeft (u,v), coordInRight (u,v),
                                coordInBelowLeft (u,v), coordInBelow (u,v), coordInBelowRight (u,v)
                             ]

    --  Lista todas as coordenadas válidas da vizinhança de determinada coordenada, ou seja, todas aquelas coordenadas que
    -- não ultrapassam os limites da matriz
    listOfValidCoords :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
    listOfValidCoords coords rowLimit colLimit = [(x,y) | (x,y) <- coords, validCoord (x,y) rowLimit colLimit]

    --  Retorna a Cell que mais se repete na vizinhança de uma Cell
    mostFrequentCell :: (Int, Int) -> Matrix (Maybe Cell) -> Maybe Cell
    mostFrequentCell (row, col) grid = 
        if isNothing cell then Nothing else cell
        where
          (_, cell) = biggestOnList frequencias
          frequencias = frequencyCells vizinhanca grid
          vizinhanca = lifeCellsCoord (row, col) grid


    ---  Retorna uma tupla contendo a Cell que mais se repete e quantas vezes a mesma se repete
    biggestOnList :: [(Int, Cell)] -> (Int, Maybe Cell)
    biggestOnList [] = (0, Nothing)
    biggestOnList [(n,cell)] = (n, Just cell)
    biggestOnList ((n,cell):xs) = if n > fst (biggestOnList xs) then (n, Just cell)
                                  else biggestOnList xs

    ---  Retorna a frequência de cada tipo de Cell
    frequencyCells :: [(Int,Int)] -> Matrix (Maybe Cell) -> [(Int, Cell)]
    frequencyCells coords grid = Set.toList (Set.fromList allCells)
        where
            allCells = [(freq, cell) | (x,y) <- coords, let freq = numTimesFoundCell (fromJust $ getCell (x,y) grid) coords grid, let cell = fromJust $ getCell (x,y) grid]

    ---  Retorna quantas vezes tal Rule/Cell aparece em determinada sequência de Rule
    numTimesFoundCell :: Cell -> [(Int,Int)] -> Matrix (Maybe Cell) -> Int
    numTimesFoundCell cell coords grid = length (filter (== cell) cells)
        where
            cells = [fromJust (getCell (x,y) grid) | (x,y) <- coords]

    gridUpdate :: Matrix (Maybe Cell) -> Matrix (Maybe Cell)
    gridUpdate grid = fromList rows cols newCells
        where
            rows = nrows grid
            cols = ncols grid
            newCells = [nextCell (row,col) grid | row <- [1..rows], col <- [1..cols]]

    nextCell :: (Int, Int) -> Matrix (Maybe Cell) -> Maybe Cell
    nextCell coord grid =
        if isAlive cell then nextFromLiveCell coord grid
        else nextFromDeadCell coord grid

        where
            cell = getCell coord grid

    nextFromDeadCell :: (Int,Int) -> Matrix (Maybe Cell) -> Maybe Cell
    nextFromDeadCell coord grid = if null coordsProposedRules then Nothing
                                  else snd (biggestOnList frequenty)

        where
            numNeighbors = numOfLiveNeighbors coord grid
            coordLiveNeighbors = lifeCellsCoord coord grid
            coordsProposedRules = [(x,y) | (x,y) <- coordLiveNeighbors, numNeighbors `elem` birth (rule $ fromJust (getCell (x,y) grid))]
            frequenty = frequencyCells coordsProposedRules grid

            -- cellsNeighborhood = [c | (freq, c) <- frequencyCells coordLiveNeighbors grid]
            -- bRules = [birth (rule cell) | cell <- cellsNeighborhood]


    ---  Retorna o update de uma Cell viva a partir de sua regra e vizinhança
    nextFromLiveCell :: (Int,Int) -> Matrix (Maybe Cell) -> Maybe Cell
    nextFromLiveCell coord grid =
        if numNeighbors `elem` sRule then cell
        else deadCell

        where
            cell = getCell coord grid
            numNeighbors = numOfLiveNeighbors coord grid
            sRule = stay (rule $ fromJust cell)
            deadCell = Nothing

    ---  Retorna se não houve mudanças entre duas gerações da simulação
    noChangeGenerations :: Matrix (Maybe Cell) -> Matrix (Maybe Cell) -> Bool
    noChangeGenerations gridA gridB = gridA == gridB

    ---  Retorna se uma determinada Matriz é composta absolutamente por células mortas
    isDeadSimulation :: Matrix (Maybe Cell) -> Bool
    isDeadSimulation grid = grid == deadGrid
        where
            deadGrid = gridGenerateFromList (nrows grid) (ncols grid) [Nothing | _ <- [1.. (nrows grid * ncols grid) ]]

    gridToLists :: Matrix (Maybe Cell) -> [[Maybe Cell]]
    gridToLists = toLists
    --- -----------------------------------------------------------------------------------------------------------------------

    --- Sequência de funções que calculam as coordenadas de todas as direções a partir de determinada coordenada

    coordOnTop :: (Int, Int) -> (Int, Int)
    coordOnTop (x,y) = (x-1, y)

    coordInBelow :: (Int, Int) -> (Int, Int)
    coordInBelow (x,y) = (x+1, y)

    coordInRight :: (Int, Int) -> (Int, Int)
    coordInRight (x,y) = (x, y + 1)

    coordInLeft :: (Int, Int) -> (Int, Int)
    coordInLeft (x,y) = (x, y - 1)

    coordOnTopRight :: (Int, Int) -> (Int, Int)
    coordOnTopRight (x,y) = (x - 1, y + 1)

    coordInBelowRight :: (Int, Int) -> (Int, Int)
    coordInBelowRight (x,y) = (x + 1, y + 1)

    coordOnTopLeft :: (Int, Int) -> (Int, Int)
    coordOnTopLeft (x,y) = (x - 1, y - 1)

    coordInBelowLeft :: (Int, Int) -> (Int, Int)
    coordInBelowLeft (x,y) = (x + 1, y - 1)

    --- -----------------------------------------------------------------------------------------------------------------------
