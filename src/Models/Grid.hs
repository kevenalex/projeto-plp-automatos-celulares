module Models.Grid where
    import Data.Matrix
    import Models.Cell
    import Data.Maybe
    import Models.Rule

    --- Função que recebe a proporção de uma matriz - Linhas e Colunas - e cria uma matriz de Células Mortas
    gridGenerate :: Int -> Int -> Matrix (Maybe Cell)
    gridGenerate rows cols = matrix rows cols (const Nothing)

    --- Função que recebe a proporção de uma matriz - Linhas e Colunas - e uma lista de células
    gridGenerateFromList :: Int -> Int -> [Maybe Cell] -> Matrix (Maybe Cell)
    gridGenerateFromList rows cols list = fromList rows cols list

    --- Função que recebe uma matriz de células, uma célula e a posição ao qual a mesma será adicionada na matriz(indexada em 1)
    insertCell :: Matrix (Maybe Cell) -> Cell -> (Int, Int) -> Matrix (Maybe Cell)
    insertCell grid cell (x,y) = setElem (Just cell) (x,y) grid

    --- Função que retorna a quantidade de células existentes na vizinhança de uma célula da grade. Esta função recebe a posição 
    --- da célula a ser analisada, e a matriz de celulas correspondente.
    numOfLiveNeighbors :: (Int, Int) -> Matrix (Maybe Cell) -> Int
    numOfLiveNeighbors (x,y) grid = length $ lifeCellsCoord (x,y) grid

    --- Função que retorna a quantidade de células mortas existentes na vizinhança de uma célula da grade. Esta função recebe
    --- a posição da célula a ser analisada, e a matriz de células correspondentes.
    numOfDeadNeighbors :: (Int, Int) -> Matrix (Maybe Cell) -> Int
    numOfDeadNeighbors (x,y) grid = 8 - numOfLiveNeighbors (x,y) grid

    --- Função que retorna uma Cell ou Nothing de uma matriz de Cell. A mesma recebe a posição da Cell e a matriz de células a ser
    --- trabalhada.
    getCell :: (Int, Int) -> Matrix (Maybe Cell) -> Maybe Cell
    getCell (x,y) cells = getElem x y cells

    --- Função que retorna uma lista com as coordenadas de todas as células vizinhas a determinada célula, que sejam válidas(dentro do
    -- escopo da matriz, ou seja, existe o tratamento para casos de coordenadas de borda)
    lifeCellsCoord :: (Int, Int) -> Matrix (Maybe Cell) -> [(Int, Int)]
    lifeCellsCoord (x,y) grid = [(row,col) | (row,col) <- list, isLive (getCell (row,col) grid)]
        where
            list = listOfCoord (x,y) grid

    -- Função que verifica se uma determinada coordenada faz parte do escopo de coordenadas de uma matriz
    validCoord :: (Int,Int) -> Int -> Int -> Bool
    validCoord (row,col) rowLimit colLimit
        | row < 1 || row > rowLimit = False
        | col < 1 || col > colLimit = False
        | otherwise = True

    -- Função que lista todas as coordenadas que estão na vizinhança de determinada coordenada (INDEPENDENTE DO ESCOPO DA MATRIZ)
    listOfCoord :: (Int,Int) -> Matrix (Maybe Cell) -> [(Int, Int)]
    listOfCoord (u,v) grid = [
                                coordOnTopLeft (u,v), coordOnTop (u,v), coordOnTopRight (u,v),
                                coordInLeft (u,v), coordInRight (u,v),
                                coordInBelowLeft (u,v), coordInBelow (u,v), coordInBelowRight (u,v)
                             ]

    -- Função que lista todas as coordenadas válidas da vizinhança de determinada coordenada, ou seja, todas aquelas coordenadas que
    -- não ultrapassam os limites da matriz
    listOfValidCoords :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
    listOfValidCoords coords rowLimit colLimit = [(x,y) | (x,y) <- coords, validCoord (x,y) rowLimit colLimit]
    
    -- Função que retorna a Cell que mais se repete na vizinhança de uma Cell
    mostFrequentlyCell :: (Int, Int) -> Matrix (Maybe Cell) -> Cell
    mostFrequentlyCell (row, col) grid = cell
        where
          (freq, cell) = biggestOnList frequencias
          frequencias = frequentyCells vizinhanca grid
          vizinhanca = lifeCellsCoord (row, col) grid
        
    
    --- Função que retorna uma tupla contendo a Cell que mais se repete e quantas vezes a mesma se repete
    biggestOnList :: [(Int, Cell)] -> (Int, Cell)
    biggestOnList ((n,cell):[]) = (n, cell)
    biggestOnList ((n,cell):xs) = if n > fst (biggestOnList xs) then (n,cell)
                                  else biggestOnList xs

    --- Função que retorna a frequência de cada tipo de Cell
    frequentyCells :: [(Int,Int)] -> Matrix (Maybe Cell) -> [(Int, Cell)]
    frequentyCells coords grid = [(freq, cell) | (x,y) <- coords, let freq = numTimesFoundCell (fromJust $ getCell (x,y) grid) coords grid, let cell = fromJust $ getCell (x,y) grid]

    --- Função que retorna quantas vezes tal Rule/Cell aparece em determinada sequência de Rule
    numTimesFoundCell :: Cell -> [(Int,Int)] -> Matrix (Maybe Cell) -> Int
    numTimesFoundCell cell coords grid = length (filter (== cell) cells)
        where
            cells = [fromJust (getCell (x,y) grid) | (x,y) <- coords]

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