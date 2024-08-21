module Models.Teste where

    import Models.Cell
    import Data.Matrix

    import Models.Rule

    import Data.Maybe
    import Data.List


    gridGenerateFromList :: Int -> Int -> [Maybe Cell] -> Matrix (Maybe Cell)
    gridGenerateFromList rows cols list = fromList rows cols list

    getCell :: (Int, Int) -> Matrix (Maybe Cell) -> Maybe Cell
    getCell (x,y) cells = getElem x y cells
