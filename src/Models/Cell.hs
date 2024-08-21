module Models.Cell where

    import Models.Rule
    import Data.Maybe (isJust)
         
    instance Show Cell where
        show cell = show (name cell)

    data Cell =   
        Cell {
        name :: String,
        rule :: Rule,
        color :: String 
        }
    
    isLive :: Maybe Cell -> Bool
    isLive mCell = isJust mCell
     

