module Models.Cell where

    import Models.Rule
    import Data.Maybe (isJust)
         
    instance Show Cell where
        show cell = show (name cell)
        
    instance Eq Cell where
       (Cell name1 rule1 color1) == (Cell name2 rule2 color2) = (name1 == name2) && (rule1 == rule2) && (color1 == color2)

    data Cell =   
        Cell {
        name :: String,
        rule :: Rule,
        color :: String 
        }
    
    isLive :: Maybe Cell -> Bool
    isLive mCell = isJust mCell
     

