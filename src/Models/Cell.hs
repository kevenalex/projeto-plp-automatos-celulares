module Models.Cell where

    import Models.Rule
    import Data.Maybe (isJust)
    import Data.Aeson
    import GHC.Generics 

    instance Show Cell where
        show cell = show (name cell)


    data Cell =   
        Cell {
        name :: String,
        rule :: Rule,
        color :: String 
        }
    
    cu:: IO()
    cu = print (toEncoding ["a", "b"])

    isLive :: Maybe Cell -> Bool
    isLive mCell = isJust mCell
     

