module Models.Cell where

    import Models.Ruler

    data Status = Live | Dead 

    instance Show Status where
        show Live = "Live"
        show Dead = "Dead"
         
    instance Show Cell where
        show cell = show (status cell)

    data Cell =   
        Cell {
        status :: Status,
        ruler :: Maybe Ruler,
        color :: String -- N sei ainda como representar
        }

     

