module Models.Cell where

    import Models.Rule

    data Status = Live | Dead 

    instance Show Status where
        show Live = "Live"
        show Dead = "Dead"
         
    instance Show Cell where
        show cell = show (color cell)

    instance Eq Status where
        (==) Live Live = True
        (==) Dead Dead = True
        (==) _ _ = False

    data Cell =   
        Cell {
        status :: Status,
        rule :: Rule,
        color :: String -- N sei ainda como representar
        }
    

     

