module Utils.Ruler where

    data Ruler = Ruler {
        birth :: [Int],
        stay :: [Int]
    } 

    instance Show Ruler where
        show (Ruler birth stay)= "B" ++ (concatMap show birth) ++ "/S" ++ (concatMap show stay)
         