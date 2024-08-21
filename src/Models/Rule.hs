module Models.Rule where

    data Rule = Rule {
        birth :: [Int],
        stay :: [Int]
    } 

    instance Show Rule where
        show (Rule birth stay)= "B" ++ (concatMap show birth) ++ "/S" ++ (concatMap show stay)
         