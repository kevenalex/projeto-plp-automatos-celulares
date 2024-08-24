module Models.Rule where

    data Rule = Rule {
        birth :: [Int],
        stay :: [Int]
    } 

    instance Show Rule where
        show (Rule birth stay)= "B" ++ (concatMap show birth) ++ "/S" ++ (concatMap show stay)

    instance Eq Rule where
        (Rule birth1 stay1) == (Rule birth2 stay2) = (birth1 == birth2) && (stay1 == stay2) 
         