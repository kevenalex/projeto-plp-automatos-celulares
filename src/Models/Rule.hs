module Models.Rule where

    data Rule = Rule {
        birth :: [Int],
        stay :: [Int]
    } 

    instance Show Rule where
        show (Rule birth stay)= "B" ++ (concatMap show birth) ++ "/S" ++ (concatMap show stay)

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    (==) :: [Int] -> [Int] -> [Int] -> [Int] -> Bool
    (==) bRuleA sRuleA bRuleB sRuleB =
        if bRuleA == bRuleB && sRuleA == sRuleB then True
        else False
         