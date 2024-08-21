module Models.Teste where

    coordOnTop :: (Int, Int) -> Maybe (Int, Int)
    coordOnTop (x,y)

        | u < 0 = Nothing
        | otherwise = Just (u,v)

        where 
            (u,v) = (x-1, y)