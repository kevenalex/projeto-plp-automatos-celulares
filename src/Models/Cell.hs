{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
-- pro generic, pra não ter que fazer implementação default, pra ler strings como bytecode

module Models.Cell where

    import Models.Rule
    import Data.Maybe (isJust)
    import Data.Aeson
    import GHC.Generics
    import System.Console.ANSI (Color)
    

    data Cell =   
        Cell {
        name :: String,
        rule :: Rule,
        color :: String
        } deriving (Generic,ToJSON, FromJSON)

    instance Show Cell where
        show cell = name cell ++ " " ++ (show $ rule cell) ++ " Cor: " ++ color cell

    instance Eq Cell where
       (Cell name1 rule1 color1) == (Cell name2 rule2 color2) = (name1 == name2) && (rule1 == rule2) && (color1 == color2)
    
    instance Ord Cell where
        (<) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 < name2
        (<=) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 <= name2
        (>) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 > name2
        (>=) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 >= name2

    isAlive :: Maybe Cell -> Bool
    isAlive = isJust 
