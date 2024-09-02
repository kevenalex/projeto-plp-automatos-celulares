{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
-- pro generic, pra não ter que fazer implementação default, pra ler strings como bytecode

module Models.Cell where

    import Models.Rule
    import Data.Maybe (isJust)
    import Data.Aeson
    import GHC.Generics
    import System.Console.ANSI (Color)
    import Data.Char (toUpper)
    import System.Console.ANSI.Codes

    data Cell =   
        Cell {
        name :: String,
        rule :: Rule,
        color :: String
        } deriving (Generic,ToJSON, FromJSON)


    instance Show Cell where
        show cell =  setSGRCode (setColor (color cell)) ++ name cell ++ " " ++ show (rule cell) ++ setSGRCode [Reset]

    instance Eq Cell where
       (Cell name1 _ _) == (Cell name2 _ _) = name1 == name2
    
    instance Ord Cell where
        (<) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 < name2
        (<=) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 <= name2
        (>) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 > name2
        (>=) (Cell name1 rule1 color1) (Cell name2 rule2 color2) = name1 >= name2

    isAlive :: Maybe Cell -> Bool
    isAlive = isJust 

    setColor :: String -> [SGR]
    setColor str =
        case str of 
            "VERMELHO" -> [SetColor Foreground Dull Red]
            "VERMELHO  BRILHANTE" -> [SetColor Foreground Vivid Red]
            "VERDE" -> [SetColor Foreground Dull Green]
            "VERDE BRILHANTE" -> [SetColor Foreground Vivid Green]
            "AMARELO" -> [SetColor Foreground Dull Yellow]
            "AMARELO BRILHANTE" -> [SetColor Foreground Vivid Yellow]
            "AZUL" -> [SetColor Foreground Dull Blue]
            "AZUL BRILHANTE" -> [SetColor Foreground Vivid Blue]
            "MAGENTA" -> [SetColor Foreground Dull Magenta]
            "MAGENTA BRILHANTE" -> [SetColor Foreground Vivid Magenta]
            "CIANO" -> [SetColor Foreground Dull Cyan]
            "CIANO BRILHANTE" -> [SetColor Foreground Vivid Cyan]
            "BRANCO" -> [SetColor Foreground Dull White]
            "BRANCO BRILHANTE" -> [SetColor Foreground Vivid White]
            _ -> [Reset]

