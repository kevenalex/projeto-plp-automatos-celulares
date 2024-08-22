{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
-- pro generic, pra não ter que fazer implementação default, pra ler strings como bytecode

module Models.Cell where

    import Models.Rule
    import Data.Maybe (isJust)
    import Data.Aeson
    import GHC.Generics


    data Cell =   
        Cell {
        name :: String,
        rule :: Rule,
        color :: String 
        } deriving (Generic,ToJSON, FromJSON)
    

    instance Show Cell where
        show cell = show (name cell)


    isLive :: Maybe Cell -> Bool
    isLive mCell = isJust mCell
     

