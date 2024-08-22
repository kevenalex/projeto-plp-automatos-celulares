{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models.Rule where

    import GHC.Generics
    import Data.Aeson

    data Rule = Rule {
        birth :: [Int],
        stay :: [Int]
    } deriving (Generic, ToJSON, FromJSON)

    instance Show Rule where
        show (Rule birth stay)= "B" ++ (concatMap show birth) ++ "/S" ++ (concatMap show stay)

    instance Eq Rule where
        (Rule birth1 stay1) == (Rule birth2 stay2) = (birth1 == birth2) && (stay1 == stay2) 
         