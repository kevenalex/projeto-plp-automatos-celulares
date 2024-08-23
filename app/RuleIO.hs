{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module RuleIO where
    import Models.Cell
    import Models.Rule
    import Data.Aeson
    import qualified Data.ByteString.Lazy as B 
    
    -- getRules :: [Maybe Cell]
    -- getRules = B.readFile