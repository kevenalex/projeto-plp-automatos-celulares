{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Files where
    import Data.Aeson
    import GHC.Generics

    import qualified Data.Map as M
    import Data.ByteString.Lazy as B
    import Models.Cell
    import Models.Rule
    import Models.Grid
    import System.Directory

    --  path = "./app/storage/cells.json"
    readCells :: FilePath ->IO B.ByteString
    readCells path = do
        exists <- doesFileExist path
        if exists then
            B.readFile path
            else do
                B.writeFile path B.empty
                B.readFile path 
    
    saveCells ::  FilePath -> [Cell] -> IO()
    saveCells path cells = B.writeFile path (encode cells)

    addCell ::  FilePath -> Cell -> IO()
    addCell path cell = do
        cellsJSON <- readCells path
        case decode cellsJSON :: Maybe [Cell] of
            Nothing -> saveCells path [cell]
            Just cells -> if cell `Prelude.elem` cells 
                            then saveCells path cells
                          else saveCells path (cell : cells) 
    -- Esse then é peba, provavelmente eu devia printar aqui,
    -- mas eu quero ter certeza que não vai ficar fora do lugar