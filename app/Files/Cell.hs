{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Files.Cell (readCells, addCell) where
    import Data.Aeson
    import GHC.Generics

    import qualified Data.Map as M
    import qualified Data.ByteString.Lazy as B
    import System.Directory

    import Models.Cell
    import Models.Rule
    import Models.Grid
    

    -- path = "./app/storage/cells.json"
    -- pra pegar a lista de células você ainda tem que tornar isso puro,
    -- "addCell" ali é um exemplo de como fazer e tratar
    -- tira o IO num do, decoda o JSON e trata o Nothing e Just.
    readCells :: FilePath -> IO B.ByteString
    readCells path = do
        exists <- doesFileExist path
        if exists 
            then 
                B.readFile path
            else do
                B.writeFile path B.empty
                B.readFile path 
    
    --substitui o arquivo inteiro
    saveCells ::  FilePath -> [Cell] -> IO()
    saveCells path cells = B.writeFile path (encode cells)

    addCell ::  FilePath -> Cell -> IO()
    addCell path cell = do
        cellsJSON <- readCells path
        case decode cellsJSON :: Maybe [Cell] of
            Nothing -> saveCells path [cell]
            Just cells -> if cell `elem` cells 
                            then saveCells path cells
                          else saveCells path (cell : cells) 
    -- Esse then é peba, provavelmente eu devia printar aqui,
    -- mas eu quero ter certeza que não vai printar fora do lugar
    -- na real devia ser um erro ne


