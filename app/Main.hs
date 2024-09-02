module Main where

import Controllers.CellController (menuCells)
import Controllers.SceneController (menuScenes)
import Controllers.SimulationController
import Data.Char
import System.Console.ANSI
import System.Exit (exitSuccess)
import Utils.Render
import Controllers.Tutorial (tutorial)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    clearScreen

    Just (_, screenHeight) <- getTerminalSize

    setCursorPosition (screenHeight - 1) 0

    selectOption False -- Laço principal

selectOption :: Bool -> IO ()
selectOption delay = do
    printScreen "app/storage/mainMenuController/mainMenu.txt" True delay

    opcao <- getLine
    if null opcao 
        then  do
            putStrLn "OPCÃO INVÁLIDA, TENTE NOVAMENTE"
            threadDelay 830000
            selectOption False
        else
            case head opcao of
                '1' -> do emptyScene "app/storage/cells.json"; selectOption False;
                -- '2' -> do menuAutomatasCarregarCena "app/storage/cells.json"; selectOption False;
                '3' -> do menuCells "app/storage/cells.json"; selectOption False
                '4' -> do tutorial; selectOption False;
                '5' -> do
                    setCursorPosition 0 0
                    printScreen "app/storage/mainMenuController/emptyMenu.txt" False True 
                    exitSuccess
                _ -> do
                    putStrLn "OPCÃO INVÁLIDA, TENTE NOVAMENTE"
                    threadDelay 830000
                    selectOption False