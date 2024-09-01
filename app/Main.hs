module Main where

import Controllers.CellController (menuCells)
import Controllers.SimulationController
import Data.Char
import System.Console.ANSI
import System.Exit (exitSuccess)
import Utils.Render
import Controllers.Tutorial (tutorial)

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
    case toUpper $ head opcao of
        '1' -> do emptyScene "app/storage/cells.json"; selectOption False;
        -- '2' -> do menuAutomatasCarregarCena "app/storage/cells.json"; selectOption False;
        '3' -> do menuCells "app/storage/cells.json"; selectOption False
        '4' -> do tutorial; selectOption False;
        '5' -> do
            setCursorPosition 0 0
            printScreen "app/storage/mainMenuController/emptyMenu.txt" False True 
            exitSuccess
        _ -> do
            printScreen "app/storage/mainMenuInvalidOption.txt" True False
            selectOption False