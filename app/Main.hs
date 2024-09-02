module Main where

import Controllers.CellController (menuCells)
import Controllers.SimulationController
import Controllers.SceneController (menuScenes)
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

    setCursorInput
    selectOption False -- Laço principal

selectOption :: Bool -> IO ()
selectOption delay = do
    printScreen "app/storage/mainMenuController/mainMenu.txt" True delay


    setCursorInput
    opcao <- getLine

    if null opcao 
        then  do
            setCursorColumn 97
            putStrLn "OPCÃO INVÁLIDA, TENTE NOVAMENTE"
            threadDelay 2000000
            selectOption False
        else
            case head opcao of
                '1' -> do emptyScene "app/storage/cells.json"; selectOption False;
                '2' -> do menuScenes "app/storage/scenes.json"; selectOption False;
                '3' -> do menuCells "app/storage/cells.json"; selectOption False
                '4' -> do tutorial; selectOption False;
                '5' -> do
                    setCursorPosition 0 0
                    printScreen "app/storage/mainMenuController/emptyMenu.txt" False True 
                    exitSuccess
                _ -> do
                    setCursorColumn 97
                    putStrLn "OPCÃO INVÁLIDA, TENTE NOVAMENTE"
                    threadDelay 2000000

                    selectOption False