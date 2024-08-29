module Main where

  import Utils.Render
  import System.Console.ANSI
  import Controllers.CellController(menuCells)
  import Data.Char
  import System.Exit (exitSuccess)
  
  main :: IO()
  main = do

      clearScreen

      Just (_, screenHeight) <- getTerminalSize

      setCursorPosition (screenHeight - 1) 0

      printLogoMoreDelay

      selectFirstOption

      selectOption False

  selectOption :: Bool -> IO ()
  selectOption error = do
    
    clearScreen
    
    if error then printMainMenuInvalidOption
    else printLogoLessDelay

    opcao <- getLine

    case toUpper $ head opcao of
      -- '1' -> do menuAutomatasSandBox "app/storage/cells.json"; selectOption False;
      -- '2' -> do menuAutomatasCarregarCena "app/storage/cells.json"; selectOption False;
      '3' -> do menuCells "app/storage/cells.json"; selectOption False;
      -- '4' -> do menuTutorial; selectOption False;
      '5' -> do
        setCursorPosition 0 0
        printTextWithDelayNoClear "app/storage/mainMenuController/emptyMenu.txt"
        exitSuccess
      _ -> do
        selectOption True

  selectFirstOption :: IO ()
  selectFirstOption = do
    
    opcao <- getLine

    case toUpper $ head opcao of
      -- '1' -> menuAutomatasSandBox "app/storage/cells.json"
      -- '2' -> menuAutomatasCarregarCena "app/storage/cells.json"
      '3' -> menuCells "app/storage/cells.json"
      -- '4' -> menuTutorial
      '5' -> do 
        setCursorPosition 0 0
        printTextWithDelayNoClear "app/storage/mainMenuController/emptyMenu.txt"
        exitSuccess
      _ -> do
        clearScreen
        printMainMenuInvalidOption
        selectFirstOption