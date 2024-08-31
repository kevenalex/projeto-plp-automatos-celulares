module Utils.Render where

    import Control.Concurrent(threadDelay)
    import System.Console.ANSI

    -- Imprime a logo
    printLogoMoreDelay :: IO ()
    printLogoMoreDelay = do 

        logo <- readFile "app/storage/mainMenuController/mainMenu.txt"
        
        let linhas = lines logo
        
        mapM_ printMoreDelay linhas

    printLogoLessDelay :: IO ()
    printLogoLessDelay = do

        logo <- readFile "app/storage/mainMenuController/mainMenu.txt"

        let linhas = lines logo
        
        mapM_ printLessDelay linhas

    printRuleMenuColorError :: IO()
    printRuleMenuColorError = do

        menuColor <- readFile "app/storage/ruleMenuColorError.txt"

        let linhas = lines menuColor
        
        mapM_ printLessDelay linhas

    printMainMenuInvalidOption :: IO ()
    printMainMenuInvalidOption = do

        logo <- readFile "app/storage/mainMenuController/mainMenuInvalidOption.txt"

        let linhas = lines logo
        
        mapM_ printLessDelay linhas

    printMoreDelay :: String -> IO ()
    printMoreDelay str = do
        putStrLn str
        threadDelay 130000

    printLessDelay :: String -> IO ()
    printLessDelay str = do
        putStrLn str
        threadDelay 10000

    printEmptyLines :: Int -> IO ()
    printEmptyLines 0 = return ()
    printEmptyLines n = do putStrLn ""; printEmptyLines (n-1) 

    printTextWithDelayNoClear :: FilePath -> IO ()
    printTextWithDelayNoClear file = do
        screen <- readFile file
        let linhas = lines screen
        mapM_ printMoreDelay linhas 

    printTextFileWithClear :: FilePath -> IO()
    printTextFileWithClear file = do
        clearScreen
        screen <- readFile file
        let linhas = lines screen
        mapM_ printLessDelay linhas 

    printTextFileNoClear :: FilePath -> IO()
    printTextFileNoClear file = do
        screen <- readFile file
        let linhas = lines screen
        mapM_ printLessDelay linhas 
    -- tira esoaços do começo e final de uma string
    trim :: String -> String
    trim = unwords . words

