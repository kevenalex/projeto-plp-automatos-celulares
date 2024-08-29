module Utils.Logo where

    import Control.Concurrent(threadDelay)

    -- Imprime a logo
    printLogoMoreDelay :: IO ()
    printLogoMoreDelay = do 

        logo <- readFile "app/storage/logo.txt"
        
        let linhas = lines logo
        
        mapM_ printMoreDelay linhas

    printLogoLessDelay :: IO ()
    printLogoLessDelay = do

        logo <- readFile "app/storage/logo.txt"

        let linhas = lines logo
        
        mapM_ printLessDelay linhas

    printMainMenuInvalidOption :: IO ()
    printMainMenuInvalidOption = do

        logo <- readFile "app/storage/mainMenuInvalidOption.txt"

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