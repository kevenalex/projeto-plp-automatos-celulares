module Utils.Render where

    import Control.Concurrent(threadDelay)
    import System.Console.ANSI

    --- Imprime um arquivo de texto na tela. Tendo como parâmetros a String do caminho do arquivo, e dois booleanos, clear e delay
    --- o qual decidem se a tela será limpada antes da impressão e se o texto será impresso com rapidamente ou não.
    printScreen :: FilePath -> Bool -> Bool -> IO ()
    printScreen file clear delay = do

        lines <- textFileToLines file

        if clear then do clearScreen; printScreenWithDelay lines delay;
        else printScreenWithDelay lines delay

    -- Converte um arquivo de texto em uma lista de Strings do tipo IO       
    textFileToLines :: FilePath -> IO [String]
    textFileToLines file = do
        screen <- readFile file
        return $ lines screen

    -- Imprime na tela uma sequência de strings, com o parâmetro delay do tipo boolean decidindo se esta impressão ocorrerá rapidamente ou não.
    printScreenWithDelay :: [String] -> Bool -> IO()
    printScreenWithDelay lines delay = do
        if delay then mapM_ (printTextSpeed 130000) lines
        else mapM_ (printTextSpeed 10000) lines

    -- Imprime uma string e determina um delay após a sua impressão.
    printTextSpeed :: Int -> String -> IO ()
    printTextSpeed speed str = do
        putStrLn str
        threadDelay speed

    -- Imprime N linhas vazias na tela.
    printEmptyLines :: Int -> IO ()
    printEmptyLines 0 = return ()
    printEmptyLines n = do putStrLn ""; printEmptyLines (n-1) 

    trim :: String -> String
    trim = unwords . words

