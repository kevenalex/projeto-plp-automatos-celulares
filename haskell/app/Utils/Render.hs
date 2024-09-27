module Utils.Render where

    import Control.Concurrent(threadDelay)
    import System.Console.ANSI
    import System.IO
    import qualified Data.Text as T
    import qualified Data.Text.IO as TIO

    
    --- Imprime um arquivo de texto na tela. Tendo como parâmetros a String do caminho do arquivo, e dois booleanos, clear e delay
    --- o qual decidem se a tela será limpada antes da impressão e se o texto será impresso com rapidamente ou não.
    printScreen :: FilePath -> Bool -> Bool -> IO ()
    printScreen file clear delay = do

        lines <- textFileToLines file

        if clear then do clearScreen; printScreenWithDelay lines delay;
        else printScreenWithDelay lines delay

    -- Converte um arquivo de texto em uma lista de Strings do tipo IO       
    textFileToLines :: FilePath -> IO [T.Text]
    textFileToLines file = do
        screen <- readFileUtf8 file
        return $ T.lines screen

    readFileUtf8 :: FilePath -> IO T.Text
    readFileUtf8 filePath = do
        handle <- openFile filePath ReadMode  
        hSetEncoding handle utf8              
        content <- TIO.hGetContents handle    
        hClose handle                         
        return content

    -- Imprime na tela uma sequência de strings, com o parâmetro delay do tipo boolean decidindo se esta impressão ocorrerá rapidamente ou não.
    printScreenWithDelay :: [T.Text] -> Bool -> IO()
    printScreenWithDelay lines delay = do
        if delay then mapM_ (printTextSpeed 130000) lines
        else mapM_ (printTextSpeed 10000) lines

    -- Imprime uma string e determina um delay após a sua impressão.
    printTextSpeed :: Int -> T.Text -> IO ()
    printTextSpeed speed str = do
        TIO.hPutStrLn stdout str
        threadDelay speed

    -- Imprime N linhas vazias na tela.
    printEmptyLines :: Int -> IO ()
    printEmptyLines 0 = return ()
    printEmptyLines n = do putStrLn ""; printEmptyLines (n-1) 

    printEmptyLinesWithDelay :: Int -> IO ()
    printEmptyLinesWithDelay 0 = return ()
    printEmptyLinesWithDelay n = do 
        putStr "\n"
        threadDelay 330000
        printEmptyLines (n-1) 

    trim :: String -> String
    trim = unwords . words

    printMidScreen :: String -> IO ()
    printMidScreen str = do setCursorColumn (100 - halfLengthStr); putStrLn str;
        where
            halfLengthStr = (length str) `div` 2

    setCursorInput :: IO ()
    setCursorInput = setCursorColumn 85