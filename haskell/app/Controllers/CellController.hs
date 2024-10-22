module Controllers.CellController where
    import Files.Cell
    import Data.Aeson
    import Data.Char
    import Data.Maybe
    import qualified Data.Set as Set

    import Models.Cell ( Cell(Cell) )
    import Models.Rule (Rule (Rule))

    import System.Console.ANSI
    import Utils.Render

    import Control.Concurrent(threadDelay)
    import qualified Data.ByteString.Lazy as B

    import Text.Read

    -- Menu de Criação de Células
    -- 1) Direciona para a criação de Célula
    -- 2) Direciona para o menu de exclusão de Célula existente
    -- 3) Direciona para o menu principal
    menuCells :: FilePath -> IO()
    menuCells path = do

        cellsJSON <- readCells path
        
        let cells = case decode cellsJSON :: Maybe [Cell] of 
                Nothing -> do  
                        if B.length cellsJSON > 2 -- Testando se o arquivo tá vazio ou não. B.null não funciona pra isso, 
                            -- e esse teste só existe pra evitar sobrepor um arquivo parcialmente corrompido 
                            -- que o usuario poderia querer corrigir na mão
                            then 
                                error "ARQUIVO app/storage/cells.json MODIFICADO OU CORROMPIDO\nAPAGUE-O\nE TENTE EXECUTAR O PROGRAMA NOVAMENTE"
                            else
                                []
                Just c -> c
        listCells cells
        
        setCursorInput        
        option <- getLine

        case option of
            "1" -> do addAutomata path; menuCells path;
            "2" -> do removeAutomata path cells; menuCells path
            "3" -> return ()
            _ -> menuCells path


    -- Print formatado do menu de Celulas fazendo o tratamento caso 
    -- não existam Celulas criadas, e listando-as caso existam. 
    listCells :: [Cell] -> IO()
    listCells cells = 
        if null cells then printScreen "app/storage/ruleController/screenNoCells.txt" True False;
                    else do 
                        printScreen "app/storage/ruleController/listOfCells.txt" True False

                        printEmptyLines 2

                        printCells cells 1

                        printEmptyLines 1

                        printScreen "app/storage/ruleController/ruleMenuOptions.txt" False False

    -- Inicio do procedimento de criação da Celula, recebendo o nome da Celula e chamando a função
    -- para adicionar a regra de nascimento
    addAutomata :: FilePath ->  IO()
    addAutomata path = do

        printScreen "app/storage/ruleController/nameCellQuestion.txt" True False

        setCursorInput
        nameCellNT <- getLine
        let nameCellT = map toUpper nameCellNT


        addBirthRule path nameCellT
    
    -- Criação da Regra de Nascimento da Celula, o usuario pode inserir de 0 a 8 digitos entre 1 e 8.
    addBirthRule :: FilePath -> String -> IO()
    addBirthRule path nameCellT = do
        printScreen "app/storage/ruleController/birthRule.txt" True False
        
        setCursorInput
        nascStr <- getLine
        if handleBornRule nascStr then do
            let nascList = map (\x -> read [x] :: Int) (filter isDigit nascStr)
            addStayRule path nameCellT $ Set.toList (Set.fromList nascList)
        
        else do
            putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
            threadDelay 800000
            addBirthRule path nameCellT
    
    -- Criação da Regra de Permanencia da Celula, o usuario pode inserir de 0 a 8 digitos entre 1 e 8.
    addStayRule :: FilePath -> String -> [Int] -> IO()
    addStayRule path nameCellT nascList = do
        printScreen "app/storage/ruleController/stayRule.txt" True False
        
        setCursorInput
        stayStr <- getLine
        if handleStayRule stayStr then do
            let stayList = map (\x -> read [x] :: Int) (filter isDigit stayStr)
            let regra = Rule nascList $ Set.toList (Set.fromList stayList)
            addColor path nameCellT regra
        
        else do
            putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
            threadDelay 800000
            addStayRule path nameCellT nascList
    
    -- Criação da Cor da Celula, o usuario pode inserir uma das 22 opções de cor que o terminal disponibiliza.
    addColor :: FilePath -> String -> Rule -> IO()
    addColor path nameCellT regra = do
        printColors
        
        setCursorInput
        colorI <- getLine
        case readMaybe colorI :: Maybe Int of 
            Just color -> 
                if handleColorChoice color then do
                    let cell = Cell nameCellT regra $ fromJust $ selectColor color
                    addCell path cell
                else do
                    putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
                    threadDelay 800000
                    addColor path nameCellT regra
            Nothing -> do
                    putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
                    threadDelay 800000
                    addColor path nameCellT regra

    -- Print formatado das opções de cor
    printColors :: IO()
    printColors = do
        clearScreen
        cursorUpLine 30
        printMidScreen "QUAL A COR DA CÉLULA?"
        printColorsHelper 85
        printMidScreen "Cores dependem do tema do seu terminal\n"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull Red]
        putStr "1) VERMELHO     "
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "2) VERMELHO BRILHANTE"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull Green]
        putStr "3) VERDE        "
        setSGR [SetColor Foreground Vivid Green]
        putStrLn "4) VERDE BRILHANTE"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull Yellow]
        putStr "5) AMARELO      "
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn "6) AMARELO BRILHANTE"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull Blue]
        putStr "7) AZUL         "
        setSGR [SetColor Foreground Vivid Blue]
        putStrLn "8) AZUL BRILHANTE"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull Magenta]
        putStr "9) MAGENTA      "
        setSGR [SetColor Foreground Vivid Magenta]
        putStrLn "10) MAGENTA BRILHANTE"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull Cyan]
        putStr "11) CIANO       "
        setSGR [SetColor Foreground Vivid Cyan]
        putStrLn "12) CIANO BRILHANTE"
        printColorsHelper 85
        setSGR [SetColor Foreground Dull White]
        putStr "13) BRANCO      "
        setSGR [SetColor Foreground Vivid White]
        putStrLn "14) BRANCO BRILHANTE"
        printColorsHelper 85
        setSGR [Reset]
        printMidScreen "--- CORES INDEPENDENTES DO TEMA ---"

        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 214]
        putStrLn "15) DOURADO     "
        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 57]
        putStrLn "16) ROXO"
        printColorsHelper 95 
        setSGR [SetPaletteColor Foreground 50]
        putStrLn "17) VERDE ÁGUA  "
        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 217]
        putStrLn "18) SALMÃO    "
        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 202]
        putStrLn "19) LARANJA     "
        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 219]
        putStrLn "20) ROSINHA   "
        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 201]
        putStrLn "21) ROSA SHOCK  "
        printColorsHelper 95
        setSGR [SetPaletteColor Foreground 46]
        putStrLn "22) VERDE NEON"
        setSGR [Reset]
        printEmptyLines 20

    printColorsHelper  :: Int ->  IO ()
    printColorsHelper n = do
        setCursorColumn n
        threadDelay 130000

    -- Menu de exclusão de célula por nome
    removeAutomata :: FilePath -> [Cell] -> IO()
    removeAutomata path cells = do 
        
        if null cells 
            then
                return ()
            else do
            
            printScreen "app/storage/ruleController/listOfCells.txt" True False

            printEmptyLines 2

            printCells cells 1

            printEmptyLines 1

            printScreen "app/storage/ruleController/removeCellMenu.txt" False False

            setCursorInput
            nameCellI <- getLine
            let nameCellT = map toUpper nameCellI 
            deleteCell path nameCellT
    -- Print formatado das células criadas
    printCells :: [Cell] -> Int -> IO()
    printCells [] _ = return ()
    printCells (x:xs) n = do
        setCursorColumn 90
        putStrLn $ show n ++ " - " ++ show x
        printCells xs (n + 1)
    
    -- Tratamento da escolha das cores
    selectColor :: Int -> Maybe String
    selectColor color = case color of
        1 -> Just "VERMELHO"
        2 -> Just "VERMELHO  BRILHANTE"
        3 -> Just "VERDE"
        4 -> Just "VERDE BRILHANTE"
        5 -> Just "AMARELO"
        6 -> Just "AMARELO BRILHANTE"
        7 -> Just "AZUL"
        8 -> Just "AZUL BRILHANTE"
        9 -> Just "MAGENTA"
        10 -> Just "MAGENTA BRILHANTE"
        11 -> Just "CIANO"
        12 -> Just "CIANO BRILHANTE"
        13 -> Just "BRANCO"
        14 -> Just "BRANCO BRILHANTE"
        15 -> Just "DOURADO" 
        16 -> Just "ROXO"
        17 -> Just "VERDE ÁGUA"
        18 -> Just "SALMÃO"
        19 -> Just "LARANJA"
        20 -> Just "ROSINHA"
        21 -> Just "ROSA SHOCK"
        22 -> Just "VERDE NEON"
        _ -> Nothing

    -- Faz o tratemento das entradas das regras de Nascimento e Permanência, removendo
    -- espaços em branco e verificando se há de 0 a 8 dígitos entre 1 e 8.
    handleStayRule :: String -> Bool
    handleStayRule regra = let regraFormatada = filter (not . isSpace) regra
                                  in length regraFormatada <= 8 && all (`elem` "012345678") regraFormatada

    handleBornRule :: String -> Bool
    handleBornRule regra = let regraFormatada = filter (not . isSpace) regra
                                  in length regraFormatada <= 8 && all (`elem` "12345678") regraFormatada

    -- Faz o tratamento da entrada de cor, verifica se a entrada é um número entre 1 e 22
    handleColorChoice :: Int -> Bool
    handleColorChoice cor = cor > 0 && cor < 23