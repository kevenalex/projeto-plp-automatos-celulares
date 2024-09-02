module Controllers.SceneController where
-- {"eu amo keven":{"cols":10,"flatMatrix":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"rows":10,"title":"eu amo keven"},"leo eh foda":{"cols":10,"flatMatrix":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"rows":10,"title":"leo eh foda"},"oi":{"cols":10,"flatMatrix":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"rows":10,"title":"oi"},"oi levo":{"cols":10,"flatMatrix":[null,null,{"color":"Verde","name":"C","rule":{"birth":[3],"stay":[2,3]}},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"color":"Verde","name":"C","rule":{"birth":[3],"stay":[2,3]}},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"rows":10,"title":"oi levo"}}
    import Files.Scene
    import Data.Aeson
    import Models.Rule (Rule (Rule))
    import Utils.Render
    import qualified Data.Map as M
    import Data.Char
    import Control.Concurrent(threadDelay)
    import System.Console.ANSI
    import Controllers.SimulationController (prepareSimulate)
    import Data.Maybe
    import Models.Grid
    
    -- se não tiver nenhuma cena ele só printa o 3 e volta
    -- 

    menuScenes :: FilePath -> IO()
    menuScenes path = do
        
        scenesJSON <- readScenes path
        case decode scenesJSON :: Maybe (M.Map String Scene) of
            Nothing -> do
                printScreen "app/storage/sceneController/screenNoScenes.txt" True False;
                opcao <- getLine
                case opcao of
                    "3" -> return ()
                    _ -> menuScenes path

            Just scenesDict -> do
                listScenes scenesDict

                setCursorColumn 85

                opcao <- getLine
                case opcao of
                    "1" -> do simulateScenesChoice path scenesDict; menuScenes path
                    "2" -> do deleteScenesChoice path scenesDict; menuScenes path
                    "3" -> return ()
                    _ -> menuScenes path

    -- literalmente mesma logica de listCells porem com Scenes
    listScenes :: M.Map String Scene -> IO()
    listScenes scenes =
        if null scenes then printScreen "app/storage/sceneController/screenNoScenes.txt" True False;
                    else do
                        printScreen "app/storage/sceneController/listOfScenes.txt" True False
                        printEmptyLines 2
                        printScenes $ extractSceneNames scenes
                        printEmptyLines 1
                        printScreen "app/storage/sceneController/sceneMenuOptions.txt" False False

    -- literalmente mesma logica de printCells porem com Scenes
    printScenes :: [String] -> IO()
    printScenes [] = return ()
    printScenes (x:xs) = do
        setCursorColumn 92
        print x
        printScenes xs

    -- Extrai os nomes das Cenas salvas
    extractSceneNames :: M.Map String Scene -> [String]
    extractSceneNames scenesDict = map title (M.elems scenesDict)

    simulateScenesChoice :: FilePath -> M.Map String Scene -> IO ()
    simulateScenesChoice path scenes = do
        printScreen "app/storage/sceneController/sceneChoiceQuestion.txt" True False

        formatedOutput scenes

        choice <- getLine
        if M.member choice scenes then do
            let row = extractSceneRow scenes choice
            let cols = extractSceneCol scenes choice
            prepareSimulate (gridGenerate row cols) "app/storage/cells.json"
            
        else do
            putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
            threadDelay 800000
            simulateScenesChoice path scenes

    deleteScenesChoice :: FilePath -> M.Map String Scene -> IO ()
    deleteScenesChoice path scenes = do
        printScreen "app/storage/sceneController/sceneDeleteQuestion.txt" True False

        formatedOutput scenes

        choice <- getLine
        if M.member choice scenes then do
            deleteScene path choice
            
        else do
            putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
            threadDelay 800000
            deleteScenesChoice path scenes

    -- Print de listagem de cenas + listas vazias para organizar os prints
    formatedOutput :: M.Map String Scene -> IO ()
    formatedOutput scenes = do
        printEmptyLines 2
        printScenes $ extractSceneNames scenes 
        printScreen "app/storage/sceneController/sceneEmptyLines.txt" False False
        setCursorColumn 85

    -- Extrai o numero de linhas da Cena, como o gridGenerate não aceita Maybe Int, tem que ter um default
    extractSceneRow :: M.Map String Scene -> String -> Int
    extractSceneRow scene choice =
            rows $ fromMaybe defaultScene (M.lookup choice scene)
        where
            defaultScene = Scene { title = choice, rows = 0, cols = 0}

    -- Extrai o numero de colunas da Cena, como o gridGenerate não aceita Maybe Int, tem que ter um default
    extractSceneCol :: M.Map String Scene -> String -> Int
    extractSceneCol scene choice =
            cols $ fromMaybe defaultScene (M.lookup choice scene)
        where
            defaultScene = Scene { title = choice, rows = 0, cols = 0}