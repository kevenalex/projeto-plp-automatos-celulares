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

    -- se não tiver nenhuma cena ele só printa o 3 e volta
    -- 

    menuScenes :: FilePath -> IO()
    menuScenes path = do
        -- let cena1 = Scene "Nome da Cena" 10 10 [Nothing]
        scenesJSON <- readScenes path
        case decode scenesJSON :: Maybe (M.Map String Scene) of
            Nothing -> do
                printScreen "app/storage/sceneController/screenNoScenes.txt" True False;
                opcao <- getLine
                case opcao of
                    "3" -> return ()
                    _ -> menuScenes path
-- consertar esta porra feia repetida pra nao dar erro em scenesDict

            Just scenesDict -> do
                listScenes scenesDict

                setCursorColumn 85
                opcao <- getLine
                -- to confuso: pq isso nao da errose o arquivo de cenas tiver vazio?\
                case opcao of
                    "1" -> do scenesChoice path scenesDict; menuScenes path
                    "2" -> do menuRemove path; menuScenes path
                    "3" -> return ()
                    _ -> menuScenes path

    -- literalmente mesma logica de listCells porem com Scenes
    listScenes :: M.Map String Scene -> IO()
    listScenes scenes =
        if null scenes then printScreen "app/storage/sceneController/screenNoScenes.txt" True False;
                    else do
                        printScreen "app/storage/sceneController/listOfScenes.txt" True False
                        printEmptyLines 2
                        printScenes (extractSceneNames scenes) 1
                        printEmptyLines 1
                        printScreen "app/storage/sceneController/sceneMenuOptions.txt" False False

    -- literalmente mesma logica de printCells porem com Scenes
    printScenes :: [String] -> Int -> IO()
    printScenes [] _ = return ()
    printScenes (x:xs) n = do
        setCursorColumn 90
        putStrLn $ show n ++ " - " ++ show x
        printScenes xs (n + 1)

    -- em Java era só dar um getName fuck haskell
    extractSceneNames :: M.Map String Scene -> [String]
    extractSceneNames scenesDict = map title (M.elems scenesDict)

    scenesChoice :: FilePath -> M.Map String Scene -> IO ()
    scenesChoice path scenes = do
        printScreen "app/storage/sceneController/listOfScenes.txt" True False
        printEmptyLines 2
        printScenes (extractSceneNames scenes) 1
        setCursorColumn 85
        choice <- readLn :: IO Int
        if handleSceneChoice choice $ selectIdScenes scenes then do
            viewScene choice

        else
            scenesChoice path scenes

    menuRemove :: FilePath -> IO ()
    menuRemove = undefined

    viewScene :: Int -> IO()
    viewScene = undefined

    selectIdScenes :: M.Map String Scene -> Int
    selectIdScenes = M.size

    handleSceneChoice :: Int -> Int -> Bool
    handleSceneChoice choice size = choice > 0 && choice < size
