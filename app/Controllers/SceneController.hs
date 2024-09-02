module Controllers.SceneController where
    import Files.Scene
    import Utils.Render

    import Data.Aeson
    import qualified Data.Map as M
    
    import System.Console.ANSI
    
    import Control.Concurrent(threadDelay)
    import Controllers.SimulationController (prepareSimulate)
    
    -- Menu Inicial de Cenas que faz o tratamento caso não existam Cenas
    -- criadas e caso existam, permite três opções:
    -- 1) Simular Cena, em seguida a Cena deve ser escolhida por nome
    -- 2) Deletar Cena, em seguida a Cena deve ser escolhida por nome
    -- 3) Retornar ao menu principal
    menuScenes :: FilePath -> IO()
    menuScenes path = do
        
        scenesJSON <- readScenes path
        case decode scenesJSON :: Maybe (M.Map String Scene) of
            Nothing -> do
                printScreen "app/storage/sceneController/screenNoScenes.txt" True False;
                setCursorInput
                opcao <- getLine
                case opcao of
                    "3" -> return ()
                    _ -> menuScenes path

            Just scenesDict -> do
                listScenes scenesDict

                setCursorInput
                opcao <- getLine
                case opcao of
                    "1" -> do simulateScenesChoice path scenesDict; menuScenes path
                    "2" -> do deleteScenesChoice path scenesDict; menuScenes path
                    "3" -> return ()
                    _ -> menuScenes path

    -- Extrai os nomes das Cenas salvas no arquivo json
    extractSceneNames :: M.Map String Scene -> [String]
    extractSceneNames scenesDict = map title (M.elems scenesDict)

    -- Print formatado do menu de Cenas fazendo o tratamento caso 
    -- não existam Cenas criadas, e listando-as caso existam. 
    listScenes :: M.Map String Scene -> IO()
    listScenes scenes =
        if null scenes then printScreen "app/storage/sceneController/screenNoScenes.txt" True False;
                    else do
                        printScreen "app/storage/sceneController/listOfScenes.txt" True False
                        printEmptyLines 2
                        printScenes $ extractSceneNames scenes
                        printEmptyLines 1
                        printScreen "app/storage/sceneController/sceneMenuOptions.txt" False False

    -- Print formatado da listagem de Cenas por nome
    printScenes :: [String] -> IO()
    printScenes [] = return ()
    printScenes (x:xs) = do
        setCursorColumn 92
        print x
        printScenes xs

    -- Print da listagem de cenas existentes + opção de retorno para organizar
    -- os menus e evitar repetição de código desnecessária em simulateScenesChoice
    -- e deleteScenesChoice
    formatedOutput :: M.Map String Scene -> IO ()
    formatedOutput scenes = do
        printEmptyLines 2
        printScenes $ extractSceneNames scenes 
        printScreen "app/storage/sceneController/sceneReturnOption.txt" False False
        printEmptyLines 1
        setCursorColumn 85

    -- Menu de escolha de simulação de Cena, retorna a simulação com a Cena escolhida
    simulateScenesChoice :: FilePath -> M.Map String Scene -> IO ()
    simulateScenesChoice path scenes = do
        printScreen "app/storage/sceneController/sceneChoiceQuestion.txt" True False

        formatedOutput scenes

        setCursorInput
        choice <- getLine
        if M.member choice scenes then do
            case M.lookup choice scenes of 
                Just scene -> do
                    let grid = sceneToGrid scene
                    prepareSimulate grid "app/storage/cells.json"
                    
                Nothing -> do
                    putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
                    threadDelay 800000
                    simulateScenesChoice path scenes
            
        else if choice == "3"
            then return ()
            else do
                putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
                threadDelay 800000
                simulateScenesChoice path scenes

    -- Menu de exclusão de Cena, retorna o menu de escolha de Cenas sem a Cena deletada
    deleteScenesChoice :: FilePath -> M.Map String Scene -> IO ()
    deleteScenesChoice path scenes = do
        printScreen "app/storage/sceneController/sceneDeleteQuestion.txt" True False
        
        formatedOutput scenes
        
        setCursorInput
        choice <- getLine
        if M.member choice scenes then do
            deleteScene path choice
        else if choice == "3"
            then return ()
            else do
                putStrLn "INPUT ERRADO, TENTE NOVAMENTE"
                threadDelay 800000
                deleteScenesChoice path scenes
