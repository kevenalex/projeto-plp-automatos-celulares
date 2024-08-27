module Controllers.SceneController where

    import Files.Scene
    import Data.Aeson
    import Models.Rule
    import qualified Data.Map as M
    import Data.Char


    -- menuScenes :: FilePath -> IO()
    -- menuScenes path = do
    --     scenesJSON <- readScenes path
    --     case decode scenesJSON :: Maybe (M.Map String Scene) of
    --         Nothing -> putStrLn "vsf não tem cenas"
    --         Just scenesDict -> print "Cenas existentes:\n" ++  map (\x -> "- " ++ title x ++ '\n') scenesDict -- Feio né  

    --     putStrLn " - (V)isualizar cena\n - (D)eletar \n - (R)etornar"

    --     opcao <- getLine
    --     case toUpperper opcao of
    --         "V" -> do scenesVisu path; menuScenes path  
    --         "D" -> do menuRemove path; menuScenes path
    --         "R" -> putStrLn ""
    --         _ -> menuScenes 

    -- printScenes :: M.Map String Scene -> Matrix []