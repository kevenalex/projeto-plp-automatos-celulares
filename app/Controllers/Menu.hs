module Controllers.Menu where

    import Brick

    tui :: IO()
    tui = do
        initialState <- buildInitialState
        endState <- defaultMain tuiApp initialState
        print endState

    data TuiState = 
        TuiState 
        { tuiStatePaths :: [FilePath]}
        deriving (Show, Eq)
    
    type Resource = String

    tuiApp :: App TuiState e Resource
    tuiApp = 
        App {
            appDraw = drawTui,
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleTuiEvent,
            appStartEvent = pure,
            appAttrMap = const $ attrMap mempty []
        }
    
    buildInitialState :: IO TuiState
    buildInitialState = do 
        here <-  
        pure TuiState

    drawTui :: TuiState -> [Int]
    drawTui _ts = [3]

    handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
    handleTuiEvent s e = 
        case e of 
            VtyEvent vty -> 
                case vtye of
                    EvKey (KChar 'q') [] -> halt stay
                    _ -> continue s
                _ -> continue s