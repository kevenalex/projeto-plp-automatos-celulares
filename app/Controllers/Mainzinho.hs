module Mainzinho where

    main :: IO()
    main = do 
        x <- getLine
        chooseOption x

    chooseOption :: String -> IO()
    chooseOption choose
        | choose == "1" = print "playGame"
        | choose == "2" = print "CreateRule"
        | choose == "3" = print "CreateGrid"
        | choose == "q" = print "xau"
        | otherwise = 
            do 
                _ <- system "clear"
                print "tente de novo"
                main

module Controllers.GridController where

    import Models.Cell
    import Models.Grid
    import Data.Matrix
    import System.Process (system)

    chooseGrid :: Matrix [Maybe Cell]
    chooseGrid = 
        do
            _ <- print "qual a grid que mofi quer ?"
            __ <- print "se nao escolher nenhuma vai ser uma vazia, se liga hein maluco"
            findGrid
    
    playGame :: IO()
    playGame = do
        grid <- chooseGrid
        _ <- system "clear"
        simulate grid
    
    simulate :: Matrix [Maybe Cell] -> IO()
    simulate grid = do 
        _ <- system "clear"
        printGrid grid
        print "digite n para carregar o proximo passo"
        print "digite i para inserir alguma celula"
        nextStep <- getLine
        simulationDecision nextStep grid
    
    simulationDecision :: String -> Matrix [Maybe Cell] -> IO()
    simulationDecision decision grid
        | decision == "n" = simulate (gridUpdate grid)
        | decision == "i" = simulate insertion grid
        | otherwise = print "gay"

    insertion :: Matrix [Maybe Cell] -> Matrix [Maybe Cell]
    insertion grid = 
        do 
            print "qual a coordenada x ?"
            x <- read getLine
            print "qual a coordenada y ?"
            y <- read getLine
            print "qual celula desejas adicionar ?"
            cell <- getCellFromLeo
            insertGrid (x,y) cell
            