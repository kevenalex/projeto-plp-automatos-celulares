module Test.Simulation where
    import Models.Cell
    import Models.Rule
    import Models.Grid
    import Data.Matrix
    import Test.HUnit 
    
-------------------------
-- todos os testes vão dentro da lista de testes
-- os testes são declarados na forma 
-- "nomeTeste" ~: oQueVoceTaTestando ~?= resultadoEsperado
-- se o teste for muito complicado é melhor separar ele 
-- em outras funções lá em baixo 
-------------------------

    -- rode esses testes com stack runghc -- test/conwaysTest.hs
    main :: IO Counts
    main = do
        runTestTT $ test [
            "Morra sozinho" ~:
                gridUpdate(insertCell (square 3) conways (1, 1)) ~?=
                     square 3,
            "Flicker com 2 amigos" ~:
                gridUpdate (insertCells (square 3) conways [(2,1), (2,2), (2,3)]) ~?= 
                    insertCells (square 3) conways [(1,2), (2,2), (3,2)],
            "Conways e o amigo alto" ~: gridUpdate test3Setup ~?= test3Result
                    ]

--- testes ---
    test3Setup :: Matrix (Maybe Cell)
    test3Setup = insertCells (insertCells (square 5) highLife [(2,2), (3,2), (4,2)])conways [(2,4), (3,4), (4,4)] 
    test3Result :: Matrix (Maybe Cell)
    test3Result = insertCells(insertCells (square 5) highLife [(3,1), (3,2), (3,3)]) conways [(3,4), (3,5)]

--- Constantes ---
    conways :: Cell
    conways = Cell "C" (Rule [3] [2,3]) "Verde"

    highLife :: Cell
    highLife = Cell "H" (Rule [3, 6] [2,3]) "Vermelho"
    
    square :: Int -> Matrix (Maybe Cell)
    square n = gridGenerate n n
