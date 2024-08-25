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
    main = do
        runTestTT $ test [
            "Morra sozinho" ~:
                gridUpdate(insertCell square conways (1, 1)) ~?=
                     square,
            "Flicker com 2 amigos" ~:
                gridUpdate (insertCells square conways [(2,1), (2,2), (2,3)]) ~?=
                    insertCells square conways [(1,2), (2,2), (3,2)]
                     ]


--- Constantes ---
    conways :: Cell
    conways = Cell "C" (Rule [3] [2,3]) "C"

    highLife :: Cell
    highLife = Cell "H" (Rule [3, 6] [2,3]) "H"
    
    square :: Matrix (Maybe Cell)
    square = gridGenerate 3 3