module Test.Simulation where
    import Models.Cell
    import Models.Rule
    import Models.Grid
    import Data.Matrix
    import Test.HUnit 
    import qualified Control.Applicative as Aqui
    
{----------------------------------------------- [OBSERVAÇÕES] ---------------------------------------------------

╰> Todos os testes vão dentro da lista de testes
╰> Os testes são declarados na forma 
╰> "nomeTeste" ~: oQueVoceTaTestando ~?= resultadoEsperado
╰> Se o teste for muito complicado é melhor separar ele 
╰> Em outras funções lá em baixo 
╰> Rode os testes com stack runghc -- test/conwaysTest.hs

------------------------------------------------ [CONSTANTES] ----------------------------------------------------}

    conways :: Cell
    conways = Cell "C" (Rule [3] [2,3]) "Verde"

    highLife :: Cell
    highLife = Cell "H" (Rule [3, 6] [2,3]) "Vermelho"
    
    square :: Int -> Matrix (Maybe Cell)
    square n = gridGenerate n n

------------------------------------------------ [VARIÁVEIS] ------------------------------------------------------

    test3Setup :: Matrix (Maybe Cell)
    test3Setup = insertCells (insertCells (square 5) highLife [(2,2), (3,2), (4,2)]) conways [(2,4), (3,4), (4,4)] 

    test3Result :: Matrix (Maybe Cell)
    test3Result = insertCells (insertCells (square 5) highLife [(3,1), (3,2), (3,3)]) conways [(3,4), (3,5)]

    listaDeTestes :: Test
    listaDeTestes =
        TestList [
            testNumOfDeadNeighbors0, testNumOfDeadNeighbors1, testNumOfDeadNeighbors2, testNumOfDeadNeighbors3, 
            testGridGenerateFromList0, testGridGenerateFromList1, testGridGenerateFromList2, testGridGenerateFromList3
        ]

----------------------------------------- [MAIN: TESTES DO MÓDULO GRID] ------------------------------------------

    main :: IO Counts
    main = do
        runTestTT $ test [
            listaDeTestes,
            "Morra sozinho" ~:
                gridUpdate(insertCell (square 3) conways (1, 1)) ~?=
                     square 3,
            "Flicker com 2 amigos" ~:
                gridUpdate (insertCells (square 3) conways [(2,1), (2,2), (2,3)]) ~?= 
                    insertCells (square 3) conways [(1,2), (2,2), (3,2)],
            "Conways e o amigo alto" ~: gridUpdate test3Setup ~?= test3Result
                    ]

-------------------------------------- [CONTINUÇÃO: TESTES MODULARIZADOS] ---------------------------------------

{-- 
    The Grid from List: verifica se um grid gerado a partir de uma lista
    é equivalente a outro já definido
--}

    testGridGenerateFromList0 :: Test
    testGridGenerateFromList0 = TestCase $ do
        let resultGrid = gridGenerateFromList rows cols cellList
        assertEqual "Grid from List 0" expectedGrid resultGrid
        where
            rows = 2
            cols = 3
            cellList = [Just conways, Just conways, Just conways, Nothing, Nothing, Nothing]
            expectedGrid = 
                fromLists [
                    [Just conways, Just conways, Just conways], 
                    [Nothing, Nothing, Nothing]
                ]

    testGridGenerateFromList1 :: Test
    testGridGenerateFromList1 = TestCase $ do
        let resultGrid = gridGenerateFromList rows cols cellList
        assertEqual "Grid from List 1" expectedGrid resultGrid
        where
            rows = 2
            cols = 3
            cellList = 
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            expectedGrid = 
                fromLists [
                    [Nothing, Nothing, Nothing], 
                    [Nothing, Nothing, Nothing]
                ]

    testGridGenerateFromList2 :: Test
    testGridGenerateFromList2 = TestCase $ do
        let resultGrid = gridGenerateFromList rows cols cellList
        assertEqual "Grid from List 2" expectedGrid resultGrid
        where
            rows = 2
            cols = 3
            cellList = [Just conways, Just conways, Just conways, Just conways, Just conways, Just conways]
            expectedGrid = 
                fromLists [
                    [Just conways, Just conways, Just conways], 
                    [Just conways, Just conways, Just conways]
                ]

    testGridGenerateFromList3 :: Test
    testGridGenerateFromList3 = TestCase $ do
        let resultGrid = gridGenerateFromList rows cols cellList
        assertEqual "Grid from List 3" expectedGrid resultGrid
        where
            rows = 3
            cols = 2
            cellList = [Just conways, Nothing, Just conways, Nothing, Just conways, Nothing]
            expectedGrid = 
                fromLists [
                    [Just conways, Nothing], 
                    [Just conways, Nothing], 
                    [Just conways, Nothing]
                ]

-----------------------------------------------------------------------------------------------------------------

{--
    A Vizinhaça Zumbi: cria um grid 3x3 de células mortas e verifica se há 8 vizinhos
    para uma célula localizada no meio da matriz, no primeiro caso...
--}

    testNumOfDeadNeighbors0 :: Test
    testNumOfDeadNeighbors0 = TestCase $ do
        let grid = square 3
        let resultNum = numOfDeadNeighbors (x,y) grid
        assertEqual "Vizinhaça Zumbi 0" expectedGOLNum resultNum
        where
            x = 2
            y = 2
            expectedGOLNum = 8

    testNumOfDeadNeighbors1 :: Test
    testNumOfDeadNeighbors1 = TestCase $ do
        let grid = square 1
        let resultNum = numOfDeadNeighbors (x,y) grid
        assertEqual "Vizinhaça Zumbi 1" expectedGOLNum resultNum
        where
            x = 1
            y = 1
            expectedGOLNum = 8

    testNumOfDeadNeighbors2 :: Test
    testNumOfDeadNeighbors2 = TestCase $ do
        let grid = insertCells (square 3) conways [(1,2), (2,1), (2,3), (3,2)]
        let resultNum = numOfDeadNeighbors (x,y) grid
        assertEqual "Vizinhaça Zumbi 2" expectedGOLNum resultNum
        where
            x = 2
            y = 2
            expectedGOLNum = 4

    testNumOfDeadNeighbors3 :: Test
    testNumOfDeadNeighbors3 = TestCase $ do
        let preGrid = insertCells (square 3) highLife [(1,1), (1,3), (3,1), (3,3)]
        let grid = insertCells preGrid conways [(1,2), (2,1), (2,3), (3,2)]
        let resultNum = numOfDeadNeighbors (x,y) grid
        assertEqual "Vizinhaça Zumbi 3" expectedGOLNum resultNum
        where
            x = 2
            y = 2
            expectedGOLNum = 0

-----------------------------------------------------------------------------------------------------------------