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
            testGridGenerateFromList0, testGridGenerateFromList1, testGridGenerateFromList2, testGridGenerateFromList3,
            testNumOfLiveNeighbors0, testNumOfLiveNeighbors1, testNumOfLiveNeighbors2, testNumOfLiveNeighbors3,
            testNumOfLiveNeighbors4, testGetCellTrue, testGetCellFalse
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
        assertEqual "Grid from List 0" expectedGrid resultGrid
        where
            resultGrid = gridGenerateFromList 2 3 cellList
            cellList = [Just conways, Just conways, Just conways, Nothing, Nothing, Nothing]
            expectedGrid = 
                fromLists [
                    [Just conways, Just conways, Just conways], 
                    [Nothing, Nothing, Nothing]
                ]

    testGridGenerateFromList1 :: Test
    testGridGenerateFromList1 = TestCase $ do
        assertEqual "Grid from List 1" expectedGrid resultGrid
        where
            resultGrid = gridGenerateFromList 2 3 cellList
            cellList = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            expectedGrid = 
                fromLists [
                    [Nothing, Nothing, Nothing], 
                    [Nothing, Nothing, Nothing]
                ]

    testGridGenerateFromList2 :: Test
    testGridGenerateFromList2 = TestCase $ do
        assertEqual "Grid from List 2" expectedGrid resultGrid
        where
            resultGrid = gridGenerateFromList 2 3 cellList
            cellList = [Just conways, Just conways, Just conways, Just conways, Just conways, Just conways]
            expectedGrid = 
                fromLists [
                    [Just conways, Just conways, Just conways], 
                    [Just conways, Just conways, Just conways]
                ]

    testGridGenerateFromList3 :: Test
    testGridGenerateFromList3 = TestCase $ do
        assertEqual "Grid from List 3" expectedGrid resultGrid
        where
            resultGrid = gridGenerateFromList 3 2 cellList
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
        assertEqual "Vizinhaça Zumbi 0" expectedNum resultNum
        where
            expectedNum = 8
            resultNum = numOfDeadNeighbors (2,2) (square 3)

    testNumOfDeadNeighbors1 :: Test
    testNumOfDeadNeighbors1 = TestCase $ do
        assertEqual "Vizinhaça Zumbi 1" expectedNum resultNum
        where
            expectedNum = 8
            resultNum = numOfDeadNeighbors (1,1) (square 1)

    testNumOfDeadNeighbors2 :: Test
    testNumOfDeadNeighbors2 = TestCase $ do
        assertEqual "Vizinhaça Zumbi 2" expectedNum resultNum
        where
            expectedNum = 4
            grid = insertCells (square 3) conways [(1,2), (2,1), (2,3), (3,2)]
            resultNum = numOfDeadNeighbors (2,2) grid

    testNumOfDeadNeighbors3 :: Test
    testNumOfDeadNeighbors3 = TestCase $ do
        assertEqual "Vizinhaça Zumbi 3" expectedNum resultNum
        where
            expectedNum = 0
            preGrid = insertCells (square 3) highLife [(1,1), (1,3), (3,1), (3,3)]
            grid = insertCells preGrid conways [(1,2), (2,1), (2,3), (3,2)]
            resultNum = numOfDeadNeighbors (2,2) grid

-----------------------------------------------------------------------------------------------------------------

{--
    Conta Vizinhos: verificamos se a função retorna a quantidade de células 
    existentes na vizinhança de uma certa célula da grade
--}

    testNumOfLiveNeighbors0 :: Test
    testNumOfLiveNeighbors0 = TestCase $ do
        assertEqual "Conta Vizinhos 0" expectedNum resultNum
        where
            expectedNum = 8
            grid = insertCells (square 3) conways [(1,1), (1,2),(1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]
            resultNum = numOfLiveNeighbors (2,2) grid

    testNumOfLiveNeighbors1 :: Test
    testNumOfLiveNeighbors1 = TestCase $ do
        assertEqual "Conta Vizinhos 1" expectedNum resultNum
        where
            expectedNum = 0
            resultNum = numOfLiveNeighbors (2,2) (square 3)

    testNumOfLiveNeighbors2 :: Test
    testNumOfLiveNeighbors2 = TestCase $ do
        assertEqual "Conta Vizinhos 2" expectedNum resultNum
        where
            expectedNum = 4
            grid = insertCells (square 3) conways [(1,1), (1,3), (3,1), (3,3)]
            resultNum = numOfLiveNeighbors (2,2) grid

    testNumOfLiveNeighbors3 :: Test
    testNumOfLiveNeighbors3 = TestCase $ do
        assertEqual "Conta Vizinhos 3" expectedNum resultNum
        where
            expectedNum = 4
            grid = insertCells (square 3) conways [(1,2), (2,1), (2,3), (3,2)]
            resultNum = numOfLiveNeighbors (2,2) grid

    testNumOfLiveNeighbors4 :: Test
    testNumOfLiveNeighbors4 = TestCase $ do
        assertEqual "Conta Vizinhos 4" expectedNum resultNum
        where
            expectedNum = 0
            resultNum = numOfLiveNeighbors (1,1) (square 1)

-----------------------------------------------------------------------------------------------------------------

{--
    Simplismente Get Cell: aqui, verificamos os seus dois casos
--}

    testGetCellTrue :: Test
    testGetCellTrue = TestCase $ do
        assertEqual "getCellTrue" result expected
            where
                expected = Just conways
                grid = insertCell (square 1) conways (1,1)
                result = getCell (1,1) grid
    
    testGetCellFalse :: Test
    testGetCellFalse = TestCase $ do 
        assertEqual "getCellFalse" result expected
            where
                expected = Nothing
                result = getCell (1,1) (square 1)

-----------------------------------------------------------------------------------------------------------------