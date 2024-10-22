module Test.Simulation where
    import Models.Cell
    import Models.Rule
    import Models.Grid
    import Data.Matrix
    import Test.HUnit 
    import qualified Control.Applicative as Aqui
    import Data.Maybe
    
{----------------------------------------------- [OBSERVAÇÕES] ---------------------------------------------------

╰> Todos os testes vão dentro da lista de testes
╰> Os testes são declarados na forma 
╰> "nomeTeste" ~: oQueVoceTaTestando ~?= resultadoEsperado
╰> Se o teste for muito complicado é melhor separar ele 
╰> Em outras funções lá em baixo 
╰> Rode os testes com stack runghc -- test/conwaysTest.hs

------------------------------------------------ [CONSTANTES] ----------------------------------------------------}

{--
    Constantes: utilizadas para trazer flexibilidade ao código
--}

    conways :: Cell
    conways = Cell "C" (Rule [3] [2,3]) "VERDE"

    highLife :: Cell
    highLife = Cell "H" (Rule [3, 6] [2,3]) "VERMELHO"
    
    square :: Int -> Matrix (Maybe Cell)
    square n = gridGenerate n n

    fullGOL3Grid :: Matrix (Maybe Cell)
    fullGOL3Grid = insertCells (square 3) conways [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]
 
    fullHL3Grid :: Matrix (Maybe Cell)
    fullHL3Grid = insertCells (square 3) highLife [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]

------------------------------------------------ [VARIÁVEIS] ------------------------------------------------------

{--
    Variáveis: de uso exclusivo do main
--}

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
            testNumOfLiveNeighbors4, testGetCellTrue, testGetCellFalse, testLifeCellsCoord0, testLifeCellsCoord1,
            testLifeCellsCoord3, testValidCoord0, testValidCoord1, testValidCoord2, testValidCoord3, testValidCoord4,
            testValidCoord5, testValidCoord6, testValidCoord7, testValidCoord8, testValidCoord9, testValidCoord10, 
            testListOfValidCoords0, testListOfValidCoords1, testListOfValidCoords2, testListOfValidCoords3, 
            testListOfValidCoords4, testListOfValidCoords5, testMostFrequentCell0, testMostFrequentCell2,
            testMostFrequentCell3, testMostFrequentCell4, noChangeGenerations0, noChangeGenerations1, 
            noChangeGenerations2, noChangeGenerations3, noChangeGenerations4, isDeadSimulation0,isDeadSimulation1, 
            isDeadSimulation2, gridToLists0, gridToLists0, gridToLists1, gridToLists2, gridToLists3, insertCell0,
            insertCell1, insertCell2, testListOfCoord2, testListOfCoord3
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
    gridGenerateFromList: verifica se um grid gerado a partir de uma lista
    é equivalente a outro já definido
--}

    testGridGenerateFromList0 :: Test
    testGridGenerateFromList0 = TestCase $ do
        assertEqual "gridGenerateFromList 0" expectedGrid resultGrid
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
        assertEqual "gridGenerateFromList 1" expectedGrid resultGrid
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
        assertEqual "gridGenerateFromList 2" expectedGrid resultGrid
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
        assertEqual "gridGenerateFromList 3" expectedGrid resultGrid
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
    numOfDeadNeighbors: cria um grid 3x3 de células mortas e verifica se há 8 vizinhos
    para uma célula localizada no meio da matriz, no primeiro caso...
--}

    testNumOfDeadNeighbors0 :: Test
    testNumOfDeadNeighbors0 = TestCase $ do
        assertEqual "numOfDeadNeighbors 0" expectedNum resultNum
        where
            expectedNum = 8
            resultNum = numOfDeadNeighbors (2,2) (square 3)

    testNumOfDeadNeighbors1 :: Test
    testNumOfDeadNeighbors1 = TestCase $ do
        assertEqual "numOfDeadNeighbors 1" expectedNum resultNum
        where
            expectedNum = 8
            resultNum = numOfDeadNeighbors (1,1) (square 1)

    testNumOfDeadNeighbors2 :: Test
    testNumOfDeadNeighbors2 = TestCase $ do
        assertEqual "numOfDeadNeighbors 2" expectedNum resultNum
        where
            expectedNum = 4
            grid = insertCells (square 3) conways [(1,2), (2,1), (2,3), (3,2)]
            resultNum = numOfDeadNeighbors (2,2) grid

    testNumOfDeadNeighbors3 :: Test
    testNumOfDeadNeighbors3 = TestCase $ do
        assertEqual "numOfDeadNeighbors 3" expectedNum resultNum
        where
            expectedNum = 0
            preGrid = insertCells (square 3) highLife [(1,1), (1,3), (3,1), (3,3)]
            grid = insertCells preGrid conways [(1,2), (2,1), (2,3), (3,2)]
            resultNum = numOfDeadNeighbors (2,2) grid

-----------------------------------------------------------------------------------------------------------------

{--
    numOfLiveNeighbors: verificamos se a função retorna a quantidade de células 
    existentes na vizinhança de uma certa célula da grade
--}

    testNumOfLiveNeighbors0 :: Test
    testNumOfLiveNeighbors0 = TestCase $ do
        assertEqual "numOfLiveNeighbors 0" expectedNum resultNum
        where
            expectedNum = 8
            resultNum = numOfLiveNeighbors (2,2) fullGOL3Grid

    testNumOfLiveNeighbors1 :: Test
    testNumOfLiveNeighbors1 = TestCase $ do
        assertEqual "numOfLiveNeighbors 1" expectedNum resultNum
        where
            expectedNum = 0
            resultNum = numOfLiveNeighbors (2,2) (square 3)

    testNumOfLiveNeighbors2 :: Test
    testNumOfLiveNeighbors2 = TestCase $ do
        assertEqual "numOfLiveNeighbors 2" expectedNum resultNum
        where
            expectedNum = 4
            grid = insertCells (square 3) conways [(1,1), (1,3), (3,1), (3,3)]
            resultNum = numOfLiveNeighbors (2,2) grid

    testNumOfLiveNeighbors3 :: Test
    testNumOfLiveNeighbors3 = TestCase $ do
        assertEqual "numOfLiveNeighbors 3" expectedNum resultNum
        where
            expectedNum = 4
            grid = insertCells (square 3) conways [(1,2), (2,1), (2,3), (3,2)]
            resultNum = numOfLiveNeighbors (2,2) grid

    testNumOfLiveNeighbors4 :: Test
    testNumOfLiveNeighbors4 = TestCase $ do
        assertEqual "numOfLiveNeighbors 4" expectedNum resultNum
        where
            expectedNum = 0
            resultNum = numOfLiveNeighbors (1,1) (square 1)

-----------------------------------------------------------------------------------------------------------------

{--
    getCell: aqui, verificamos os seus dois casos
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

{--
    lifeCellsCoord: verifica se a função retorna uma lista com as coordenadas 
    de todas as células vizinhas a determinada célula
--}

    testLifeCellsCoord0 :: Test
    testLifeCellsCoord0 = TestCase $ do
        assertEqual "lifeCellsCoord 0" result expected
            where
                result = lifeCellsCoord (2,2) fullGOL3Grid
                expected = [(1,1), (1,2),(1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]


    testLifeCellsCoord1 :: Test
    testLifeCellsCoord1 = TestCase $ do
        assertEqual "lifeCellsCoord 1" result expected
            where
                result = lifeCellsCoord (2,2) (square 3)
                expected = []
    
    testLifeCellsCoord2 :: Test
    testLifeCellsCoord2 = TestCase $ do
        assertEqual "lifeCellsCoord 2" result expected
            where
                result = lifeCellsCoord (1,2) fullGOL3Grid
                expected = [(1,1), (1,3), (2,1), (2,2), (2,3)]

    testLifeCellsCoord3 :: Test
    testLifeCellsCoord3 = TestCase $ do
        assertEqual "lifeCellsCoord 3" result expected
            where
                result = lifeCellsCoord (1,3) fullGOL3Grid
                expected = [(1,2), (2,2), (2,3)]

-----------------------------------------------------------------------------------------------------------------

{--
    validCoord: verifica se o retorno da função é uma determinada coordenada 
    que faz parte do escopo de uma dada matriz
--}

    testValidCoord0 :: Test
    testValidCoord0 = TestCase $ do
        assertEqual "validCoord 0" result expected
            where
                expected = True
                result = validCoord (1,1) 3 3

    testValidCoord1 :: Test
    testValidCoord1 = TestCase $ do
        assertEqual "validCoord 1" result expected
            where
                expected = True
                result = validCoord (1,2) 3 3

    testValidCoord2 :: Test
    testValidCoord2 = TestCase $ do
        assertEqual "validCoord 2" result expected
            where
                expected = True
                result = validCoord (1,3) 3 3

    testValidCoord3 :: Test
    testValidCoord3 = TestCase $ do
        assertEqual "validCoord 3" result expected
            where
                expected = True
                result = validCoord (2,1) 3 3

    testValidCoord4 :: Test
    testValidCoord4 = TestCase $ do
        assertEqual "validCoord 4" result expected
            where
                expected = True
                result = validCoord (2,2) 3 3
    
    testValidCoord5 :: Test
    testValidCoord5 = TestCase $ do
        assertEqual "validCoord 5" result expected
            where
                expected = True
                result = validCoord (2,3) 3 3

    testValidCoord6 :: Test
    testValidCoord6 = TestCase $ do
        assertEqual "validCoord 6" result expected
            where
                expected = True
                result = validCoord (3,1) 3 3

    testValidCoord7 :: Test
    testValidCoord7 = TestCase $ do
        assertEqual "validCoord 7" result expected
            where
                expected = True
                result = validCoord (3,2) 3 3

    testValidCoord8 :: Test
    testValidCoord8 = TestCase $ do
        assertEqual "validCoord 8" result expected
            where
                expected = True
                result = validCoord (3,3) 3 3

    testValidCoord9 :: Test
    testValidCoord9 = TestCase $ do
        assertEqual "validCoord 9" result expected
            where
                expected = False
                result = validCoord (1,2) 1 1

    testValidCoord10 :: Test
    testValidCoord10 = TestCase $ do
        assertEqual "validCoord 9" result expected
            where
                expected = False
                result = validCoord (2,1) 1 1
    
-----------------------------------------------------------------------------------------------------------------

{--
    listOfCoord: verifica se a lista contempla todas coordenadas que estão na 
    vizinhança de determinada coordenada, independente do escopo da matriz
--}

    testListOfCoord0 :: Test
    testListOfCoord0 = TestCase $ do
        assertEqual "listOfCoord 0" expected result
            where
                result = listOfCoord (2,2) (square 3)
                expected = [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]

    testListOfCoord1 :: Test
    testListOfCoord1 = TestCase $ do
        assertEqual "listOfCoord 1" expected result
            where
                result = listOfCoord (4,4) (square 3)
                expected = [(3,3), (3,4), (3,5), (4,3), (4,5), (5,3), (5,4), (5,5)]

    testListOfCoord2 :: Test
    testListOfCoord2 = TestCase $ do
        assertEqual "listOfCoord 2" expected result
            where
                result = listOfCoord (2,2) (square 1)
                expected = [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]

    testListOfCoord3 :: Test
    testListOfCoord3 = TestCase $ do
        assertEqual "listOfCoord 3" expected result
            where
                result = listOfCoord (4,4) (square 100)
                expected = [(3,3), (3,4), (3,5), (4,3), (4,5), (5,3), (5,4), (5,5)]

-----------------------------------------------------------------------------------------------------------------

{--
    listOfValidCoords: verifica se a função retorna apenas coordenadas válidas,
    ou seja, aquelas que não ultrapassam o escopo da matriz
--}

    testListOfValidCoords0 :: Test
    testListOfValidCoords0 = TestCase $ do
        assertEqual "listOfValidCoords 0" expected result
            where
                result = listOfValidCoords [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)] 1 1
                expected = [(1,1)]

    testListOfValidCoords1 :: Test
    testListOfValidCoords1 = TestCase $ do
        assertEqual "listOfValidCoords 1" expected result
            where
                result = listOfValidCoords [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)] 2 2
                expected = [(1,1), (1,2), (2,1), (2,2)]

    testListOfValidCoords2 :: Test
    testListOfValidCoords2 = TestCase $ do
        assertEqual "listOfValidCoords 2" expected result
            where
                result = listOfValidCoords [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)] 3 3
                expected = [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]
                
    testListOfValidCoords3 :: Test
    testListOfValidCoords3 = TestCase $ do
        assertEqual "listOfValidCoords 3" expected result
            where
                result = listOfValidCoords [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)] 4 4
                expected = [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]

        
    testListOfValidCoords4 :: Test
    testListOfValidCoords4 = TestCase $ do
        assertEqual "listOfValidCoords 4" expected result
            where
                result = listOfValidCoords [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)] 0 0
                expected = []

    testListOfValidCoords5 :: Test
    testListOfValidCoords5 = TestCase $ do
        assertEqual "listOfValidCoords 5" expected result
            where
                result = listOfValidCoords [] 1 1
                expected = []

-----------------------------------------------------------------------------------------------------------------

{--
    mostFrequentCell: verifica se a função retorna a célula que mais se 
    repete na vizinhança de uma dada célula
--}

    testMostFrequentCell0 :: Test
    testMostFrequentCell0 = TestCase $ do
        assertEqual "mostFrequentCell 0" expected result
            where
                result =  mostFrequentCell (2,2) fullGOL3Grid
                expected = Just conways

    testMostFrequentCell1 :: Test
    testMostFrequentCell1 = TestCase $ do
        assertEqual "mostFrequentCell 1" expected result
            where
                result = mostFrequentCell (2,2) (square 3)
                expected = Nothing 
    
    testMostFrequentCell2 :: Test
    testMostFrequentCell2 = TestCase $ do
        assertEqual "mostFrequentCell 2" expected result
            where
                result =  mostFrequentCell (2,2) fullHL3Grid
                expected = Just highLife

    testMostFrequentCell3 :: Test
    testMostFrequentCell3 = TestCase $ do
        assertEqual "mostFrequentCell 3" expected result
            where
                grid = insertCells fullGOL3Grid highLife [(3,1), (3,2), (3,3)]
                result =  mostFrequentCell (2,2) grid
                expected = Just conways
    
    testMostFrequentCell4 :: Test
    testMostFrequentCell4 = TestCase $ do
        assertEqual "mostFrequentCell 4" expected result
            where
                result =  mostFrequentCell (4,4) fullGOL3Grid
                expected = Just conways

-----------------------------------------------------------------------------------------------------------------

{--
    noChangeGenerations: verifica se a função retorna se houve mudanças 
    entre duas gerações da simulação. Uma forma simples de fazer isso
    é comparado duas grids e iguais ou não
--}

    noChangeGenerations0 :: Test
    noChangeGenerations0 = TestCase $ do
        assertEqual "noChangeGenerations 0" expected result
            where
                result = noChangeGenerations fullGOL3Grid fullGOL3Grid
                expected = True

    noChangeGenerations1 :: Test
    noChangeGenerations1 = TestCase $ do
        assertEqual "noChangeGenerations 1" expected result
            where
                result = noChangeGenerations fullHL3Grid fullHL3Grid
                expected = True

    noChangeGenerations2 :: Test
    noChangeGenerations2 = TestCase $ do
        assertEqual "noChangeGenerations 2" expected result
            where
                result = noChangeGenerations fullHL3Grid fullGOL3Grid
                expected = False

    noChangeGenerations3 :: Test
    noChangeGenerations3 = TestCase $ do
        assertEqual "noChangeGenerations 3" expected result
            where
                result = noChangeGenerations (square 3) (square 3)
                expected = True

    noChangeGenerations4 :: Test
    noChangeGenerations4 = TestCase $ do
        assertEqual "noChangeGenerations 4" expected result
            where
                result = noChangeGenerations fullGOL3Grid (square 3)
                expected = False

-----------------------------------------------------------------------------------------------------------------

{--
    listOfValidCoords: verifica se a função retorna apenas coordenadas válidas,
    ou seja, aquelas que não ultrapassam o escopo da matriz
--}

    isDeadSimulation0 :: Test
    isDeadSimulation0 = TestCase $ do
        assertEqual "isDeadSimulation 0" expected result
            where
                result = isDeadSimulation (square 3)
                expected = True

    isDeadSimulation1 :: Test
    isDeadSimulation1 = TestCase $ do
        assertEqual "isDeadSimulation 1" expected result
            where
                result = isDeadSimulation fullGOL3Grid
                expected = False

    isDeadSimulation2 :: Test
    isDeadSimulation2 = TestCase $ do
        assertEqual "isDeadSimulation 2" expected result
            where
                grid = insertCells (square 3) conways [(1,1)]
                result = isDeadSimulation grid
                expected = False

-----------------------------------------------------------------------------------------------------------------

{--
    gridToLists: verifica se a função retorna uma lista de listas, onde 
    cada sublista representa uma linha da matriz
--}

    gridToLists0 :: Test
    gridToLists0 = TestCase $ do
        assertEqual "gridToLists 0" expected result
            where
                result = gridToLists (square 3)
                expected = [
                    [Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing]
                    ]

    gridToLists1 :: Test
    gridToLists1 = TestCase $ do
        assertEqual "gridToLists 1" expected result
            where
                result = gridToLists fullGOL3Grid
                expected = [
                    [Just conways, Just conways, Just conways],
                    [Just conways, Just conways, Just conways],
                    [Just conways, Just conways, Just conways]
                    ]

    gridToLists2 :: Test
    gridToLists2 = TestCase $ do
        assertEqual "gridToLists 2" expected result
            where
                result = gridToLists fullHL3Grid
                expected = [
                    [Just highLife, Just highLife, Just highLife],
                    [Just highLife, Just highLife, Just highLife],
                    [Just highLife, Just highLife, Just highLife]
                    ]

    gridToLists3 :: Test
    gridToLists3 = TestCase $ do
        assertEqual "gridToLists 3" expected result
            where
                grid = insertCells (insertCells (square 3) conways [(1,2), (2,1), (2,3), (3,2)]) highLife [(1,1), (1,3), (3,1), (3,3)]
                result = gridToLists grid
                expected = [
                    [Just highLife, Just conways, Just highLife],
                    [Just conways,     Nothing   , Just conways],
                    [Just highLife, Just conways, Just highLife]
                    ]

-----------------------------------------------------------------------------------------------------------------

{--
    insertCell: verifica se a função retorna uma matriz com a célula devidamente inserida
--}

    insertCell0 :: Test
    insertCell0 = TestCase $ do
        assertEqual "insertCell 0" expected result
            where
                result = insertCell (square 3) conways (2,2)
                expected = 
                    fromLists [
                        [Nothing,    Nothing,   Nothing],
                        [Nothing, Just conways, Nothing],
                        [Nothing,    Nothing,   Nothing]
                    ]

    insertCell1 :: Test
    insertCell1 = TestCase $ do
        assertEqual "insertCell 1" expected result
            where
                result = insertCell (square 1) highLife (1,1)
                expected = fromLists [[Just highLife]]

    insertCell2 :: Test
    insertCell2 = TestCase $ do
        assertEqual "insertCell 2" expected result
            where
                result = insertCell fullHL3Grid conways (2,2)
                expected = 
                    fromLists [
                        [Just highLife, Just highLife, Just highLife],
                        [Just highLife, Just  conways, Just highLife],
                        [Just highLife, Just highLife, Just highLife]
                    ]

-----------------------------------------------------------------------------------------------------------------