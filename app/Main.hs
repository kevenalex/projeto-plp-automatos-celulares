module Main where

    import Models.Grid
    import Models.Rule
    import Models.Cell
    import Data.Matrix

    import Control.Concurrent (threadDelay)
    import System.Console.ANSI (clearScreen)

    import Controllers.Menu (menu)
    
  {-   main :: IO ()
    main = do
      putStrLn "Oi"
      
      -- let golRule = Rule [3] [1,2,3,4,5]
      -- let golCell = Cell "Maze" golRule "Verde"

      -- let state0 = gridGenerate 9 9
      -- let state1 = insertCell state0 golCell (1,2)
      -- let state2 = insertCell state1 golCell (2,3)
      -- let state3 = insertCell state2 golCell (3,1)
      -- let state4 = insertCell state3 golCell (3,2)
      -- let state5 = insertCell state4 golCell (3,3)

      -- printAndClear state5 20

      
-- printsList :: Matrix (Maybe Cell) -> Int -> IO [String]
-- printsList _ 0 = return [] -- Caso base: quando n for 0, para a recursão
-- printsList grid n = do
--     updates <- print grid
--     printsList updates : (gridUpdate grid) (n - 1)

<<<<<<< HEAD
 -}
    main :: IO()
    main = menu
=======
    -- threadDelay 1000000  -- Espera 1 segundo (1 segundo = 1.000.000 microssegundos)
    -- clearScreen
>>>>>>> 55f5473cdae25833b80b33002fbd07de9f8da202
