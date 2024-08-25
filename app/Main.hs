module Main where

    import Models.Grid
    import Models.Rule
    import Models.Cell

    import Data.Char
    import Data.Matrix

    import Control.Concurrent (threadDelay)
    import System.Console.ANSI (clearScreen)
    
    main :: IO () 
    main = print "gay"
    
  {-   main :: IO ()
=======
    toAnimal :: Char -> Char
    toAnimal c = chr . (+127970) $ ord c

    toSmileyEmoji :: Char-> Char
    toSmileyEmoji c = chr . (+128415) $ ord c

    encode :: String -> String
    encode xs = map (\c -> if isUpper c then toAnimal c else toSmileyEmoji c) xs

    fromAnimal :: Char -> Char
    fromAnimal c = chr $ (ord c) - 127970

    fromSmileyEmoji :: Char-> Char
    fromSmileyEmoji c = chr $ (ord c) - 128415

    isAnimal :: Char -> Bool
    isAnimal c = if ((ord c >= ord '🐣') && (ord c <= ord '🐼')) then True else False

    decode :: String -> String
    decode xs = map (\c -> if isAnimal c then fromAnimal c else fromSmileyEmoji c) xs

    main :: IO ()
>>>>>>> ef2106f62f7b0c3c17594de33a9fa2e597fd5677
    main = do

      contents1 <- getLine
      contents2 <- getLine
      putStr $ (encode contents1) ++ "\n" ++ (decode contents2)

      -- putStrLn "Oi"
      
      -- let golRule = Rule [3] [1,2,3,4,5]
      -- let golCell = Cell "🐱" golRule "Verde"

      -- let state0 = gridGenerate 9 9
      -- let state1 = insertCell state0 golCell (1,2)
      -- let state2 = insertCell state1 golCell (2,3)
      -- let state3 = insertCell state2 golCell (3,1)
      -- let state4 = insertCell state3 golCell (3,2)
      -- let state5 = insertCell state4 golCell (3,3)



      -- putStr $ show state5

      
-- printsList :: Matrix (Maybe Cell) -> Int -> IO [String]
-- printsList _ 0 = return [] -- Caso base: quando n for 0, para a recursão
-- printsList grid n = do
--     updates <- print grid
--     printsList updates : (gridUpdate grid) (n - 1)

 -}
    -- threadDelay 1000000  -- Espera 1 segundo (1 segundo = 1.000.000 microssegundos)
    -- clearScreen
