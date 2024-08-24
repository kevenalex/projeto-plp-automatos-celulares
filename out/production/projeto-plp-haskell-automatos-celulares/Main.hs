module Main where

    import Models.Grid
    import Models.Rule
    import Models.Cell

    main :: IO ()
    main = do
      let golRule = Rule [3] [2,3]
      let golCell = Cell "GOL" golRule "Verde"
      let state0 = gridGenerateFromList 3 3 [Nothing, Nothing, Nothing, golCell, golCell, golCell, Nothing, Nothing, Nothing]

      print state0


