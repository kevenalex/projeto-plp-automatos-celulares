module Main where

    import Models.Grid
    import Models.Rule
    import Models.Cell

    main :: IO ()
    main = do
      let golRule = Rule [3] [2,3]
      let golCell = Cell "GOL" golRule "Verde"

      let state0 = gridGenerate 9 9
      let state1 = insertCell state0 golCell (1,2)
      let state2 = insertCell state1 golCell (2,3)
      let state3 = insertCell state2 golCell (3,1)
      let state4 = insertCell state3 golCell (3,2)
      let state5 = insertCell state4 golCell (3,3)

      print state5

      let state6 = gridUpdate state5

      print state6
      
      let state7 = gridUpdate state6

      print state7

      let state8 = gridUpdate state7

      print state8

      let state9 = gridUpdate state8

      print state9

      let state10 = gridUpdate state9

      print state10

      let state11 = gridUpdate state10

      print state11

      -- let state2 = gridUpdate state1

      -- print state2


