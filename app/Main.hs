module Main where

import Models.Grid
import Models.Ruler

main :: IO ()
main = do

    putStrLn "Digite uma sequência de números representando a quantidade de vizinhos para instanciar a regra"
    putStr "Regra de nascimento: "
    
    nascStr <- getLine
    let nascList = map (\x -> read [x] :: Int) nascStr

    putStr "Regra de permanênmcia: "

    stayStr <- getLine
    let stayList = map (\x -> read [x] :: Int) stayStr


    let regra = Ruler nascList stayList

    putStrLn ("A sua regra tem a seguinte forma " ++ (show regra))

