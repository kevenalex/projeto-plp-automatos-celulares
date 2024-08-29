module Controllers.Temp where

import Control.Monad (forever)
import System.IO (hSetEcho, stdin, hSetBuffering, BufferMode(NoBuffering), hReady)
import Control.Concurrent (threadDelay)

-- Função que será executada em loop
loopFunction :: IO ()
loopFunction = do 
  putStrLn "Executando... Pressione qualquer tecla para sair."
  threadDelay 500000

-- Função principal que controla o loop
runLoop :: IO ()
runLoop = do
    hSetBuffering stdin NoBuffering -- Desabilita o buffer do input
    hSetEcho stdin False            -- Desabilita a ecoação do input no terminal
    let checkInput = do
            inputAvaliable <- hReady stdin
            if inputAvaliable then do 
              return() 
            else do
              loopFunction
              runLoop

    checkInput
-- Menu inicial
menu :: IO ()
menu = putStrLn "Menu Inicial"

main :: IO ()
main = do
    putStrLn "eita que legau"
    runLoop
    putStrLn "pega no meu pau"  -- Retorna ao menu inicial depois que o loop é interrompido
