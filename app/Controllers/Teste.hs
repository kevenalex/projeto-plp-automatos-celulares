module Controllers.Teste where

import System.Console.ANSI

type Grid = [[Char]]

-- Função para imprimir uma linha da grid
printRow :: [Char] -> IO ()
printRow row = do
    mapM_ printCell row
    putStrLn ""  -- Move para a próxima linha

-- Função para imprimir uma célula com um quadrado branco ou espaço
printCell :: Char -> IO ()
printCell cell
    | cell == 'O' = do
        setSGR [SetColor Foreground Vivid White]  -- Define a cor do texto como branco
        putStr "██"  -- Imprime um bloco sólido
        setSGR [Reset]  -- Reseta as cores para o padrão
    | otherwise = putStr " "  -- Espaço para célula morta

-- Função para imprimir a grid inteira
printGrid :: Grid -> IO ()
printGrid grid = do
    clearScreen  -- Limpa a tela antes de imprimir
    mapM_ printRow grid

-- Exemplo de uso
main :: IO ()
main = do
    let grid = ["  O    O  ",
                "O  O  O  O",
                "  O    O  ",
                " O      O ",
                "          ",
                " O      O ",
                "  O    O  ",
                "O  O  O  O",
                "  O    O  "]
    printGrid grid