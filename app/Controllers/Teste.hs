module Controllers.Teste where

import System.IO (hFlush, stdout)
import System.Console.ANSI

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      setSGR [SetColor Foreground Dull Blue]
      putStr "Enter your name: "
      setSGR [SetColor Foreground Dull Yellow]
      hFlush stdout  -- flush the output buffer before getLine
      name <- getLine
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "Hello, " ++ name ++ "!"
      setSGR [Reset]  -- reset to default colour scheme
    else
      putStrLn "Standard output does not support 'ANSI' escape codes."