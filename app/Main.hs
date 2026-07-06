-- | CLI do monitor: lê um traço de um arquivo, emite o veredito de prefixo passo
-- a passo e os vereditos de sessão interrompida e de sessão encerrada (§4).
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Monitor
import TraceIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> runFile fp
    _    -> do
      putStrLn "uso: monitor-liveness <arquivo-de-traco>"
      exitFailure

runFile :: FilePath -> IO ()
runFile fp = do
  tr <- readTraceFile fp
  putStrLn $ "Traço: " ++ fp ++ " (" ++ show (length tr) ++ " eventos)"
  putStrLn "passo | evento            | estado | veredito-prefixo"
  putStrLn "------+-------------------+--------+-----------------"
  mapM_ printStep (zip [1 :: Int ..] (runTrace tr))
  putStrLn ""
  putStrLn $ "veredito (sessão interrompida aqui): " ++ show (openVerdict tr)
  putStrLn $ "veredito (sessão encerrada):         " ++ show (endedVerdict tr)
  where
    printStep (i, (ev, st, v)) =
      putStrLn $ pad 5 (show i) ++ " | " ++ pad 17 (renderEvent ev)
                 ++ " | " ++ pad 6 (renderState st) ++ " | " ++ show v
    pad n s = take n (s ++ repeat ' ')
