-- | Testes dos vereditos do monitor sobre os traços da Seção 5 do artigo, mais
-- casos de borda de δ (átomos combinados, evento vazio, traço vazio), do parser
-- (TraceIO) e a leitura efetiva dos arquivos traces/*.trace.
-- Sem dependências externas: usa apenas 'base'/'containers' e falha com exitFailure.
module Main (main) where

import qualified Data.Set as Set
import Control.Monad (forM_, unless)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import System.Exit (exitFailure)
import Monitor
import Types
import TraceIO

ev :: [AP] -> Event
ev = Set.fromList

-- | (nome, traço, veredito-se-interrompido, veredito-se-encerrado).
cases :: [(String, Trace, Verdict, Verdict)]
cases =
  [ ( "A. válido (chl, rsp, live)"
    , [ ev [Chl], ev [Rsp], ev [Live] ],          Inconclusive, Accept )
  , ( "A'. válido com live primeiro (live, chl, rsp)"
    , [ ev [Live], ev [Chl], ev [Rsp] ],          Inconclusive, Accept )
  , ( "B. fake injetado (chl, fake, live)"
    , [ ev [Chl], ev [Fake], ev [Live] ],         Reject,       Reject )
  , ( "C. desafio sem resposta (chl, live)"
    , [ ev [Chl], ev [Live] ],                    Inconclusive, Reject )
  , ( "D. sem vivacidade (chl, rsp)"
    , [ ev [Chl], ev [Rsp] ],                     Inconclusive, Reject )
  , ( "E. sessão interrompida (chl)"
    , [ ev [Chl] ],                               Inconclusive, Reject )
  ]

-- | Casos de borda de δ não cobertos pela Seção 5: eventos com múltiplos átomos
-- (regras de prioridade), evento vazio (coluna neutro) e traço vazio.
edgeCases :: [(String, Trace, Verdict, Verdict)]
edgeCases =
  [ ( "F. fake tem prioridade sobre live ({fake,live} => q_rej)"
    , [ ev [Fake, Live] ],                        Reject,       Reject )
  , ( "G. rsp precede chl no mesmo evento ({chl,rsp} zera pendência), com live antes"
    , [ ev [Live], ev [Chl, Rsp] ],               Inconclusive, Accept )
  , ( "H. chl e live no mesmo evento (q11: vivo mas pendente)"
    , [ ev [Chl, Live] ],                         Inconclusive, Reject )
  , ( "I. fake tem prioridade mesmo com rsp ({fake,rsp} => q_rej)"
    , [ ev [Chl], ev [Fake, Rsp] ],               Reject,       Reject )
  , ( "J. evento vazio (neutro) preserva o estado"
    , [ ev [Chl], ev [Rsp], ev [], ev [Live] ],   Inconclusive, Accept )
  , ( "K. traço vazio: sem observação"
    , [],                                          Inconclusive, Reject )
  ]

-- | A rejeição por fake é precoce: B já emite Reject no 2º prefixo.
earlyRejectCheck :: (String, Bool)
earlyRejectCheck =
  let prefixVerdicts = [ v | (_, _, v) <- runTrace [ ev [Chl], ev [Fake], ev [Live] ] ]
  in ( "B emite Reject já no 2º passo (rejeição precoce)"
     , length prefixVerdicts >= 2 && prefixVerdicts !! 1 == Reject )

-- | Casos do parser TraceIO: tokens, separadores, aliases e ruído.
parserCases :: [(String, Event, Event)]
parserCases =
  [ ( "case-insensitive e separadores mistos"
    , parseEvent "CHL, Fake; Live",  ev [Chl, Fake, Live] )
  , ( "aliases chl_k/rsp_k equivalem a chl/rsp"
    , parseEvent "chl_k rsp_k",      ev [Chl, Rsp] )
  , ( "tokens desconhecidos e '-' produzem evento vazio"
    , parseEvent "- neutro xyz",     ev [] )
  ]

-- | O parser de traço ignora linhas em branco e comentários '#'.
parseTraceCase :: (String, Bool)
parseTraceCase =
  ( "parseTrace ignora comentários e linhas em branco"
  , parseTrace "# comentário\nchl\n\n  # outro\nrsp , live\n"
      == [ ev [Chl], ev [Rsp, Live] ] )

-- | Espelho da tabela da Seção 5: cada arquivo deve produzir estes vereditos.
traceFiles :: [(FilePath, Verdict, Verdict)]
traceFiles =
  [ ( "traces/valido.trace",              Inconclusive, Accept )
  , ( "traces/fake-injetado.trace",       Reject,       Reject )
  , ( "traces/desafio-sem-resposta.trace",Inconclusive, Reject )
  , ( "traces/sem-vivacidade.trace",      Inconclusive, Reject )
  , ( "traces/interrompida.trace",        Inconclusive, Reject )
  ]

main :: IO ()
main = do
  fails <- newIORef (0 :: Int)
  forM_ (cases ++ edgeCases) $ \(name, tr, expOpen, expEnded) -> do
    check fails (name ++ " [interrompida]") (openVerdict tr)  expOpen
    check fails (name ++ " [encerrada]")    (endedVerdict tr) expEnded
  let (ername, erok) = earlyRejectCheck
  reportBool fails ername erok
  forM_ parserCases $ \(name, got, want) ->
    reportBool fails (name ++ " => " ++ show (Set.toList got)) (got == want)
  let (ptname, ptok) = parseTraceCase
  reportBool fails ptname ptok
  forM_ traceFiles $ \(fp, expOpen, expEnded) -> do
    tr <- readTraceFile fp
    check fails (fp ++ " [interrompida]") (openVerdict tr)  expOpen
    check fails (fp ++ " [encerrada]")    (endedVerdict tr) expEnded
  n <- readIORef fails
  if n == 0
    then putStrLn "OK: todos os testes passaram."
    else putStrLn (show n ++ " teste(s) falharam.") >> exitFailure
  where
    check ref label got want =
      reportBool ref (label ++ " => " ++ show got) (got == want)

reportBool :: IORef Int -> String -> Bool -> IO ()
reportBool ref label ok = do
  putStrLn $ (if ok then "[ok]   " else "[FAIL] ") ++ label
  unless ok $ modifyIORef' ref (+ 1)
