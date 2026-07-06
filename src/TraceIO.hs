-- | Leitura e exibição de traços (§4 do artigo). Formato textual: uma linha por
-- evento, com proposições separadas por espaço, vírgula ou ponto e vírgula.
-- Linhas em branco e iniciadas por @#@ são ignoradas; @-@ denota o evento vazio.
module TraceIO
  ( parseAP
  , parseEvent
  , parseTrace
  , readTraceFile
  , renderAP
  , renderEvent
  ) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import qualified Data.Set as Set
import Types

-- | Converte um token textual numa proposição atômica (case-insensitive).
parseAP :: String -> Maybe AP
parseAP tok = case map toLower tok of
  "chl"   -> Just Chl
  "chl_k" -> Just Chl
  "rsp"   -> Just Rsp
  "rsp_k" -> Just Rsp
  "live"  -> Just Live
  "fake"  -> Just Fake
  _       -> Nothing

-- | Uma linha vira um evento (conjunto de APs). Tokens desconhecidos (inclusive
-- @-@ e @neutro@) são ignorados, de modo que tais linhas produzem o evento vazio.
parseEvent :: String -> Event
parseEvent line =
  Set.fromList [ ap | tok <- words (map normalize line), Just ap <- [parseAP tok] ]
  where
    normalize c = if c == ',' || c == ';' then ' ' else c

-- | Analisa um traço completo, ignorando linhas em branco e comentários (@#@).
parseTrace :: String -> Trace
parseTrace = map parseEvent . filter relevant . lines
  where
    relevant l = case dropWhile isSpace l of
      ""      -> False
      ('#':_) -> False
      _       -> True

-- | Lê um traço de um arquivo.
readTraceFile :: FilePath -> IO Trace
readTraceFile fp = parseTrace <$> readFile fp

-- | Símbolo textual de uma proposição atômica.
renderAP :: AP -> String
renderAP Chl  = "chl_k"
renderAP Rsp  = "rsp_k"
renderAP Live = "live"
renderAP Fake = "fake"

-- | Representação legível de um evento, e.g. @{chl_k}@ ou @∅@.
renderEvent :: Event -> String
renderEvent ev
  | Set.null ev = "(vazio)"
  | otherwise   = "{" ++ intercalate "," (map renderAP (Set.toList ev)) ++ "}"
