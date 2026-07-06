-- | Autômato-monitor da política reduzida Φ_red (§3.4–3.5 do artigo).
-- O DFA é uma função de transição pura; o veredito de três valores decorre da
-- classificação do estado alcançado.
module Monitor
  ( State(..)
  , initial
  , delta
  , renderState
  , classify
  , finalVerdict
  , step
  , runTrace
  , openVerdict
  , endedVerdict
  ) where

import Data.Set (member)
import Types

-- | Estados do DFA da §3.5: q_{ℓp}, com ℓ = vivacidade observada e
-- p = desafio pendente; mais a armadilha de rejeição irrevogável.
data State
  = Q Bool Bool  -- ^ @Q live pending@
  | QRej         -- ^ armadilha (síntese observada).
  deriving (Eq, Show)

-- | Estado inicial q_{00}.
initial :: State
initial = Q False False

-- | Função de transição δ : Q × Σ → Q, exatamente a tabela da §3.5.
-- Regra de prioridade: @fake@ leva à armadilha; senão atualiza-se vivacidade e
-- pendência, com @rsp@ tendo precedência sobre @chl@ no mesmo evento.
delta :: State -> Event -> State
delta QRej _ = QRej
delta (Q live pending) ev
  | Fake `member` ev = QRej
  | otherwise        = Q live' pending'
  where
    live' = live || Live `member` ev
    pending'
      | Rsp `member` ev = False
      | Chl `member` ev = True
      | otherwise       = pending

-- | Rótulo legível do estado (q00, q01, q10, q11, q_rej).
renderState :: State -> String
renderState QRej     = "q_rej"
renderState (Q l p)  = "q" ++ bit l ++ bit p
  where bit b = if b then "1" else "0"

-- | Veredito de três valores sobre o prefixo lido até este estado.
-- ⊥ só é definitivo na armadilha; o aceite não se firma antes do término,
-- pois φ_auth = G¬fake permanece refutável enquanto a sessão prossegue.
classify :: State -> Verdict
classify QRej = Reject
classify _    = Inconclusive

-- | Veredito ao término do traço: aceita-se sse o estado pertence a F = {q_{10}}.
finalVerdict :: State -> Verdict
finalVerdict (Q True False) = Accept
finalVerdict _              = Reject

-- | Um passo de monitoramento online: estado alcançado e veredito de prefixo.
step :: State -> Event -> (State, Verdict)
step st ev = let st' = delta st ev in (st', classify st')

-- | Execução passo a passo: para cada evento, o estado alcançado e o veredito
-- de prefixo emitido naquele instante.
runTrace :: Trace -> [(Event, State, Verdict)]
runTrace = go initial
  where
    go _  []       = []
    go st (ev:evs) = let st' = delta st ev
                     in (ev, st', classify st') : go st' evs

-- | Veredito caso a sessão seja interrompida no fim do que foi lido
-- (semântica de prefixo: ? ou ⊥).
openVerdict :: Trace -> Verdict
openVerdict = classify . foldl delta initial

-- | Veredito caso a sessão seja declarada encerrada (semântica final: ⊤ ou ⊥).
endedVerdict :: Trace -> Verdict
endedVerdict = finalVerdict . foldl delta initial
