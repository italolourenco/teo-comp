-- | Tipos básicos do modelo formal (§3.1–3.2 do artigo): proposições atômicas,
-- eventos, traços e o veredito de três valores.
module Types
  ( AP(..)
  , Event
  , Trace
  , Verdict(..)
  ) where

import Data.Set (Set)

-- | Proposições atômicas observáveis em um passo da sessão de prova de vida.
-- Caso reduzido com um único tipo de desafio @k@ (cf. §3.5).
data AP
  = Chl   -- ^ @chl_k@: o serviço emitiu o desafio do tipo k.
  | Rsp   -- ^ @rsp_k@: a resposta esperada para o desafio k foi observada.
  | Live  -- ^ @live@: o detector sinalizou captura compatível com pessoa viva.
  | Fake  -- ^ @fake@: o detector sinalizou mídia sintética.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Um evento é uma valoração das proposições: σ ∈ Σ = 2^AP.
type Event = Set AP

-- | Uma sessão é um traço finito de eventos: w ∈ Σ*.
type Trace = [Event]

-- | Veredito de três valores no sentido de Bauer, Leucker e Schallhart (2011).
data Verdict
  = Accept        -- ^ ⊤ — sessão aceita (definitivo, só ao término).
  | Reject        -- ^ ⊥ — sessão rejeitada (definitivo).
  | Inconclusive  -- ^ ? — observação parcial ainda não decide a política.
  deriving (Eq, Show)
