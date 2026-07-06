# monitor-liveness

Protótipo funcional (Haskell) do **autômato-monitor de prova de vida** descrito no
artigo. A sessão é vista como um traço finito de eventos; o detector de deepfake é um
**oráculo** que rotula eventos; o monitor é um **DFA puro** que compõe esses rótulos com
o protocolo de desafio-resposta e emite `Accept`, `Reject` ou `Inconclusive`.

Implementa o DFA da política reduzida `Φ_red = G¬fake ∧ F live ∧ G(chl_k → (¬fake U rsp_k))`
(Seção 3.5 do artigo).

## Estrutura

| Arquivo | Papel |
|---|---|
| [`src/Types.hs`](src/Types.hs) | proposições atômicas, evento (`Set AP`), traço, veredito de 3 valores |
| [`src/Monitor.hs`](src/Monitor.hs) | DFA como função de transição pura `delta`, classificação de estados, vereditos |
| [`src/TraceIO.hs`](src/TraceIO.hs) | leitor/serializador do formato textual de traços |
| [`app/Main.hs`](app/Main.hs) | CLI: lê um traço e emite o veredito passo a passo |
| [`test/Spec.hs`](test/Spec.hs) | testes dos traços da Seção 5 (sem dependências externas) |
| [`traces/`](traces/) | traços de exemplo (válido, fake injetado, etc.) |

## Como executar

Com **Cabal** (GHC ≥ 8.10):

```sh
cabal build
cabal run monitor-liveness -- traces/valido.trace
cabal test
```

Com **Stack**:

```sh
stack run -- traces/valido.trace
stack test
```

## Formato de traço

Uma linha por evento; proposições separadas por espaço/vírgula. Tokens válidos:
`chl`, `rsp`, `live`, `fake`. Linhas em branco e iniciadas por `#` são ignoradas; uma
linha `-` denota o evento vazio. Exemplo (`traces/valido.trace`):

```
chl
rsp
live
```

## Vereditos

Para cada prefixo o monitor emite o veredito de três valores: `Reject` (⊥) é definitivo
(armadilha de síntese); caso contrário `Inconclusive` (?), pois o aceite só se firma ao
término. Ao encerrar a sessão, aceita-se (⊤) sse o estado final é `q10` (vivacidade
observada e nenhum desafio pendente). O CLI reporta os dois desfechos — *sessão
interrompida* (semântica de prefixo) e *sessão encerrada* (semântica final).
