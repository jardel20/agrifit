# Ajusta o Modelo Linear Descontínuo (LRP)

Implementa o ajuste iterativo do modelo Linear Response Plateau (LRP),
testando cada ponto de dose como potencial breakpoint e selecionando o
modelo com maior R². O LRP combina um segmento linear (\\\hat{Y} = b_0 +
b_1 X\\) com plateau (\\\hat{Y} = b_2\\), onde \\X_i = (b_2 - b_0) /
b_1\\ define a transição.

## Usage

``` r
ajustar_LRP(
  dose,
  ...,
  title = "Ajuste do Modelo Linear Descontínuo (LRP)",
  xlab = "Dose",
  ylab = "Resposta",
  adjustment_color = "Set1",
  legend_position_ggplot = "top",
  dashed = FALSE,
  show_intersection = TRUE,
  verbose = TRUE
)
```

## Arguments

- dose:

  Vetor numérico com níveis do fator (X). Mínimo 4 pontos distintos.

- ...:

  Uma ou mais respostas (vetores Y nomeados). Exemplo:
  `MS = materia_seca`.

- title:

  Título do gráfico (padrão: "Ajuste do Modelo Linear Descontínuo
  (LRP)").

- xlab:

  Rótulo eixo X (padrão: "Dose").

- ylab:

  Rótulo eixo Y (padrão: "Resposta").

- adjustment_color:

  Paleta RColorBrewer para curvas (padrão: "Set1").

- legend_position_ggplot:

  Posição da legenda no ggplot (padrão: "top").

- dashed:

  Lógico. Se TRUE, adiciona linhas tracejadas nos breakpoints (padrão:
  FALSE).

- show_intersection:

  Lógico. Mostra ponto de intersecção (X) no gráfico (padrão: TRUE).

- verbose:

  Lógico. Gera gráficos e prints detalhados (padrão: TRUE).

## Value

Lista com:

- resultados:

  Data frame: b0, b1, b2, Xi, R², AIC, BIC, RMSE, p-valores, etc. para
  cada resposta.

- modelos:

  Lista dos melhores modelos por resposta.

- equacoes:

  Strings formatadas das equações LRP.

- nomes_respostas:

  Nomes das respostas processadas.

- grafico_ggplot:

  Gráfico ggplot2 (se verbose=TRUE).

- grafico_plotly:

  Gráfico plotly interativo (se verbose=TRUE).

## Details

**Atualizações:** Inclui cálculo de AIC, BIC, RMSE e testes de
significância para os parâmetros do segmento linear (\\b_0\\ e \\b_1\\).

**Algoritmo iterativo:**

1.  Testa breakpoints de 3 até n-1

2.  Regressão linear pré-breakpoint (`lm`)

3.  Média pós-breakpoint como plateau (b2)

4.  Calcula Xi, R², AIC, BIC, RMSE total

5.  Seleciona melhor R²

**Saídas gráficas** (verbose=TRUE):

- Pontos observados + curvas ajustadas

- Ponto de intersecção opcional (X)

- Linhas tracejadas opcionais (breakpoint)

- Salva "lrp_multiplo.png" (12x8in, 300dpi)

**Dependências:** `ggplot2`, `plotly`, `dplyr`, `tidyr`

## Examples

``` r
if (FALSE) { # \dontrun{
dose_P <- c(0.0, 32.5, 65.0, 97.5, 130.0, 195.0, 260.0, 325.0)
MS <- c(6.74, 8.73, 10.89, 12.56, 14.11, 15.21, 15.50, 15.60)

res <- ajustar_lrp(
  dose = dose_P, MS = MS,
  title = "Matéria Seca vs Dose de P",
  xlab = "P (mg/dm³)", ylab = "MS (g/vaso)",
  adjustment_color = "Dark2"
)

print(res$resultados)
print(res$equacoes)
} # }
```
