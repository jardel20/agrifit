# Ajusta o Modelo Linear Simples

Implementa o ajuste do modelo de regressão linear simples: Y = b0 + b1
\* X.

## Usage

``` r
ajustar_linear(dose, ..., verbose = TRUE)
```

## Arguments

- dose:

  Vetor numérico com níveis do fator (X).

- ...:

  Uma ou mais respostas (vetores Y nomeados). Exemplo:
  `MS = materia_seca`.

- verbose:

  Lógico. Gera prints detalhados (padrão: TRUE).

## Value

Lista com:

- resultados:

  Data frame: b0, b1, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada
  resposta.

- modelos:

  Lista dos objetos `lm` para cada resposta.

- equacoes:

  Strings formatadas das equações.

- graficos:

  Lista de gráficos ggplot2 com observações e reta ajustada para cada
  resposta.

## Examples

``` r
if (FALSE) { # \dontrun{
dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 15, 20, 25, 30, 35)

resultado <- ajustar_linear(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
