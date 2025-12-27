# Ajusta o Modelo Cúbico

Implementa o ajuste do modelo de regressão cúbica: Y = b0 + b1 \* X + b2
\* X^2 + b3 \* X^3.

## Usage

``` r
ajustar_cubico(dose, ..., verbose = TRUE)
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

  Data frame: b0, b1, b2, b3, R², AIC, BIC, RMSE, p-valores, IC, etc.
  para cada resposta.

- modelos:

  Lista dos objetos `lm` para cada resposta.

- equacoes:

  Strings formatadas das equações.

- graficos:

  Lista de gráficos ggplot2 com observações e curva ajustada para cada
  resposta.

## Examples

``` r
if (FALSE) { # \dontrun{
dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 18, 26, 29, 31, 32)

resultado <- ajustar_cubico(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
