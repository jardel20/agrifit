# Ajusta o Modelo Potencial

Implementa o ajuste do modelo de regressão potencial: Y = a \* X^b. O
ajuste é feito usando `nls` (Non-linear Least Squares).

## Usage

``` r
ajustar_potencial(dose, ..., verbose = TRUE)
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

  Data frame: a, b, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada
  resposta.

- modelos:

  Lista dos objetos `nls` para cada resposta.

- equacoes:

  Strings formatadas das equações.

- graficos:

  Lista de gráficos ggplot2 com observações e curva ajustada para cada
  resposta.

## Examples

``` r
if (FALSE) { # \dontrun{
dose <- c(1, 10, 20, 50, 100, 200)
resposta <- c(5, 12, 18, 28, 38, 50)

resultado <- ajustar_potencial(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
