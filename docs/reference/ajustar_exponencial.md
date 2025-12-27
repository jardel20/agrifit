# Ajusta o Modelo Exponencial

Implementa o ajuste do modelo de regressão exponencial: Y = a \* exp(b
\* X). O ajuste é feito usando `nls` (Non-linear Least Squares).

## Usage

``` r
ajustar_exponencial(dose, ..., verbose = TRUE)
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
dose <- c(0, 25, 50, 75, 100, 125)
resposta <- c(10, 12, 15, 20, 28, 40)

resultado <- ajustar_exponencial(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
