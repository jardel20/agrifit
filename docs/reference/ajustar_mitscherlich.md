# Ajusta o Modelo de Mitscherlich

Implementa o ajuste do modelo de Mitscherlich: Y = A - B \* c^X. O
ajuste é feito usando `nls` (Non-linear Least Squares).

## Usage

``` r
ajustar_mitscherlich(dose, ..., verbose = TRUE)
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

  Data frame: A, B, c, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada
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
dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(5, 12, 18, 22, 25, 27)

resultado <- ajustar_mitscherlich(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
