# Ajusta o Modelo Raiz Quadrático

Implementa o ajuste do modelo de regressão raiz quadrática: Y = b0 + b1
\* sqrt(X) + b2 \* X.

## Usage

``` r
ajustar_raiz_quadratica(dose, ..., verbose = TRUE)
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

  Data frame: b0, b1, b2, R², AIC, BIC, RMSE, p-valores, IC, etc. para
  cada resposta.

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
dose <- c(0, 25, 50, 100, 150, 200)
resposta <- c(10, 16, 20, 26, 30, 33)

resultado <- ajustar_raiz_quadratica(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
