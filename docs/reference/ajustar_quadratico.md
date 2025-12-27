# Ajusta o Modelo Quadrático

Implementa o ajuste do modelo de regressão quadrática: Y = b0 + b1 \*
X + b2 \* X^2.

## Usage

``` r
ajustar_quadratico(dose, ..., verbose = TRUE)
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

## Examples

``` r
if (FALSE) { # \dontrun{
dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 18, 24, 28, 30, 31)

resultado <- ajustar_quadratico(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
} # }
```
