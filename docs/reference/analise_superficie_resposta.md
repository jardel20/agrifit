# Ajusta o Modelo Quadrático de Superfície de Resposta (2 Fatores)

Implementa o ajuste do modelo quadrático completo para dois fatores (X1
e X2): Y = b0 + b1*X1 + b2*X2 + b11*X1^2 + b22*X2^2 + b12*X1*X2.

## Usage

``` r
analise_superficie_resposta(X1, X2, ..., verbose = TRUE)
```

## Arguments

- X1:

  Vetor numérico do Fator 1.

- X2:

  Vetor numérico do Fator 2.

- ...:

  Uma ou mais respostas (vetores Y nomeadas). Exemplo:
  `Prod = producao`.

- verbose:

  Lógico. Gera prints detalhados (padrão: TRUE).

## Value

Lista com:

- resultados:

  Data frame: b0, b1, b2, b11, b22, b12, R², AIC, BIC, RMSE, p-valores,
  IC, Ponto Estacionário, etc. para cada resposta.

- modelos:

  Lista dos objetos `lm` para cada resposta.

- equacoes:

  Strings formatadas das equações.

- graficos:

  Lista de gráficos de superfície de resposta 3D para cada resposta.

## Examples

``` r
if (FALSE) { # \dontrun{
X1 <- c(-1, -1, 1, 1, 0, 0, 0, 0, 0)
X2 <- c(-1, 1, -1, 1, 0, 0, 0, 0, 0)
prod <- c(50, 60, 55, 70, 65, 65, 65, 66, 64)

resultado <- analise_superficie_resposta(
  X1 = X1,
  X2 = X2,
  Producao = prod,
  verbose = TRUE
)

print(resultado$resultados)
print(resultado$equacoes)
# resultado$graficos$Producao  # Para visualizar a superfície 3D
} # }
```
