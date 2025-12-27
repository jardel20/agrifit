# Escolha do Modelo Adequado (RMSE + Critério Flexível + Barras Arranjadas)

Compara modelos por R², AIC, BIC, RMSE com gráficos de barras
ARRANJADOS.

## Usage

``` r
escolha_modelo_adequado(
  ...,
  criterio = "multiplo",
  layout_grid = "auto",
  layout_barras = "auto",
  verbose = TRUE
)
```

## Arguments

- ...:

  Resultados de modelos.

- criterio:

  String: "AIC", "BIC", "RMSE", "R2" ou "multiplo" (padrão).

- layout_grid:

  "linha", "coluna", "grade", "auto" para gráficos dos modelos.

- layout_barras:

  "linha", "coluna", "grade", "auto" para barras de comparação.

- verbose:

  Lógico.

## Value

Lista completa com todos os resultados e gráficos.
