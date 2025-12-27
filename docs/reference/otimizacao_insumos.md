# Otimização de Uso de Insumos (DMET e DMEE)

Calcula a Dose de Máxima Eficiência Técnica (DMET) e a Dose de Máxima
Eficiência Econômica (DMEE) para modelos de regressão quadrática (Y =
b0 + b1*X + b2*X^2).

## Usage

``` r
otimizacao_insumos(
  modelo_quadratico,
  preco_insumo,
  preco_produto,
  verbose = TRUE
)
```

## Arguments

- modelo_quadratico:

  Objeto de resultado da função `ajustar_quadratico`.

- preco_insumo:

  Preço unitário do insumo (dose).

- preco_produto:

  Preço unitário do produto (resposta).

- verbose:

  Lógico. Gera prints detalhados (padrão: TRUE).

## Value

Data frame com os resultados de otimização (DMET, Y_max, DMEE, Y_DMEE)
para cada resposta.

## Examples

``` r
if (FALSE) { # \dontrun{
dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 18, 24, 28, 30, 31)

# Ajusta modelo quadrático
modelo_quad <- ajustar_quadratico(dose, Y = resposta, verbose = FALSE)

# Calcula DMET e DMEE
otimizacao <- otimizacao_insumos(
  modelo_quadratico = modelo_quad,
  preco_insumo = 10,
  preco_produto = 50,
  verbose = TRUE
)

print(otimizacao)
} # }
```
