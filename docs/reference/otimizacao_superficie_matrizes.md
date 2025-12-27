# Otimização de Superfície de Respostas por Álgebra de Matrizes (DMEE)

Calcula a Dose de Máxima Eficiência Econômica (DMEE) para o modelo
quadrático de Superfície de Resposta (2 Fatores) usando Álgebra de
Matrizes.

## Usage

``` r
otimizacao_superficie_matrizes(
  modelo_superficie,
  preco_insumo_X1,
  preco_insumo_X2,
  preco_produto,
  verbose = TRUE
)
```

## Arguments

- modelo_superficie:

  Objeto de resultado da função `analise_superficie_resposta`.

- preco_insumo_X1:

  Preço unitário do insumo X1.

- preco_insumo_X2:

  Preço unitário do insumo X2.

- preco_produto:

  Preço unitário do produto (resposta).

- verbose:

  Lógico. Gera prints detalhados (padrão: TRUE).

## Value

Lista com:

- resultados:

  Data frame com os resultados de otimização (X1_DMEE, X2_DMEE, Y_DMEE)
  para cada resposta.

- graficos:

  Lista de gráficos 3D de superfície de resposta com pontos marcados de
  DMEE para cada resposta.

## Examples

``` r
if (FALSE) { # \dontrun{
X1 <- c(-1, -1, 1, 1, 0, 0, 0, 0, 0)
X2 <- c(-1, 1, -1, 1, 0, 0, 0, 0, 0)
prod <- c(50, 60, 55, 70, 65, 65, 65, 66, 64)

# Ajusta modelo de superfície
superficie <- analise_superficie_resposta(
  X1 = X1, X2 = X2,
  Producao = prod,
  verbose = FALSE
)

# Calcula DMEE
otimizacao <- otimizacao_superficie_matrizes(
  modelo_superficie = superficie,
  preco_insumo_X1 = 20,
  preco_insumo_X2 = 25,
  preco_produto = 100,
  verbose = TRUE
)

print(otimizacao)
} # }
```
