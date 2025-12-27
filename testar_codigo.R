X1_N <- c(-1, -1, -1, 0, 0, 0, 1, 1, 1) # Nitrogênio (codificado)
X2_P <- c(-1, 0, 1, -1, 0, 1, -1, 0, 1) # Fósforo (codificado)
Produtividade <- c(
  4500, 5200, 5800, # kg/ha milho
  4800, 6500, 7200,
  5100, 6100, 6800
)

superficie <- analise_superficie_resposta(
  X1 = X1_N, X2 = X2_P,
  Milho_kg_ha = Produtividade,
  verbose = TRUE
)

otimizacao <- otimizacao_superficie_matrizes(
  modelo_superficie = superficie,
  preco_insumo_X1 = 3.0, # R$/kg N (Ureia)
  preco_insumo_X2 = 4.0, # R$/kg P (MAP)
  preco_produto = 1.2, # R$/kg milho
  verbose = TRUE
)

dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 18, 26, 29, 31, 32)

resultado <- ajustar_cubico(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose <- c(0, 25, 50, 75, 100, 125)
resposta <- c(10, 12, 15, 20, 28, 40)

resultado <- ajustar_exponencial(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(15, 18, 20, 23, 29, 35)

resultado <- ajustar_linear(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose_P <- c(0.0, 32.5, 65.0, 97.5, 130.0, 195.0, 260.0, 325.0)
MS <- c(6.74, 8.73, 10.89, 12.56, 14.11, 15.21, 15.50, 15.60)

res <- ajustar_LRP(
  dose = dose_P, MS = MS,
  title = "Matéria Seca vs Dose de P",
  xlab = "P (mg/dm³)", ylab = "MS (g/vaso)",
  adjustment_color = "Dark2"
)

dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(5, 12, 18, 22, 25, 27)

resultado <- ajustar_mitscherlich(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose <- c(1, 10, 20, 50, 100, 200)
resposta <- c(5, 12, 18, 28, 38, 50)

resultado <- ajustar_potencial(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 18, 24, 28, 25, 17)

resultado <- ajustar_quadratico(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose <- c(0, 25, 50, 100, 150, 200)
resposta <- c(100, 160, 200, 260, 300, 330)

resultado <- ajustar_raiz_quadratica(
  dose = dose,
  Y = resposta,
  verbose = TRUE
)

dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(100, 180, 240, 280, 300, 310)

modelo_quad <- ajustar_quadratico(dose = dose, Y = resposta, verbose = T)

otimizacao <- otimizacao_insumos(
  modelo_quadratico = modelo_quad,
  preco_insumo = 200,
  preco_produto = 500,
  verbose = TRUE
)

dose <- c(0, 50, 100, 150, 200, 250)
resposta <- c(10, 18, 24, 28, 30, 31)

# Ajusta diferentes modelos
linear <- ajustar_linear(dose, MS = resposta, verbose = FALSE)
quad <- ajustar_quadratico(dose, MS = resposta, verbose = FALSE)
cubico <- ajustar_cubico(dose, MS = resposta, verbose = FALSE)
cubico2 <- ajustar_exponencial(dose, MS = resposta, verbose = FALSE)
cubico3 <- ajustar_potencial(dose, MS = resposta, verbose = FALSE)
# Compara os modelos
comparacao <- escolha_modelo_adequado(
  linear,
  quad,
  cubico,
  cubico2,
  cubico3,
  verbose = TRUE,
  criterio = "multiplo",
  layout_grid = "auto",
  layout_barras = "linha"
)
