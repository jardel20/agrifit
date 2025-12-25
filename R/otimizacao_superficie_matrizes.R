#' Otimização de Superfície de Respostas por Álgebra de Matrizes (DMEE)
#'
#' Calcula a Dose de Máxima Eficiência Econômica (DMEE) para o modelo quadrático
#' de Superfície de Resposta (2 Fatores) usando Álgebra de Matrizes.
#'
#' @param modelo_superficie Objeto de resultado da função `analise_superficie_resposta`.
#' @param preco_insumo_X1 Preço unitário do insumo X1.
#' @param preco_insumo_X2 Preço unitário do insumo X2.
#' @param preco_produto Preço unitário do produto (resposta).
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Data frame com os resultados de otimização (X1_DMEE, X2_DMEE, Y_DMEE) para cada resposta.
#'
#' @importFrom dplyr "%>%"
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' X1 <- c(-1, -1, 1, 1, 0, 0, 0, 0, 0)
#' X2 <- c(-1, 1, -1, 1, 0, 0, 0, 0, 0)
#' prod <- c(50, 60, 55, 70, 65, 65, 65, 66, 64)
#'
#' # Ajusta modelo de superfície
#' superficie <- analise_superficie_resposta(
#'   X1 = X1, X2 = X2,
#'   Producao = prod,
#'   verbose = FALSE
#' )
#'
#' # Calcula DMEE
#' otimizacao <- otimizacao_superficie_matrizes(
#'   modelo_superficie = superficie,
#'   preco_insumo_X1 = 20,
#'   preco_insumo_X2 = 25,
#'   preco_produto = 100,
#'   verbose = TRUE
#' )
#'
#' print(otimizacao)
#' }
#'
#' @export
otimizacao_superficie_matrizes <- function(
  modelo_superficie,
  preco_insumo_X1,
  preco_insumo_X2,
  preco_produto,
  verbose = TRUE
) {
  # Evitar aviso de "no visible binding"
  resposta <- parametro <- valor <- NULL

  if (
    !is.list(modelo_superficie) || !("resultados" %in% names(modelo_superficie))
  ) {
    stop(
      "O objeto fornecido não é um resultado válido da função de ajuste de superfície de resposta."
    )
  }

  if (preco_insumo_X1 <= 0 || preco_insumo_X2 <= 0 || preco_produto <= 0) {
    stop("Os preços dos insumos e do produto devem ser maiores que zero.")
  }

  df_resultados <- modelo_superficie$resultados
  nomes_respostas <- unique(df_resultados$resposta)

  resultados_otimizacao <- list()

  for (resp in nomes_respostas) {
    # Extrai os coeficientes do modelo quadrático completo
    coefs_resp <- df_resultados %>%
      dplyr::filter(resposta == resp) %>%
      tidyr::pivot_wider(names_from = parametro, values_from = valor)

    # Verifica se o modelo é quadrático completo (b12 existe)
    if (!("b12_interacao" %in% names(coefs_resp))) {
      warning(sprintf(
        "A resposta '%s' não tem os coeficientes de superfície de resposta esperados. Otimização ignorada.",
        resp
      ))
      next
    }

    # Coeficientes
    b1 <- coefs_resp$b1_linear_X1
    b2 <- coefs_resp$b2_linear_X2
    b11 <- coefs_resp$b11_quad_X1
    b22 <- coefs_resp$b22_quad_X2
    b12 <- coefs_resp$b12_interacao

    # Relação de Preços (Custo Marginal / Receita Marginal)
    R1 <- preco_insumo_X1 / preco_produto
    R2 <- preco_insumo_X2 / preco_produto

    # --- Cálculo da DMEE por Álgebra de Matrizes ---
    # O sistema de equações para o máximo econômico é:
    # dY/dX1 = b1 + 2*b11*X1 + b12*X2 = R1
    # dY/dX2 = b2 + 2*b22*X2 + b12*X1 = R2

    # Reorganizando:
    # (2*b11)*X1 + (b12)*X2 = R1 - b1
    # (b12)*X1 + (2*b22)*X2 = R2 - b2

    # Matriz A (Coeficientes das variáveis X1 e X2)
    A <- matrix(c(2 * b11, b12, b12, 2 * b22), nrow = 2, byrow = TRUE)

    # Vetor B (Termos independentes)
    B <- matrix(c(R1 - b1, R2 - b2), nrow = 2)

    # Ponto de Máximo Econômico (X1_DMEE, X2_DMEE)
    tryCatch(
      {
        X_dmee <- solve(A, B)
        X1_dmee <- X_dmee[1]
        X2_dmee <- X_dmee[2]

        # Y_DMEE (Resposta no ponto de DMEE)
        Y_dmee <- coefs_resp$b0_intercepto +
          b1 * X1_dmee +
          b2 * X2_dmee +
          b11 * X1_dmee^2 +
          b22 * X2_dmee^2 +
          b12 * X1_dmee * X2_dmee

        aviso <- "DMEE calculada com sucesso."

        # Ajuste para doses negativas (se a DMEE for negativa, a dose recomendada é 0)
        if (X1_dmee < 0) {
          X1_dmee <- 0
          aviso <- paste(aviso, "X1_DMEE ajustado para 0.")
        }
        if (X2_dmee < 0) {
          X2_dmee <- 0
          aviso <- paste(aviso, "X2_DMEE ajustado para 0.")
        }
      },
      error = function(e) {
        X1_dmee <- NA
        X2_dmee <- NA
        Y_dmee <- NA
        aviso <- paste(
          "Cálculo da DMEE falhou (Matriz Singular ou Erro):",
          e$message
        )
      }
    )

    # Armazenamento
    resultados_otimizacao[[resp]] <- data.frame(
      resposta = resp,
      X1_DMEE = X1_dmee,
      X2_DMEE = X2_dmee,
      Y_DMEE = Y_dmee,
      Relacao_Precos_X1 = R1,
      Relacao_Precos_X2 = R2,
      Aviso = aviso
    )
  }

  df_final <- do.call(rbind, resultados_otimizacao)

  # Impressão Verbosa
  if (verbose) {
    cat("\n=== Otimização de Superfície de Respostas (DMEE por Matrizes) ===\n")
    cat(sprintf("Relação de Preços (X1/Produto): %.4f\n", R1))
    cat(sprintf("Relação de Preços (X2/Produto): %.4f\n\n", R2))
    print(df_final)
  }

  return(df_final)
}
