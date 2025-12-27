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
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame com os resultados de otimização (X1_DMEE, X2_DMEE, Y_DMEE) para cada resposta.}
#'   \item{graficos}{Lista de gráficos 3D de superfície de resposta com pontos marcados de DMEE para cada resposta.}
#' }
#'
#' @importFrom dplyr "%>%"
#' @importFrom tidyr pivot_wider
#' @importFrom plotly plot_ly add_trace layout
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
  graficos_otimizacao <- list()

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
    b1 <- as.numeric(coefs_resp$b1_linear_X1)
    b2 <- as.numeric(coefs_resp$b2_linear_X2)
    b11 <- as.numeric(coefs_resp$b11_quad_X1)
    b22 <- as.numeric(coefs_resp$b22_quad_X2)
    b12 <- as.numeric(coefs_resp$b12_interacao)

    # Substituir NA por 0 para cálculos
    if (is.na(b1)) {
      b1 <- 0
    }
    if (is.na(b2)) {
      b2 <- 0
    }
    if (is.na(b11)) {
      b11 <- 0
    }
    if (is.na(b22)) {
      b22 <- 0
    }
    if (is.na(b12)) {
      b12 <- 0
    }

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
    resultado_dmee <- tryCatch(
      {
        X_dmee <- solve(A, B)
        X1_dmee_temp <- X_dmee[1]
        X2_dmee_temp <- X_dmee[2]

        # Y_DMEE (Resposta no ponto de DMEE)
        b0_val <- as.numeric(coefs_resp$b0_intercepto)
        Y_dmee_temp <- b0_val +
          b1 * X1_dmee_temp +
          b2 * X2_dmee_temp +
          b11 * X1_dmee_temp^2 +
          b22 * X2_dmee_temp^2 +
          b12 * X1_dmee_temp * X2_dmee_temp

        aviso_temp <- "DMEE calculada com sucesso."

        # Ajuste para doses negativas (se a DMEE for negativa, a dose recomendada é 0)
        if (X1_dmee_temp < 0) {
          X1_dmee_temp <- 0
          aviso_temp <- paste(aviso_temp, "X1_DMEE ajustado para 0.")
        }
        if (X2_dmee_temp < 0) {
          X2_dmee_temp <- 0
          aviso_temp <- paste(aviso_temp, "X2_DMEE ajustado para 0.")
        }

        list(
          X1_DMEE = X1_dmee_temp,
          X2_DMEE = X2_dmee_temp,
          Y_DMEE = Y_dmee_temp,
          Aviso = aviso_temp
        )
      },
      error = function(e) {
        list(
          X1_DMEE = NA,
          X2_DMEE = NA,
          Y_DMEE = NA,
          Aviso = paste(
            "Cálculo da DMEE falhou (Matriz Singular ou Erro):",
            e$message
          )
        )
      }
    )

    # Armazenamento
    resultados_otimizacao[[resp]] <- data.frame(
      resposta = resp,
      X1_DMEE = resultado_dmee$X1_DMEE,
      X2_DMEE = resultado_dmee$X2_DMEE,
      Y_DMEE = resultado_dmee$Y_DMEE,
      Relacao_Precos_X1 = R1,
      Relacao_Precos_X2 = R2,
      Aviso = resultado_dmee$Aviso
    )

    # --- Gráfico de Superfície de Resposta 3D com Ponto DMEE ---
    # Obter X1 e X2 originais
    X1_orig <- df_resultados %>%
      dplyr::filter(resposta == resp, parametro == "b0_intercepto") %>%
      dplyr::pull(valor) %>%
      as.numeric()

    # Se não conseguirmos, estimamos uma faixa baseada nos coeficientes
    if (length(X1_orig) == 0) {
      X1_range <- seq(-2, 2, length.out = 30)
      X2_range <- seq(-2, 2, length.out = 30)
    } else {
      X1_range <- seq(-2, 2, length.out = 30)
      X2_range <- seq(-2, 2, length.out = 30)
    }

    # Criar malha de pontos
    grid_data_opt <- expand.grid(X1 = X1_range, X2 = X2_range)

    # Obter coeficientes numéricos
    b0_opt <- as.numeric(coefs_resp$b0_intercepto)
    b1_opt <- as.numeric(coefs_resp$b1_linear_X1)
    b2_opt <- as.numeric(coefs_resp$b2_linear_X2)
    b11_opt <- ifelse(is.na(b11), 0, b11)
    b22_opt <- ifelse(is.na(b22), 0, b22)
    b12_opt <- ifelse(is.na(b12), 0, b12)

    # Calcular Z
    grid_data_opt$Z <- b0_opt +
      b1_opt * grid_data_opt$X1 +
      b2_opt * grid_data_opt$X2 +
      b11_opt * grid_data_opt$X1^2 +
      b22_opt * grid_data_opt$X2^2 +
      b12_opt * grid_data_opt$X1 * grid_data_opt$X2

    # Converter para matriz
    Z_matrix_opt <- matrix(
      grid_data_opt$Z,
      nrow = length(X1_range),
      ncol = length(X2_range)
    )

    # Criar gráfico 3D com plotly
    p_opt_3d <- plotly::plot_ly() %>%
      plotly::add_surface(
        x = X1_range, y = X2_range, z = Z_matrix_opt,
        colorscale = "Viridis", showscale = TRUE,
        name = "Superfície", opacity = 0.9
      ) %>%
      plotly::add_markers(
        x = resultado_dmee$X1_DMEE,
        y = resultado_dmee$X2_DMEE,
        z = resultado_dmee$Y_DMEE,
        marker = list(size = 12, color = "gold", symbol = "diamond"),
        name = "DMEE", showlegend = TRUE
      ) %>%
      plotly::layout(
        title = sprintf("Otimização DMEE - %s", resp),
        scene = list(xaxis = list(title = "X1"), yaxis = list(title = "X2"), zaxis = list(title = resp))
      )

    graficos_otimizacao[[resp]] <- p_opt_3d

    if (verbose) {
      print(p_opt_3d)
    }
  }

  df_final <- do.call(rbind, resultados_otimizacao)

  # Impressão Verbosa
  if (verbose) {
    cat("\n=== Otimização de Superfície de Respostas (DMEE por Matrizes) ===\n")
    cat(sprintf("Relação de Preços (X1/Produto): %.4f\n", R1))
    cat(sprintf("Relação de Preços (X2/Produto): %.4f\n\n", R2))

    # Formatação da tabela similar à de ajuste de modelos
    sep_line <- "──────────────────────────────────────────────────────────────────────────────────────"
    header <- "Resposta     X1_DMEE     X2_DMEE     Y_DMEE      Relação_Preços_X1  Relação_Preços_X2"

    cat(sep_line, "\n")
    cat(header, "\n")
    cat(sep_line, "\n")

    # Formatação das linhas
    for (i in 1:nrow(df_final)) {
      row <- df_final[i, ]
      linha <- sprintf(
        "%-12s%-12s%-12s%-12s%-20s%-20s",
        row$resposta,
        sprintf("%.4f", row$X1_DMEE),
        sprintf("%.4f", row$X2_DMEE),
        sprintf("%.4f", row$Y_DMEE),
        sprintf("%.4f", row$Relacao_Precos_X1),
        sprintf("%.4f", row$Relacao_Precos_X2)
      )
      cat(linha, "\n")
    }

    cat(sep_line, "\n\n")

    # Observações com os avisos
    cat("Observações:\n")
    for (i in 1:nrow(df_final)) {
      cat(sprintf("  %s: %s\n", df_final$resposta[i], df_final$Aviso[i]))
    }
  }

  return(list(
    resultados = df_final,
    graficos = graficos_otimizacao
  ))
}
