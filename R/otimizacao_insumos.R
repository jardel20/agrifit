#' Otimização de Uso de Insumos (DMET e DMEE)
#'
#' Calcula a Dose de Máxima Eficiência Técnica (DMET) e a Dose de Máxima Eficiência
#' Econômica (DMEE) para modelos de regressão quadrática (Y = b0 + b1*X + b2*X^2).
#'
#' @param modelo_quadratico Objeto de resultado da função `ajustar_quadratico`.
#' @param preco_insumo Preço unitário do insumo (dose).
#' @param preco_produto Preço unitário do produto (resposta).
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Data frame com os resultados de otimização (DMET, Y_max, DMEE, Y_DMEE) para cada resposta.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
otimizacao_insumos <- function(
  modelo_quadratico,
  preco_insumo,
  preco_produto,
  verbose = TRUE
) {
  # Evitar aviso de "no visible binding"
  resposta <- parametro <- valor <- NULL

  if (
    !is.list(modelo_quadratico) || !("resultados" %in% names(modelo_quadratico))
  ) {
    stop("O objeto fornecido não é um resultado válido da função de ajuste.")
  }

  if (preco_insumo <= 0 || preco_produto <= 0) {
    stop("Os preços do insumo e do produto devem ser maiores que zero.")
  }

  df_resultados <- modelo_quadratico$resultados
  nomes_respostas <- unique(df_resultados$resposta)

  resultados_otimizacao <- list()

  for (resp in nomes_respostas) {
    # Extrai os coeficientes do modelo quadrático
    coefs_resp <- df_resultados %>%
      dplyr::filter(resposta == resp) %>%
      tidyr::pivot_wider(names_from = parametro, values_from = valor)

    # Verifica se o modelo é quadrático (b2 existe)
    if (!("b2_quadratico" %in% names(coefs_resp))) {
      warning(sprintf(
        "A resposta '%s' não tem os coeficientes quadráticos esperados. Otimização ignorada.",
        resp
      ))
      next
    }

    b0 <- coefs_resp$b0_intercepto
    b1 <- coefs_resp$b1_linear
    b2 <- coefs_resp$b2_quadratico

    # --- 1. Dose de Máxima Eficiência Técnica (DMET) ---
    # DMET = -b1 / (2 * b2)
    # Requer b2 < 0 (curva côncava para baixo)
    if (b2 >= 0) {
      DMET <- NA
      Y_max <- NA
      aviso_dmet <- "Curva convexa (b2 >= 0). DMET não calculável."
    } else {
      DMET <- -b1 / (2 * b2)

      # Verifica se DMET é positiva (dose válida)
      if (DMET < 0) {
        DMET <- 0
        Y_max <- b0 # Se o máximo é negativo, o máximo na faixa positiva é 0
        aviso_dmet <- "DMET negativa. Máximo na dose 0."
      } else {
        # Y_max = b0 + b1*DMET + b2*DMET^2
        Y_max <- b0 + b1 * DMET + b2 * DMET^2
        aviso_dmet <- "DMET calculada com sucesso."
      }
    }

    # --- 2. Dose de Máxima Eficiência Econômica (DMEE) ---
    # DMEE = (b1 - (preco_insumo / preco_produto)) / (-2 * b2)
    # Requer b2 < 0

    relacao_precos <- preco_insumo / preco_produto

    if (b2 >= 0) {
      DMEE <- NA
      Y_DMEE <- NA
      aviso_dmee <- "Curva convexa (b2 >= 0). DMEE não calculável."
    } else {
      DMEE <- (b1 - relacao_precos) / (-2 * b2)

      # Verifica se DMEE é positiva (dose válida)
      if (DMEE < 0) {
        DMEE <- 0
        Y_DMEE <- b0
        aviso_dmee <- "DMEE negativa. Máximo econômico na dose 0."
      } else {
        # Y_DMEE = b0 + b1*DMEE + b2*DMEE^2
        Y_DMEE <- b0 + b1 * DMEE + b2 * DMEE^2
        aviso_dmee <- "DMEE calculada com sucesso."
      }
    }

    # Armazenamento
    resultados_otimizacao[[resp]] <- data.frame(
      resposta = resp,
      DMET = DMET,
      Y_max = Y_max,
      DMEE = DMEE,
      Y_DMEE = Y_DMEE,
      Relacao_Precos = relacao_precos,
      Aviso_DMET = aviso_dmet,
      Aviso_DMEE = aviso_dmee
    )
  }

  df_final <- do.call(rbind, resultados_otimizacao)

  # Impressão Verbosa
  if (verbose) {
    cat("\n=== Otimização de Uso de Insumos (DMET e DMEE) ===\n")
    cat(sprintf("Relação de Preços (Insumo/Produto): %.4f\n\n", relacao_precos))
    print(df_final)
  }

  return(df_final)
}
