#' Escolha do Modelo Adequado
#'
#' Consolida e compara os resultados de diferentes modelos de regressão (Linear, Quadrático, etc.)
#' com base em métricas de ajuste como AIC, BIC e R².
#'
#' @param ... Resultados de ajuste de modelos (listas retornadas pelas funções `ajustar_*`).
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Data frame consolidado com os resultados de AIC, BIC e R² para todos os modelos e respostas.
#'
#' @importFrom dplyr "%>%"
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' dose <- c(0, 50, 100, 150, 200, 250)
#' resposta <- c(10, 18, 24, 28, 30, 31)
#'
#' # Ajusta diferentes modelos
#' modelo_linear <- ajustar_linear(dose, Y = resposta, verbose = FALSE)
#' modelo_quad <- ajustar_quadratico(dose, Y = resposta, verbose = FALSE)
#' modelo_cubico <- ajustar_cubico(dose, Y = resposta, verbose = FALSE)
#'
#' # Compara os modelos
#' comparacao <- escolha_modelo_adequado(
#'   modelo_linear,
#'   modelo_quad,
#'   modelo_cubico,
#'   verbose = TRUE
#' )
#'
#' print(comparacao)
#' }
#'
#' @export
escolha_modelo_adequado <- function(..., verbose = TRUE) {
  # Evitar aviso de "no visible binding"
  parametro <- resposta <- valor <- Modelo <- R2 <- AIC <- BIC <- NULL

  modelos_list <- list(...)

  if (length(modelos_list) == 0) {
    stop("Nenhum resultado de modelo foi fornecido.")
  }

  # Extrai os nomes dos modelos (baseado no nome da variável passada)
  modelos_nomes <- as.character(match.call(expand.dots = FALSE)$`...`)

  resultados_consolidados <- list()

  for (i in 1:length(modelos_list)) {
    modelo_resultado <- modelos_list[[i]]
    modelo_nome <- modelos_nomes[i]

    # Verifica se o resultado tem a estrutura esperada
    if (
      !is.list(modelo_resultado) || !("resultados" %in% names(modelo_resultado))
    ) {
      warning(sprintf(
        "O objeto '%s' não tem a estrutura de resultado esperada e será ignorado.",
        modelo_nome
      ))
      next
    }

    df_modelo <- modelo_resultado$resultados

    # Filtra apenas as métricas de interesse
    metricas_interesse <- c("R2", "AIC", "BIC")

    df_filtrado <- df_modelo %>%
      dplyr::filter(parametro %in% metricas_interesse) %>%
      dplyr::select(resposta, parametro, valor) %>%
      tidyr::pivot_wider(names_from = parametro, values_from = valor) %>%
      dplyr::mutate(Modelo = modelo_nome)

    resultados_consolidados[[modelo_nome]] <- df_filtrado
  }

  if (length(resultados_consolidados) == 0) {
    stop("Nenhum resultado de modelo válido foi encontrado para comparação.")
  }

  df_final <- do.call(rbind, resultados_consolidados) %>%
    dplyr::select(resposta, Modelo, R2, AIC, BIC) %>%
    dplyr::arrange(resposta, AIC) # Ordena por resposta e AIC (menor AIC é melhor)

  # Impressão Verbosa
  if (verbose) {
    cat("\n=== Escolha do Modelo Adequado (Comparação de Métricas) ===\n")

    respostas_unicas <- unique(df_final$resposta)

    for (resp in respostas_unicas) {
      df_resp <- df_final %>% dplyr::filter(resposta == resp)

      cat(sprintf("\n--- Resposta: %s ---\n", resp))

      # Encontra o melhor modelo para cada métrica
      best_r2 <- df_resp %>%
        dplyr::filter(R2 == max(R2, na.rm = TRUE)) %>%
        dplyr::pull(Modelo) %>%
        unique()
      best_aic <- df_resp %>%
        dplyr::filter(AIC == min(AIC, na.rm = TRUE)) %>%
        dplyr::pull(Modelo) %>%
        unique()
      best_bic <- df_resp %>%
        dplyr::filter(BIC == min(BIC, na.rm = TRUE)) %>%
        dplyr::pull(Modelo) %>%
        unique()

      cat(sprintf("Melhor R² (Ajuste): %s\n", paste(best_r2, collapse = ", ")))
      cat(sprintf(
        "Melhor AIC (Parcimônia): %s\n",
        paste(best_aic, collapse = ", ")
      ))
      cat(sprintf(
        "Melhor BIC (Parcimônia Rigorosa): %s\n",
        paste(best_bic, collapse = ", ")
      ))

      # Tabela de resultados
      print(df_resp)
    }
  }

  return(df_final)
}
