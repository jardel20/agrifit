#' Escolha do Modelo Adequado (RMSE + Critério Flexível + Barras Arranjadas)
#'
#' Compara modelos por R², AIC, BIC, RMSE com gráficos de barras ARRANJADOS.
#'
#' @param ... Resultados de modelos.
#' @param criterio String: "AIC", "BIC", "RMSE", "R2" ou "multiplo" (padrão).
#' @param layout_grid "linha", "coluna", "grade", "auto" para gráficos dos modelos.
#' @param layout_barras "linha", "coluna", "grade", "auto" para barras de comparação.
#' @param verbose Lógico.
#'
#' @return Lista completa com todos os resultados e gráficos.
#'
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal coord_flip facet_wrap labs
#' @export
escolha_modelo_adequado <- function(...,
                                    criterio = "multiplo",
                                    layout_grid = "auto",
                                    layout_barras = "auto",
                                    verbose = TRUE) {
  # Declarações para R CMD check
  parametro <- resposta <- valor <- Modelo <- R2 <- AIC <- BIC <- RMSE <- NULL

  modelos_list <- list(...)
  modelos_nomes <- as.character(match.call(expand.dots = FALSE)$`...`)

  resultados_consolidados <- list()
  graficos_modelos <- list()

  # 1. EXTRAI MÉTRICAS + GRÁFICOS DOS MODELOS
  for (i in 1:length(modelos_list)) {
    modelo_resultado <- modelos_list[[i]]
    modelo_nome <- modelos_nomes[i]

    if (!is.list(modelo_resultado) || !("resultados" %in% names(modelo_resultado))) {
      warning(sprintf("'%s' inválido. Ignorado.", modelo_nome))
      next
    }

    df_modelo <- modelo_resultado$resultados
    df_filtrado <- df_modelo %>%
      dplyr::filter(parametro %in% c("R2", "AIC", "BIC", "RMSE")) %>%
      dplyr::select(resposta, parametro, valor) %>%
      tidyr::pivot_wider(names_from = parametro, values_from = valor) %>%
      dplyr::mutate(Modelo = modelo_nome)

    resultados_consolidados[[modelo_nome]] <- df_filtrado

    if ("graficos" %in% names(modelo_resultado)) {
      graficos_modelos[[modelo_nome]] <- modelo_resultado$graficos
    }
  }

  df_final <- do.call(rbind, resultados_consolidados) %>%
    dplyr::select(resposta, Modelo, R2, AIC, BIC, RMSE) %>%
    dplyr::arrange(resposta, Modelo)

  # 2. MELHOR MODELO POR CRITÉRIO
  melhor_modelo <- df_final %>%
    dplyr::group_by(resposta) %>%
    dplyr::slice(ifelse(criterio == "R2",
      dplyr::n(),
      which.min(ifelse(criterio == "R2", -R2,
        ifelse(criterio == "AIC", AIC,
          ifelse(criterio == "BIC", BIC, RMSE)
        )
      ))
    )) %>%
    dplyr::ungroup()

  # 3. GRÁFICOS DOS MODELOS ARRANJADOS
  graficos_modelos_arranjados <- NULL
  if (length(graficos_modelos) > 0) {
    graficos_lista <- unlist(graficos_modelos, recursive = FALSE)
    n_graficos <- length(graficos_lista)

    if (layout_grid == "linha") {
      graficos_modelos_arranjados <- patchwork::wrap_plots(graficos_lista, ncol = 1)
    } else if (layout_grid == "coluna") {
      graficos_modelos_arranjados <- patchwork::wrap_plots(graficos_lista, nrow = 1)
    } else if (layout_grid == "grade") {
      ncol_grid <- ceiling(sqrt(n_graficos))
      graficos_modelos_arranjados <- patchwork::wrap_plots(graficos_lista, ncol = ncol_grid)
    } else { # auto
      graficos_modelos_arranjados <- patchwork::wrap_plots(graficos_lista)
    }
  }

  # 4. GRÁFICOS DE COMPARAÇÃO INDIVIDUAIS
  p_aic <- df_final %>%
    dplyr::group_by(resposta) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(Modelo, AIC), y = AIC, fill = Modelo)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "AIC (menor = melhor)", x = "Modelo", y = "AIC") +
    ggplot2::facet_wrap(~resposta, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  p_bic <- df_final %>%
    dplyr::group_by(resposta) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(Modelo, BIC), y = BIC, fill = Modelo)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "BIC (menor = melhor)", x = "Modelo", y = "BIC") +
    ggplot2::facet_wrap(~resposta, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  p_rmse <- df_final %>%
    dplyr::group_by(resposta) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(Modelo, RMSE), y = RMSE, fill = Modelo)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "RMSE (menor = melhor)", x = "Modelo", y = "RMSE") +
    ggplot2::facet_wrap(~resposta, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  p_r2 <- df_final %>%
    dplyr::group_by(resposta) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(Modelo, -R2), y = R2, fill = Modelo)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "R² (maior = melhor)", x = "Modelo", y = "R²") +
    ggplot2::facet_wrap(~resposta, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  # 5. GRÁFICOS DE BARRAS ARRANJADOS (MESMA LÓGICA!)
  graficos_barras_arranjados <- NULL
  graficos_barras_lista <- list(AIC = p_aic, BIC = p_bic, RMSE = p_rmse, R2 = p_r2)
  n_barras <- length(graficos_barras_lista)

  if (n_barras > 0) {
    if (layout_barras == "linha") {
      graficos_barras_arranjados <- patchwork::wrap_plots(graficos_barras_lista, ncol = 1)
    } else if (layout_barras == "coluna") {
      graficos_barras_arranjados <- patchwork::wrap_plots(graficos_barras_lista, nrow = 1)
    } else if (layout_barras == "grade") {
      ncol_barras <- ceiling(sqrt(n_barras))
      graficos_barras_arranjados <- patchwork::wrap_plots(graficos_barras_lista, ncol = ncol_barras)
    } else { # auto
      graficos_barras_arranjados <- patchwork::wrap_plots(graficos_barras_lista)
    }
  }

  # 6. IMPRESSÃO VERBOSA
  if (verbose) {
    cat(sprintf("\n=== ESCOLHA DO MELHOR MODELO (Critério: %s) ===\n", criterio))

    cat(sprintf("\nBARRAS DE COMPARAÇÃO (layout_barras=%s):\n", layout_barras))
    print(graficos_barras_arranjados)
    cat("\n")

    cat(sprintf("\nMODELOS (layout_grid=%s):\n", layout_grid))
    if (!is.null(graficos_modelos_arranjados)) print(graficos_modelos_arranjados)

    cat("\n📋 TABELA:\n")
    print(df_final %>% dplyr::arrange(resposta, AIC))

    cat(sprintf("\nMELHOR MODELO:\n"))
    print(melhor_modelo)
  }

  return(list(
    comparacao = df_final,
    melhor_modelo = melhor_modelo,
    graficos_modelos = graficos_modelos_arranjados,
    graficos_barras = graficos_barras_arranjados,
    graficos_individuais = graficos_barras_lista
  ))
}
