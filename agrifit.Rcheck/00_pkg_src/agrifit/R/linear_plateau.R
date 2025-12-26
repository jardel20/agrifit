#' Ajusta o Modelo Linear Descontínuo (LRP)
#'
#' Implementa o ajuste iterativo do modelo Linear Response Plateau (LRP), testando
#' cada ponto de dose como potencial breakpoint e selecionando o modelo com maior R².
#' O LRP combina um segmento linear (\eqn{\hat{Y} = b_0 + b_1 X}) com plateau
#' (\eqn{\hat{Y} = b_2}), onde \eqn{X_i = (b_2 - b_0) / b_1} define a transição.
#' @param dose Vetor numérico com níveis do fator (X). Mínimo 4 pontos distintos.
#' @param ... Uma ou mais respostas (vetores Y nomeados). Exemplo: `MS = materia_seca`.
#' @param title Título do gráfico (padrão: "Ajuste do Modelo Linear Descontínuo (LRP)").
#' @param xlab Rótulo eixo X (padrão: "Dose").
#' @param ylab Rótulo eixo Y (padrão: "Resposta").
#' @param adjustment_color Paleta RColorBrewer para curvas (padrão: "Set1").
#' @param legend_position_ggplot Posição da legenda no ggplot (padrão: "top").
#' @param dashed Lógico. Se TRUE, adiciona linhas tracejadas nos breakpoints (padrão: FALSE).
#' @param show_intersection Lógico. Mostra ponto de intersecção (X) no gráfico (padrão: TRUE).
#' @param verbose Lógico. Gera gráficos e prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: b0, b1, b2, Xi, R², breakpoint para cada resposta.}
#'   \item{modelos}{Lista dos melhores modelos por resposta.}
#'   \item{equacoes}{Strings formatadas das equações LRP.}
#'   \item{nomes_respostas}{Nomes das respostas processadas.}
#'   \item{grafico_ggplot}{Gráfico ggplot2 (se verbose=TRUE).}
#'   \item{grafico_plotly}{Gráfico plotly interativo (se verbose=TRUE).}
#' }
#'
#' @details
#' **Algoritmo iterativo:**
#' 1. Testa breakpoints de 3 até n-1
#' 2. Regressão linear pré-breakpoint (\code{lm})
#' 3. Média pós-breakpoint como plateau (b2)
#' 4. Calcula Xi e R² total
#' 5. Seleciona melhor R²
#'
#' **Saídas gráficas** (verbose=TRUE):
#' - Pontos observados + curvas ajustadas
#' - Ponto de intersecção opcional (X)
#' - Linhas tracejadas opcionais (breakpoint)
#' - Salva "lrp_multiplo.png" (12x8in, 300dpi)
#'
#' **Dependências:** \code{ggplot2}, \code{plotly}, \code{dplyr}, \code{tidyr}
#'
#' @examples
#' \dontrun{
#' dose_P <- c(0.0, 32.5, 65.0, 97.5, 130.0, 195.0)
#' MS <- c(6.74, 8.73, 10.89, 12.56, 14.11, 15.21)
#'
#' res <- ajustar_lrp(
#'   dose = dose_P, MS = MS,
#'   title = "Matéria Seca vs Dose de P",
#'   xlab = "P (mg/dm³)", ylab = "MS (g/vaso)",
#'   adjustment_color = "Dark2"
#' )
#'
#' print(res$resultados)
#' print(res$equacoes)
#' }
#' @importFrom stats lm coef
#' @importFrom dplyr "%>%"
#'
#' @export

ajustar_lrp <- function(dose, ...,
                        title = "Ajuste do Modelo Linear Descontínuo (LRP)",
                        xlab = "Dose",
                        ylab = "Resposta",
                        adjustment_color = "Set1",
                        legend_position_ggplot = "top",
                        dashed = FALSE,
                        show_intersection = TRUE,
                        verbose = TRUE) {
  valor <- NULL

  # Carregamento de bibliotecas
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("O pacote 'ggplot2' é necessário.")
  if (!requireNamespace("plotly", quietly = TRUE)) stop("O pacote 'plotly' é necessário.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("O pacote 'dplyr' é necessário.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("O pacote 'tidyr' é necessário.")

  # Captura todas as variáveis resposta via "..."
  respostas_list <- list(...)
  n_respostas <- length(respostas_list)
  nomes_respostas <- names(respostas_list)

  # Validação
  if (length(dose) < 4) {
    stop("Dose deve ter mínimo 4 pontos.")
  }
  if (n_respostas == 0) {
    stop("Pelo menos uma variável resposta deve ser fornecida.")
  }

  # Padroniza nomes se não fornecidos
  if (is.null(nomes_respostas) || any(nomes_respostas == "")) {
    nomes_respostas <- paste0("resposta", 1:n_respostas)
    names(respostas_list) <- nomes_respostas
  }

  n <- length(dose)
  resultados_all <- list()
  melhores_modelos <- list()
  equacoes <- list()

  # Loop principal para cada resposta
  for (i in 1:n_respostas) {
    resposta <- respostas_list[[i]]
    nome_resposta <- nomes_respostas[i]

    # Data frame para esta resposta
    data <- data.frame(X = dose, Y = resposta)
    SST <- sum((resposta - mean(resposta))^2)

    # Inicialização
    best_r2 <- -Inf
    best_model <- NULL

    # Itera breakpoints
    for (k in 3:(n - 1)) {
      breakpoint_X <- data$X[k]
      data_linear <- data[1:(k - 1), ]
      data_plateau <- data[k:n, ]

      # 1. Ajuste do Segmento Linear
      lm_fit <- lm(Y ~ X, data = data_linear)
      b0 <- coef(lm_fit)[1]
      b1 <- coef(lm_fit)[2]

      # 2. Ajuste do Segmento Plateau
      b2 <- mean(data_plateau$Y)

      # 3. Cálculo do Ponto de Interseção (Xi)
      if (is.na(b1) || b1 <= 0) next
      Xi <- (b2 - b0) / b1

      # 4. Previsão e Cálculo do R² Total
      predict_lrp <- function(X_val) {
        ifelse(X_val < Xi, b0 + b1 * X_val, b2)
      }
      Y_hat <- predict_lrp(data$X)
      SSE <- sum((data$Y - Y_hat)^2)
      R2 <- 1 - (SSE / SST)

      if (R2 > best_r2) {
        best_r2 <- R2
        best_model <- list(
          b0 = b0, b1 = b1, b2 = b2, Xi = Xi, R2 = R2,
          breakpoint_index = k, breakpoint_X_data = breakpoint_X
        )
      }
    }

    # Armazena resultados
    if (!is.null(best_model)) {
      resultado_df <- data.frame(
        resposta = nome_resposta,
        parametro = c(
          "b0_intercepto", "b1_declividade", "b2_plateau",
          "Xi_interseccao", "R2", "breakpoint_indice", "breakpoint_X"
        ),
        valor = c(
          best_model$b0, best_model$b1, best_model$b2, best_model$Xi,
          best_model$R2, best_model$breakpoint_index, best_model$breakpoint_X_data
        ),
        stringsAsFactors = FALSE
      )
      equacao <- sprintf(
        "%s: Ŷ = %.4f + %.4fX (X < %.4f); Ŷ = %.4f",
        nome_resposta, best_model$b0, best_model$b1, best_model$Xi, best_model$b2
      )
    } else {
      resultado_df <- data.frame(
        resposta = nome_resposta, parametro = "erro",
        valor = "Nao_foi_possivel_ajustar", stringsAsFactors = FALSE
      )
      equacao <- paste(nome_resposta, ": Não ajustado")
      best_model <- NULL
    }

    resultados_all[[nome_resposta]] <- resultado_df
    melhores_modelos[[nome_resposta]] <- best_model
    equacoes[[i]] <- equacao
  }

  # Combina todos os dataframes
  resultado_df_final <- do.call(rbind, resultados_all)

  # === GRÁFICO COM GGPLOT2 + PLOTLY ===
  if (verbose) {
    # Prepara dados para ggplot
    todas_respostas <- do.call(cbind, respostas_list)
    dados_plot <- data.frame(dose = dose, todas_respostas)
    dados_plot_long <- dados_plot %>%
      tidyr::pivot_longer(-dose, names_to = "resposta", values_to = "valor")

    # Cria curvas de predição e dados para o ponto de intersecção
    curvas_data <- data.frame()
    interseccoes_data <- data.frame()

    for (i in 1:n_respostas) {
      model <- melhores_modelos[[i]]
      nome <- nomes_respostas[i]
      if (!is.null(model)) {
        # Dados para a curva de ajuste
        X_curve <- seq(min(dose), max(dose), length.out = 100)
        predict_curve <- function(X_val) {
          ifelse(X_val < model$Xi, model$b0 + model$b1 * X_val, model$b2)
        }
        Y_curve <- predict_curve(X_curve)
        curva_df <- data.frame(
          dose = X_curve, valor = Y_curve,
          resposta = nome, tipo = "curva"
        )
        curvas_data <- rbind(curvas_data, curva_df)

        # Dados para o ponto de intersecção (Xi, b2)
        interseccao_df <- data.frame(
          dose = model$Xi, valor = model$b2,
          resposta = nome
        )
        interseccoes_data <- rbind(interseccoes_data, interseccao_df)
      }
    }

    # GRÁFICO PRINCIPAL GGPLOT
    p_main <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = dados_plot_long, ggplot2::aes(x = dose, y = valor, color = resposta),
        size = 3, alpha = 0.8
      ) +
      ggplot2::geom_line(
        data = curvas_data, ggplot2::aes(x = dose, y = valor, color = resposta),
        linewidth = 1.0, alpha = 0.9
      ) +
      # PONTO DE INTERSECÇÃO CONDICIONAL
      `if`(
        show_intersection,
        ggplot2::geom_point(
          data = interseccoes_data, ggplot2::aes(x = dose, y = valor, color = resposta),
          shape = 4, size = 4, stroke = 0.5
        ),
        NULL
      ) +
      # TRACEJADO CONDICIONAL
      `if`(
        dashed,
        list(
          ggplot2::geom_vline(
            data = interseccoes_data, ggplot2::aes(xintercept = dose, color = resposta),
            linetype = "dashed", alpha = 0.5
          ),
          ggplot2::geom_hline(
            data = interseccoes_data, ggplot2::aes(yintercept = valor, color = resposta),
            linetype = "dashed", alpha = 0.5
          )
        ),
        NULL
      ) +
      ggplot2::scale_color_brewer(palette = adjustment_color, name = "Response") +
      ggplot2::labs(title = title, x = xlab, y = ylab) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = legend_position_ggplot,
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 11)
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4)))

    # SALVA PNG
    ggplot2::ggsave("lrp_multiplo.png", p_main, width = 12, height = 8, dpi = 300)

    # PLOTLY INTERATIVO
    p_interativo <- plotly::ggplotly(p_main, tooltip = c("x", "y", "color")) %>%
      plotly::layout(legend = list(orientation = "h", x = 0.5, y = 1.02, xanchor = "center"))

    # MOSTRA NA TELA
    print(p_interativo)
  }

  # IMPRESSÃO VERBOSA
  if (verbose) {
    cat("\n=== Ajuste LRP Múltiplo | LRP - Linear Response Plateau ===\n")
    cat(sprintf(
      "Dose: %d pontos | Respostas: %d (%s)\n\n",
      length(dose), n_respostas, paste(nomes_respostas, collapse = ", ")
    ))

    for (i in 1:n_respostas) {
      model <- melhores_modelos[[i]]
      nome <- nomes_respostas[i]
      eq <- equacoes[[i]]

      cat(sprintf("------------------------ %s ------------------------\n", nome))
      if (!is.null(model)) {
        cat(sprintf("R²: %.4f\n", model$R2))
        cat(sprintf("Ponto de Intersecção (Xi): %.4f\n", model$Xi))
        cat(sprintf("Ponto de Quebra na Tabela (Xk): %.4f (idx %d)\n", model$breakpoint_X_data, model$breakpoint_index))
        cat("Equação:\n", eq, "\n")
      } else {
        cat("❌ Não foi possível ajustar\n")
      }
      cat("\n")
    }
    cat("Gráfico salvo como: 'lrp_multiplo.png'\n")
  }

  # RETORNO
  return(list(
    resultados = resultado_df_final,
    modelos = melhores_modelos,
    equacoes = equacoes,
    nomes_respostas = nomes_respostas,
    grafico_ggplot = if (verbose) p_main else NULL,
    grafico_plotly = if (verbose) p_interativo else NULL
  ))
}
