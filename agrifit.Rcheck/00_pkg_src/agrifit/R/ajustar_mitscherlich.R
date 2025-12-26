#' Ajusta o Modelo de Mitscherlich
#'
#' Implementa o ajuste do modelo de Mitscherlich: Y = A - B * c^X.
#' O ajuste é feito usando `nls` (Non-linear Least Squares).
#'
#' @param dose Vetor numérico com níveis do fator (X).
#' @param ... Uma ou mais respostas (vetores Y nomeados). Exemplo: `MS = materia_seca`.
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: A, B, c, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada resposta.}
#'   \item{modelos}{Lista dos objetos `nls` para cada resposta.}
#'   \item{equacoes}{Strings formatadas das equações.}
#'   \item{graficos}{Lista de gráficos ggplot2 com observações e curva ajustada para cada resposta.}
#' }
#'
#' @importFrom stats nls coef pt qt logLik AIC BIC predict
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal element_text
#'
#' @examples
#' \dontrun{
#' dose <- c(0, 50, 100, 150, 200, 250)
#' resposta <- c(5, 12, 18, 22, 25, 27)
#'
#' resultado <- ajustar_mitscherlich(
#'   dose = dose,
#'   Y = resposta,
#'   verbose = TRUE
#' )
#'
#' print(resultado$resultados)
#' print(resultado$equacoes)
#' }
#'
#' @export
ajustar_mitscherlich <- function(dose, ..., verbose = TRUE) {
  # Declaração de variáveis globais para R CMD check
  Y <- tipo <- NULL

  # Funções auxiliares (copiadas da estrutura LRP)
  format_p_value <- function(p) {
    if (is.na(p)) {
      return("NA")
    }
    if (p < 0.001) {
      return(paste(sprintf("%.4f", p), "***"))
    }
    if (p < 0.01) {
      return(paste(sprintf("%.4f", p), "**"))
    }
    if (p < 0.05) {
      return(paste(sprintf("%.4f", p), "*"))
    }
    if (p < 0.1) {
      return(paste(sprintf("%.4f", p), "."))
    }
    return(paste(sprintf("%.4f", p), " "))
  }

  format_coef_table <- function(model) {
    # Extrai estatísticas do summary do nls
    model_summary <- summary(model)
    coef_table <- model_summary$coefficients
    df_res <- model_summary$df[2]

    params <- rownames(coef_table)
    estimates <- coef_table[, 1]
    std_errors <- coef_table[, 2]
    t_values <- coef_table[, 3]
    p_values <- coef_table[, 4]

    # Intervalos de Confiança (95%)
    t_crit <- qt(0.975, df = df_res)
    ic_low <- estimates - t_crit * std_errors
    ic_high <- estimates + t_crit * std_errors

    # Adiciona IC na tabela
    coef_df <- data.frame(
      Parameter = params,
      Estimate = estimates,
      Std.Error = std_errors,
      t.value = t_values,
      Pr.t = p_values,
      IC_Low = ic_low,
      IC_High = ic_high
    )

    # Formatação
    coef_df$Estimate <- sprintf("%.4f", coef_df$Estimate)
    coef_df$Std.Error <- ifelse(
      is.na(coef_df$Std.Error),
      "NA",
      sprintf("%.4f", coef_df$Std.Error)
    )
    coef_df$t.value <- ifelse(
      is.na(coef_df$t.value),
      "NA",
      sprintf("%.2f", coef_df$t.value)
    )
    coef_df$Pr.t <- sapply(coef_df$Pr.t, format_p_value)
    coef_df$IC_Low <- ifelse(
      is.na(coef_df$IC_Low),
      "NA",
      sprintf("%.4f", coef_df$IC_Low)
    )
    coef_df$IC_High <- ifelse(
      is.na(coef_df$IC_High),
      "NA",
      sprintf("%.4f", coef_df$IC_High)
    )

    # Alinhamento e cabeçalho
    sep_line <- "──────────────────────────────────────────────────────────────────────────────────"
    header <- "Parameter         Estimate Std. Error  t value  Pr(>|t|)   IC 95% Low  IC 95% High"
    lines <- apply(coef_df, 1, function(row) {
      sprintf(
        "%-18s%-10s%-12s%-10s%-12s%-12s%-12s",
        row[1],
        row[2],
        row[3],
        row[4],
        row[5],
        row[6],
        row[7]
      )
    })

    # Adiciona a linha de significância
    signif_line <- "──────────────────────────────────────────────────────────────────────────────────
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

    # Junta tudo
    output <- paste(
      "Coefficients:",
      sep_line,
      header,
      sep_line,
      paste(lines, collapse = ""),
      signif_line,
      sep = ""
    )

    return(output)
  }

  # --- Lógica Principal ---
  respostas_list <- list(...)
  n_respostas <- length(respostas_list)
  nomes_respostas <- names(respostas_list)
  n <- length(dose)

  if (n < 3) {
    stop("O modelo de Mitscherlich requer no mínimo 3 pontos de dose.")
  }

  resultados_all <- list()
  modelos_ajustados <- list()
  equacoes <- list()
  graficos <- list()

  for (i in 1:n_respostas) {
    resposta <- respostas_list[[i]]
    nome_resposta <- nomes_respostas[i]
    data <- data.frame(X = dose, Y = resposta)

    # Estimativas Iniciais (Baseado na forma logarítmica)
    # Y = A - B * c^X  =>  A - Y = B * c^X  =>  log(A - Y) = log(B) + X * log(c)
    # A (assíntota) é estimada como o valor máximo de Y
    start_A <- max(resposta) * 1.1 # 10% acima do máximo

    # Ajuste do Modelo Não-Linear
    tryCatch(
      {
        # Estimativas iniciais para B e c
        # log(start_A - Y) ~ X
        data_log <- data[start_A - data$Y > 0, ]
        linear_fit <- lm(log(start_A - Y) ~ X, data = data_log)
        start_B <- exp(coef(linear_fit)[1])
        start_c <- exp(coef(linear_fit)[2])

        # Ajuste NLS
        model_fit <- nls(
          Y ~ A - B * c^X,
          data = data,
          start = list(A = start_A, B = start_B, c = start_c)
        )
        model_summary <- summary(model_fit)

        # Extração de Métricas
        Y_hat <- predict(model_fit)
        SSE <- sum((resposta - Y_hat)^2)
        SST <- sum((resposta - mean(resposta))^2)
        R2 <- 1 - (SSE / SST)
        RMSE <- sqrt(SSE / n)

        # AIC e BIC (Log-Verossimilhança)
        logL <- logLik(model_fit)
        AIC_val <- AIC(model_fit)
        BIC_val <- BIC(model_fit)

        # Extração de Coeficientes e Estatísticas
        coefs <- coef(model_fit)
        A <- coefs["A"]
        B <- coefs["B"]
        c <- coefs["c"]
        df_res <- model_summary$df[2]

        # Armazenamento de Resultados
        resultado_df <- data.frame(
          resposta = nome_resposta,
          parametro = c(
            "A_assintota",
            "B_amplitude",
            "c_taxa",
            "R2",
            "RMSE",
            "AIC",
            "BIC",
            "df_residual"
          ),
          valor = c(A, B, c, R2, RMSE, AIC_val, BIC_val, df_res),
          stringsAsFactors = FALSE
        )

        equacao_str <- sprintf(
          "%s: Y = %.4f - %.4f * %.4f^X",
          nome_resposta,
          A,
          B,
          c
        )

        resultados_all[[nome_resposta]] <- resultado_df
        modelos_ajustados[[nome_resposta]] <- model_fit
        equacoes[[i]] <- equacao_str

        # Visualização ggplot2
        dose_pred <- seq(min(dose), max(dose), length.out = 100)
        Y_pred <- predict(model_fit, newdata = data.frame(X = dose_pred))
        df_plot <- data.frame(
          dose = c(dose, dose_pred),
          Y = c(resposta, Y_pred),
          tipo = c(
            rep("Observado", length(dose)),
            rep("Ajustado", length(dose_pred))
          )
        )

        p <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(x = dose, y = Y, color = tipo)
        ) +
          ggplot2::geom_point(
            data = data.frame(dose = dose, Y = resposta),
            size = 3,
            alpha = 0.7
          ) +
          ggplot2::geom_line(
            data = data.frame(dose = dose_pred, Y = Y_pred),
            linewidth = 1
          ) +
          ggplot2::labs(
            title = sprintf("Ajuste Mitscherlich - %s", nome_resposta),
            x = "Dose",
            y = nome_resposta,
            color = "Tipo",
            subtitle = equacao_str,
            caption = sprintf("R² = %.4f | RMSE = %.4f", R2, RMSE)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "top",
            plot.title = ggplot2::element_text(face = "bold", size = 12),
            plot.subtitle = ggplot2::element_text(size = 10, face = "italic")
          )

        graficos[[nome_resposta]] <- p
        print(p)

        # Impressão Verbosa
        if (verbose) {
          cat(sprintf(
            "------------------------ %s ------------------------\n",
            nome_resposta
          ))
          cat(sprintf("Modelo: Mitscherlich (nls)\n"))
          cat(sprintf(
            "R²: %.4f | RMSE: %.4f | AIC: %.2f | BIC: %.2f\n",
            R2,
            RMSE,
            AIC_val,
            BIC_val
          ))
          cat("Equação:\n", equacao_str, "\n\n")
          cat(format_coef_table(model_fit), "\n")
          cat(sprintf("Graus de Liberdade Residual: %d\n", df_res))
          cat("\n")
        }
      },
      error = function(e) {
        # Tratamento de erro para nls (não convergência)
        warning(paste("Ajuste NLS para", nome_resposta, "falhou:", e$message))
        resultado_df <- data.frame(
          resposta = nome_resposta,
          parametro = "erro",
          valor = "Nao_foi_possivel_ajustar",
          stringsAsFactors = FALSE
        )
        resultados_all[[nome_resposta]] <- resultado_df
        equacoes[[i]] <- paste(nome_resposta, ": Não ajustado (NLS falhou)")

        if (verbose) {
          cat(sprintf(
            "------------------------ %s ------------------------\n",
            nome_resposta
          ))
          cat(
            "❌ Não foi possível ajustar o Modelo de Mitscherlich (NLS falhou)\n\n"
          )
        }
      }
    )
  }

  resultado_df_final <- do.call(rbind, resultados_all)

  return(list(
    resultados = resultado_df_final,
    modelos = modelos_ajustados,
    equacoes = equacoes,
    graficos = graficos
  ))
}
