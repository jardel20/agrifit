#' Ajusta o Modelo Linear Simples
#'
#' Implementa o ajuste do modelo de regressão linear simples: Y = b0 + b1 * X.
#'
#' @param dose Vetor numérico com níveis do fator (X).
#' @param ... Uma ou mais respostas (vetores Y nomeados). Exemplo: `MS = materia_seca`.
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: b0, b1, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada resposta.}
#'   \item{modelos}{Lista dos objetos `lm` para cada resposta.}
#'   \item{equacoes}{Strings formatadas das equações.}
#'   \item{graficos}{Lista de gráficos ggplot2 com observações e reta ajustada para cada resposta.}
#' }
#'
#' @examples
#' \dontrun{
#' dose <- c(0, 50, 100, 150, 200, 250)
#' resposta <- c(10, 15, 20, 25, 30, 35)
#'
#' resultado <- ajustar_linear(
#'   dose = dose,
#'   Y = resposta,
#'   verbose = TRUE
#' )
#'
#' print(resultado$resultados)
#' print(resultado$equacoes)
#' }
#'
#' @importFrom stats lm coef pt qt vcov
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal
#'
#' @export
ajustar_linear <- function(dose, ..., verbose = TRUE) {
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

  format_coef_table <- function(model, df_res) {
    # Extrai coeficientes diretamente do modelo para evitar warnings
    coefs <- coef(model)
    params <- names(coefs)
    estimates <- as.numeric(coefs)

    # Calcula erros padrão manualmente
    var_cov <- suppressWarnings(vcov(model))
    std_errors <- sqrt(diag(var_cov))

    # Calcula t-values e p-values
    t_values <- estimates / std_errors
    p_values <- 2 * pt(-abs(t_values), df = df_res)

    # Marca t-values muito grandes como Inf (indica ajuste perfeito)
    t_values[abs(t_values) > 1e10] <- Inf

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

    # Formatação com tratamento especial para valores muito grandes
    coef_df$Estimate <- ifelse(
      is.na(coef_df$Estimate) | is.infinite(coef_df$Estimate),
      "NA",
      sprintf("%.4f", coef_df$Estimate)
    )
    coef_df$Std.Error <- ifelse(
      is.na(coef_df$Std.Error) | is.infinite(coef_df$Std.Error),
      "NA",
      sprintf("%.4f", coef_df$Std.Error)
    )
    coef_df$t.value <- ifelse(
      is.na(coef_df$t.value) | is.infinite(coef_df$t.value),
      "Inf",
      sprintf("%.4f", coef_df$t.value)
    )
    coef_df$Pr.t <- sapply(coef_df$Pr.t, format_p_value)
    coef_df$IC_Low <- ifelse(
      is.na(coef_df$IC_Low) | is.infinite(coef_df$IC_Low),
      "NA",
      sprintf("%.4f", coef_df$IC_Low)
    )
    coef_df$IC_High <- ifelse(
      is.na(coef_df$IC_High) | is.infinite(coef_df$IC_High),
      "NA",
      sprintf("%.4f", coef_df$IC_High)
    )

    # Alinhamento e cabeçalho com colunas mais amplas
    sep_line <- "──────────────────────────────────────────────────────────────────────────────────────────────"
    header <- "Parameter         Estimate Std. Error  t value   Pr(>|t|)   IC 95% Low  IC 95% High"
    lines <- apply(coef_df, 1, function(row) {
      sprintf(
        "%-18s%-12s%-12s%-10s%-12s%-12s%-12s",
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
    signif_line <- "──────────────────────────────────────────────────────────────────────────────────────────────
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"

    # Junta tudo com separadores
    output <- paste(
      "Coefficients:",
      sep_line,
      header,
      sep_line,
      paste(lines, collapse = "\n"),
      signif_line,
      sep = "\n"
    )

    return(output)
  }

  # Lógica Principal
  respostas_list <- list(...)
  n_respostas <- length(respostas_list)
  nomes_respostas <- names(respostas_list)
  n <- length(dose)

  if (n < 3) {
    stop("O modelo linear requer no mínimo 3 pontos de dose.")
  }

  resultados_all <- list()
  modelos_ajustados <- list()
  equacoes <- list()
  graficos <- list()

  for (i in 1:n_respostas) {
    resposta <- respostas_list[[i]]
    nome_resposta <- nomes_respostas[i]
    data <- data.frame(X = dose, Y = resposta)

    # Ajuste do Modelo (suprime warnings de ajustes perfeitos)
    model_fit <- suppressWarnings(lm(Y ~ X, data = data))

    # EXTRAÇÃO dos coeficientes
    sum_model <- summary(model_fit)
    coefs_table <- sum_model$coefficients
    R2 <- sum_model$r.squared
    df_res <- sum_model$df[2]
    SSE <- sum(model_fit$residuals^2)
    n <- length(resposta)
    RMSE <- sqrt(SSE / n)

    # AIC/BIC NATIVOS
    AIC_val <- AIC(model_fit)
    BIC_val <- BIC(model_fit)

    # Coeficientes do summary()
    b0 <- coefs_table[1, 1]
    b1 <- coefs_table[2, 1]
    se_b0 <- coefs_table[1, 2]
    se_b1 <- coefs_table[2, 2]
    t_b0 <- coefs_table[1, 3]
    t_b1 <- coefs_table[2, 3]
    p_b0 <- coefs_table[1, 4]
    p_b1 <- coefs_table[2, 4]

    # ICs nativos (mais preciso)
    t_crit <- qt(0.975, df = df_res)
    ic_b0_low <- b0 - t_crit * se_b0
    ic_b0_high <- b0 + t_crit * se_b0
    ic_b1_low <- b1 - t_crit * se_b1
    ic_b1_high <- b1 + t_crit * se_b1


    # Armazenamento de Resultados
    resultado_df <- data.frame(
      resposta = nome_resposta,
      parametro = c(
        "b0_intercepto",
        "b1_declividade",
        "R2",
        "RMSE",
        "AIC",
        "BIC",
        "p_valor_b0",
        "p_valor_b1",
        "ic_b0_low",
        "ic_b0_high",
        "ic_b1_low",
        "ic_b1_high",
        "df_residual"
      ),
      valor = c(
        b0,
        b1,
        R2,
        RMSE,
        AIC_val,
        BIC_val,
        p_b0,
        p_b1,
        ic_b0_low,
        ic_b0_high,
        ic_b1_low,
        ic_b1_high,
        df_res
      ),
      stringsAsFactors = FALSE
    )

    equacao_str <- sprintf("%s: Ŷ = %.4f + %.4fX", nome_resposta, b0, b1)

    resultados_all[[nome_resposta]] <- resultado_df
    modelos_ajustados[[nome_resposta]] <- model_fit
    equacoes[[i]] <- equacao_str

    # Impressão Verbosa
    if (verbose) {
      cat(sprintf(
        "------------------------ %s ------------------------\n",
        nome_resposta
      ))
      cat(sprintf("Modelo: Linear Simples\n"))
      cat(sprintf(
        "R²: %.4f | RMSE: %.4f | AIC: %.2f | BIC: %.2f\n",
        R2,
        RMSE,
        AIC_val,
        BIC_val
      ))
      cat("Equação:\n", equacao_str, "\n\n")
      cat(format_coef_table(model_fit, df_res), "\n")
      cat(sprintf("Graus de Liberdade Residual: %d\n", df_res))
      cat("\n")
    }

    # --- Gráfico ggplot2 ---
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

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = dose, y = Y, color = tipo)) +
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
        title = sprintf("Ajuste Linear - %s", nome_resposta),
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
  }

  resultado_df_final <- do.call(rbind, resultados_all)

  return(list(
    resultados = resultado_df_final,
    modelos = modelos_ajustados,
    equacoes = equacoes,
    graficos = graficos
  ))
}
