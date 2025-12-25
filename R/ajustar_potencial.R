#' Ajusta o Modelo Potencial
#'
#' Implementa o ajuste do modelo de regressão potencial: Y = a * X^b.
#' O ajuste é feito usando `nls` (Non-linear Least Squares).
#'
#' @param dose Vetor numérico com níveis do fator (X).
#' @param ... Uma ou mais respostas (vetores Y nomeados). Exemplo: `MS = materia_seca`.
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: a, b, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada resposta.}
#'   \item{modelos}{Lista dos objetos `nls` para cada resposta.}
#'   \item{equacoes}{Strings formatadas das equações.}
#' }
#'
#' @importFrom stats nls coef pt qt logLik AIC BIC predict
#' @importFrom dplyr "%>%"
#'
#' @examples
#' \dontrun{
#' dose <- c(1, 10, 20, 50, 100, 200)
#' resposta <- c(5, 12, 18, 28, 38, 50)
#'
#' resultado <- ajustar_potencial(
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
ajustar_potencial <- function(dose, ..., verbose = TRUE) {
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
    header <- "             Estimate Std. Error t value Pr(>|t|)    IC 95% Low   IC 95% High"
    lines <- apply(coef_df, 1, function(row) {
      sprintf(
        "%-15s %s %s %s %s %s %s",
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
    signif_line <- "---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

    # Junta tudo
    output <- paste(
      "Coefficients:",
      header,
      paste(lines, collapse = "\n"),
      signif_line,
      sep = "\n"
    )

    return(output)
  }

  # --- Lógica Principal ---
  respostas_list <- list(...)
  n_respostas <- length(respostas_list)
  nomes_respostas <- names(respostas_list)
  n <- length(dose)

  if (n < 3) {
    stop("O modelo potencial requer no mínimo 3 pontos de dose.")
  }

  resultados_all <- list()
  modelos_ajustados <- list()
  equacoes <- list()

  for (i in 1:n_respostas) {
    resposta <- respostas_list[[i]]
    nome_resposta <- nomes_respostas[i]
    data <- data.frame(X = dose, Y = resposta)

    # Estimativas Iniciais (Linearização)
    # log(Y) = log(a) + b * log(X)
    # Filtra dose > 0 para log
    data_log <- data[data$X > 0 & data$Y > 0, ]

    if (nrow(data_log) < 3) {
      warning(paste(
        "Ajuste NLS para",
        nome_resposta,
        "falhou: Dados insuficientes após log-transformação."
      ))
      resultado_df <- data.frame(
        resposta = nome_resposta,
        parametro = "erro",
        valor = "Nao_foi_possivel_ajustar",
        stringsAsFactors = FALSE
      )
      resultados_all[[nome_resposta]] <- resultado_df
      equacoes[[i]] <- paste(
        nome_resposta,
        ": Não ajustado (Dados insuficientes)"
      )

      if (verbose) {
        cat(sprintf(
          "------------------------ %s ------------------------\n",
          nome_resposta
        ))
        cat(
          "❌ Não foi possível ajustar o Modelo Potencial (Dados insuficientes)\n\n"
        )
      }
      next
    }

    linear_fit <- lm(log(Y) ~ log(X), data = data_log)
    start_a <- exp(coef(linear_fit)[1])
    start_b <- coef(linear_fit)[2]

    # Ajuste do Modelo Não-Linear
    tryCatch(
      {
        model_fit <- nls(
          Y ~ a * X^b,
          data = data,
          start = list(a = start_a, b = start_b)
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
        a <- coefs["a"]
        b <- coefs["b"]
        df_res <- model_summary$df[2]

        # Armazenamento de Resultados
        resultado_df <- data.frame(
          resposta = nome_resposta,
          parametro = c(
            "a_escala",
            "b_expoente",
            "R2",
            "RMSE",
            "AIC",
            "BIC",
            "df_residual"
          ),
          valor = c(a, b, R2, RMSE, AIC_val, BIC_val, df_res),
          stringsAsFactors = FALSE
        )

        equacao_str <- sprintf("%s: Ŷ = %.4f * X^%.4f", nome_resposta, a, b)

        resultados_all[[nome_resposta]] <- resultado_df
        modelos_ajustados[[nome_resposta]] <- model_fit
        equacoes[[i]] <- equacao_str

        # Impressão Verbosa
        if (verbose) {
          cat(sprintf(
            "------------------------ %s ------------------------\n",
            nome_resposta
          ))
          cat(sprintf("Modelo: Potencial (nls)\n"))
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
          cat("❌ Não foi possível ajustar o Modelo Potencial (NLS falhou)\n\n")
        }
      }
    )
  }

  resultado_df_final <- do.call(rbind, resultados_all)

  return(list(
    resultados = resultado_df_final,
    modelos = modelos_ajustados,
    equacoes = equacoes
  ))
}
