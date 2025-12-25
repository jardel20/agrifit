#' Ajusta o Modelo Quadrático de Superfície de Resposta (2 Fatores)
#'
#' Implementa o ajuste do modelo quadrático completo para dois fatores (X1 e X2):
#' Y = b0 + b1*X1 + b2*X2 + b11*X1^2 + b22*X2^2 + b12*X1*X2.
#'
#' @param X1 Vetor numérico do Fator 1.
#' @param X2 Vetor numérico do Fator 2.
#' @param ... Uma ou mais respostas (vetores Y nomeados). Exemplo: `Prod = producao`.
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: b0, b1, b2, b11, b22, b12, R², AIC, BIC, RMSE, p-valores, IC, Ponto Estacionário, etc. para cada resposta.}
#'   \item{modelos}{Lista dos objetos `lm` para cada resposta.}
#'   \item{equacoes}{Strings formatadas das equações.}
#' }
#'
#' @importFrom stats lm coef pt qt
#' @importFrom dplyr "%>%"
#'
#' @examples
#' \dontrun{
#' X1 <- c(-1, -1, 1, 1, 0, 0, 0, 0, 0)
#' X2 <- c(-1, 1, -1, 1, 0, 0, 0, 0, 0)
#' prod <- c(50, 60, 55, 70, 65, 65, 65, 66, 64)
#'
#' resultado <- analise_superficie_resposta(
#'   X1 = X1,
#'   X2 = X2,
#'   Producao = prod,
#'   verbose = TRUE
#' )
#'
#' print(resultado$resultados)
#' print(resultado$equacoes)
#' }
#'
#' @export
analise_superficie_resposta <- function(X1, X2, ..., verbose = TRUE) {
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
    # Extrai estatísticas do summary
    lm_summary <- summary(model)
    lm_coefs <- lm_summary$coefficients

    params <- rownames(lm_coefs)
    estimates <- lm_coefs[, 1]
    std_errors <- lm_coefs[, 2]
    t_values <- lm_coefs[, 3]
    p_values <- lm_coefs[, 4]

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
    coef_df$Std.Error <- ifelse(is.na(coef_df$Std.Error), "NA", sprintf("%.4f", coef_df$Std.Error))
    coef_df$t.value <- ifelse(is.na(coef_df$t.value), "NA", sprintf("%.2f", coef_df$t.value))
    coef_df$Pr.t <- sapply(coef_df$Pr.t, format_p_value)
    coef_df$IC_Low <- ifelse(is.na(coef_df$IC_Low), "NA", sprintf("%.4f", coef_df$IC_Low))
    coef_df$IC_High <- ifelse(is.na(coef_df$IC_High), "NA", sprintf("%.4f", coef_df$IC_High))

    # Alinhamento e cabeçalho
    sep_line <- "──────────────────────────────────────────────────────────────────────────────────"
    header <- "Parameter         Estimate Std. Error  t value  Pr(>|t|)   IC 95% Low  IC 95% High"
    lines <- apply(coef_df, 1, function(row) {
      sprintf("%-18s%-10s%-12s%-10s%-12s%-12s%-12s", row[1], row[2], row[3], row[4], row[5], row[6], row[7])
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
  n <- length(X1)

  # O modelo quadrático completo tem 6 parâmetros. Requer no mínimo 7 pontos.
  if (n < 7) stop("O modelo de Superfície de Resposta (Quadrático Completo) requer no mínimo 7 pontos de dose.")

  resultados_all <- list()
  modelos_ajustados <- list()
  equacoes <- list()

  for (i in 1:n_respostas) {
    resposta <- respostas_list[[i]]
    nome_resposta <- nomes_respostas[i]
    data <- data.frame(X1 = X1, X2 = X2, Y = resposta)

    # Ajuste do Modelo Quadrático Completo
    # Y = b0 + b1*X1 + b2*X2 + b11*X1^2 + b22*X2^2 + b12*X1*X2
    model_fit <- lm(Y ~ X1 + X2 + I(X1^2) + I(X2^2) + X1:X2, data = data)
    model_summary <- summary(model_fit)

    # Extração de Métricas
    SSE <- sum(model_fit$residuals^2)
    SST <- sum((resposta - mean(resposta))^2)
    R2 <- model_summary$r.squared
    RMSE <- sqrt(SSE / n)

    # AIC e BIC (Log-Verossimilhança)
    K <- length(coef(model_fit)) # 6 parâmetros
    logL <- -(n / 2) * (log(2 * pi) + log(SSE / n) + 1)
    AIC_val <- -2 * logL + 2 * K
    BIC_val <- -2 * logL + K * log(n)

    # Extração de Coeficientes e Estatísticas
    coefs <- coef(model_fit)
    b0 <- coefs[1]
    b1 <- coefs[2]
    b2 <- coefs[3]
    b11 <- coefs[4] # I(X1^2)
    b22 <- coefs[5] # I(X2^2)
    b12 <- coefs[6] # X1:X2
    df_res <- model_summary$df[2]

    # --- Cálculo do Ponto Estacionário (Máximo/Mínimo/Sela) ---
    # Gradiente: dY/dX1 = b1 + 2*b11*X1 + b12*X2 = 0
    # Gradiente: dY/dX2 = b2 + 2*b22*X2 + b12*X1 = 0

    # Matriz H (Hessiana) para o sistema de equações
    H <- matrix(c(2 * b11, b12, b12, 2 * b22), nrow = 2, byrow = TRUE)

    # Vetor B para o sistema de equações
    B <- matrix(c(-b1, -b2), nrow = 2)

    # Ponto Estacionário (X1s, X2s)
    tryCatch(
      {
        X_estacionario <- solve(H, B)
        X1s <- X_estacionario[1]
        X2s <- X_estacionario[2]
        Y_estacionario <- b0 + b1 * X1s + b2 * X2s + b11 * X1s^2 + b22 * X2s^2 + b12 * X1s * X2s

        # Classificação (Eigenvalues da Matriz H)
        eigen_values <- eigen(H)$values

        if (all(eigen_values < 0)) {
          classificacao <- "Máximo"
        } else if (all(eigen_values > 0)) {
          classificacao <- "Mínimo"
        } else {
          classificacao <- "Sela"
        }
      },
      error = function(e) {
        X1s <- NA
        X2s <- NA
        Y_estacionario <- NA
        classificacao <- "Não Calculável"
      }
    )

    # Armazenamento de Resultados
    resultado_df <- data.frame(
      resposta = nome_resposta,
      parametro = c(
        "b0_intercepto", "b1_linear_X1", "b2_linear_X2", "b11_quad_X1", "b22_quad_X2", "b12_interacao",
        "R2", "RMSE", "AIC", "BIC", "df_residual",
        "X1_estacionario", "X2_estacionario", "Y_estacionario", "classificacao_ponto"
      ),
      valor = c(
        b0, b1, b2, b11, b22, b12,
        R2, RMSE, AIC_val, BIC_val, df_res,
        X1s, X2s, Y_estacionario, classificacao
      ),
      stringsAsFactors = FALSE
    )

    equacao_str <- sprintf(
      "%s: Ŷ = %.4f + %.4fX1 + %.4fX2 + %.4fX1² + %.4fX2² + %.4fX1X2",
      nome_resposta, b0, b1, b2, b11, b22, b12
    )

    resultados_all[[nome_resposta]] <- resultado_df
    modelos_ajustados[[nome_resposta]] <- model_fit
    equacoes[[i]] <- equacao_str

    # Impressão Verbosa
    if (verbose) {
      cat(sprintf("------------------------ %s ------------------------\n", nome_resposta))
      cat(sprintf("Modelo: Superfície de Resposta (Quadrático Completo)\n"))
      cat(sprintf("R²: %.4f | RMSE: %.4f | AIC: %.2f | BIC: %.2f\n", R2, RMSE, AIC_val, BIC_val))
      cat("Equação:\n", equacao_str, "\n\n")
      cat(format_coef_table(model_fit, df_res), "\n")
      cat(sprintf("Graus de Liberdade Residual: %d\n", df_res))

      cat("\n--- Ponto Estacionário ---\n")
      cat(sprintf("X1s: %.4f\n", X1s))
      cat(sprintf("X2s: %.4f\n", X2s))
      cat(sprintf("Y_estacionario: %.4f\n", Y_estacionario))
      cat(sprintf("Classificação: %s\n", classificacao))
      cat("\n")
    }
  }

  resultado_df_final <- do.call(rbind, resultados_all)

  return(list(
    resultados = resultado_df_final,
    modelos = modelos_ajustados,
    equacoes = equacoes
  ))
}
