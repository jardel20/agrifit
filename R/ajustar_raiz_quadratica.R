#' Ajusta o Modelo Raiz Quadrático
#'
#' Implementa o ajuste do modelo de regressão raiz quadrática: Y = b0 + b1 * sqrt(X) + b2 * X.
#'
#' @param dose Vetor numérico com níveis do fator (X).
#' @param ... Uma ou mais respostas (vetores Y nomeados). Exemplo: `MS = materia_seca`.
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: b0, b1, b2, R², AIC, BIC, RMSE, p-valores, IC, etc. para cada resposta.}
#'   \item{modelos}{Lista dos objetos `lm` para cada resposta.}
#'   \item{equacoes}{Strings formatadas das equações.}
#' }
#'
#' @importFrom stats lm coef pt qt
#' @importFrom dplyr "%>%"
#'
#' @export
ajustar_raiz_quadratica <- function(dose, ..., verbose = TRUE) {
  
  # Funções auxiliares (copiadas da estrutura LRP)
  format_p_value <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return(paste(sprintf("%.4f", p), "***"))
    if (p < 0.01) return(paste(sprintf("%.4f", p), "**"))
    if (p < 0.05) return(paste(sprintf("%.4f", p), "*"))
    if (p < 0.1) return(paste(sprintf("%.4f", p), "."))
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
    header <- "             Estimate Std. Error t value Pr(>|t|)    IC 95% Low   IC 95% High"
    lines <- apply(coef_df, 1, function(row) {
      sprintf("%-15s %s %s %s %s %s %s", row[1], row[2], row[3], row[4], row[5], row[6], row[7])
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
  
  if (n < 4) stop("O modelo raiz quadrático requer no mínimo 4 pontos de dose.")
  
  resultados_all <- list()
  modelos_ajustados <- list()
  equacoes <- list()
  
  for (i in 1:n_respostas) {
    resposta <- respostas_list[[i]]
    nome_resposta <- nomes_respostas[i]
    data <- data.frame(X = dose, Y = resposta, X_sqrt = sqrt(dose))
    
    # Ajuste do Modelo
    model_fit <- lm(Y ~ X_sqrt + X, data = data)
    model_summary <- summary(model_fit)
    
    # Extração de Métricas
    SSE <- sum(model_fit$residuals^2)
    SST <- sum((resposta - mean(resposta))^2)
    R2 <- model_summary$r.squared
    RMSE <- sqrt(SSE / n)
    
    # AIC e BIC (Log-Verossimilhança)
    K <- length(coef(model_fit)) # 3 parâmetros (b0, b1, b2)
    logL <- - (n / 2) * (log(2 * pi) + log(SSE / n) + 1)
    AIC_val <- -2 * logL + 2 * K
    BIC_val <- -2 * logL + K * log(n)
    
    # Extração de Coeficientes e Estatísticas
    coefs <- model_summary$coefficients
    b0 <- coefs[1, 1]
    b1 <- coefs[2, 1] # Coeficiente de sqrt(X)
    b2 <- coefs[3, 1] # Coeficiente de X
    p_b0 <- coefs[1, 4]
    p_b1 <- coefs[2, 4]
    p_b2 <- coefs[3, 4]
    df_res <- model_summary$df[2]
    
    # Intervalos de Confiança (IC)
    t_crit <- qt(0.975, df = df_res)
    ic_b0_low <- b0 - t_crit * coefs[1, 2]
    ic_b0_high <- b0 + t_crit * coefs[1, 2]
    ic_b1_low <- b1 - t_crit * coefs[2, 2]
    ic_b1_high <- b1 + t_crit * coefs[2, 2]
    ic_b2_low <- b2 - t_crit * coefs[3, 2]
    ic_b2_high <- b2 + t_crit * coefs[3, 2]
    
    # Armazenamento de Resultados
    resultado_df <- data.frame(
      resposta = nome_resposta,
      parametro = c("b0_intercepto", "b1_raiz_quadratica", "b2_linear", "R2", "RMSE", "AIC", "BIC",
                    "p_valor_b0", "p_valor_b1", "p_valor_b2",
                    "ic_b0_low", "ic_b0_high", "ic_b1_low", "ic_b1_high",
                    "ic_b2_low", "ic_b2_high", "df_residual"),
      valor = c(b0, b1, b2, R2, RMSE, AIC_val, BIC_val, p_b0, p_b1, p_b2,
                ic_b0_low, ic_b0_high, ic_b1_low, ic_b1_high,
                ic_b2_low, ic_b2_high, df_res),
      stringsAsFactors = FALSE
    )
    
    equacao_str <- sprintf("%s: Ŷ = %.4f + %.4f√X + %.4fX", nome_resposta, b0, b1, b2)
    
    resultados_all[[nome_resposta]] <- resultado_df
    modelos_ajustados[[nome_resposta]] <- model_fit
    equacoes[[i]] <- equacao_str
    
    # Impressão Verbosa
    if (verbose) {
      cat(sprintf("------------------------ %s ------------------------\n", nome_resposta))
      cat(sprintf("Modelo: Raiz Quadrática\n"))
      cat(sprintf("R²: %.4f | RMSE: %.4f | AIC: %.2f | BIC: %.2f\n", R2, RMSE, AIC_val, BIC_val))
      cat("Equação:\n", equacao_str, "\n\n")
      cat(format_coef_table(model_fit, df_res), "\n")
      cat(sprintf("Graus de Liberdade Residual: %d\n", df_res))
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
