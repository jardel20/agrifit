#' Ajusta o Modelo Quadrático de Superfície de Resposta (2 Fatores)
#'
#' Implementa o ajuste do modelo quadrático completo para dois fatores (X1 e X2):
#' Y = b0 + b1*X1 + b2*X2 + b11*X1^2 + b22*X2^2 + b12*X1*X2.
#'
#' @param X1 Vetor numérico do Fator 1.
#' @param X2 Vetor numérico do Fator 2.
#' @param ... Uma ou mais respostas (vetores Y nomeadas). Exemplo: `Prod = producao`.
#' @param verbose Lógico. Gera prints detalhados (padrão: TRUE).
#'
#' @return Lista com:
#' \describe{
#'   \item{resultados}{Data frame: b0, b1, b2, b11, b22, b12, R², AIC, BIC, RMSE, p-valores, IC, Ponto Estacionário, etc. para cada resposta.}
#'   \item{modelos}{Lista dos objetos `lm` para cada resposta.}
#'   \item{equacoes}{Strings formatadas das equações.}
#'   \item{graficos}{Lista de gráficos de superfície de resposta 3D para cada resposta.}
#' }
#'
#' @importFrom stats lm coef pt qt
#' @importFrom dplyr "%>%"
#' @importFrom plotly plot_ly add_surface layout
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
#' # resultado$graficos$Producao  # Para visualizar a superfície 3D
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
  if (n < 7) {
    stop(
      "O modelo de Superfície de Resposta (Quadrático Completo) requer no mínimo 7 pontos de dose."
    )
  }

  resultados_all <- list()
  modelos_ajustados <- list()
  equacoes <- list()
  graficos <- list()

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

    # Substituir NA por 0 para evitar problemas no cálculo de ponto estacionário
    if (is.na(b22)) {
      b22 <- 0
    }
    if (is.na(b12)) {
      b12 <- 0
    }
    if (is.na(b11)) {
      b11 <- 0
    }
    if (is.na(b2)) {
      b2 <- 0
    }
    if (is.na(b1)) {
      b1 <- 0
    }

    df_res <- model_summary$df[2]

    # --- Cálculo do Ponto Estacionário (Máximo/Mínimo/Sela) ---
    # Gradiente: dY/dX1 = b1 + 2*b11*X1 + b12*X2 = 0
    # Gradiente: dY/dX2 = b2 + 2*b22*X2 + b12*X1 = 0

    # Inicializar variáveis
    X1s <- NA
    X2s <- NA
    Y_estacionario <- NA
    classificacao <- "Não Calculável"

    # Matriz H (Hessiana) para o sistema de equações
    H <- matrix(c(2 * b11, b12, b12, 2 * b22), nrow = 2, byrow = TRUE)

    # Vetor B para o sistema de equações
    B <- matrix(c(-b1, -b2), nrow = 2)

    # Ponto Estacionário (X1s, X2s)
    resultado_estacionario <- tryCatch(
      {
        # Verificar se a matriz H é invertível
        det_H <- det(H)
        if (abs(det_H) < 1e-6) {
          list(
            X1s = NA,
            X2s = NA,
            Y_estacionario = NA,
            classificacao = "Não Calculável"
          )
        } else {
          X_estacionario <- solve(H, B)
          X1s_temp <- X_estacionario[1]
          X2s_temp <- X_estacionario[2]
          Y_estacionario_temp <- b0 +
            b1 * X1s_temp +
            b2 * X2s_temp +
            b11 * X1s_temp^2 +
            b22 * X2s_temp^2 +
            b12 * X1s_temp * X2s_temp

          # Classificação (Eigenvalues da Matriz H)
          eigen_values <- eigen(H)$values

          if (all(eigen_values < 0)) {
            classificacao_temp <- "Máximo"
          } else if (all(eigen_values > 0)) {
            classificacao_temp <- "Mínimo"
          } else {
            classificacao_temp <- "Sela"
          }

          list(
            X1s = X1s_temp,
            X2s = X2s_temp,
            Y_estacionario = Y_estacionario_temp,
            classificacao = classificacao_temp
          )
        }
      },
      error = function(e) {
        list(
          X1s = NA,
          X2s = NA,
          Y_estacionario = NA,
          classificacao = "Não Calculável"
        )
      }
    )

    X1s <- resultado_estacionario$X1s
    X2s <- resultado_estacionario$X2s
    Y_estacionario <- resultado_estacionario$Y_estacionario
    classificacao <- resultado_estacionario$classificacao

    # Armazenamento de Resultados
    resultado_df <- data.frame(
      resposta = nome_resposta,
      parametro = c(
        "b0_intercepto",
        "b1_linear_X1",
        "b2_linear_X2",
        "b11_quad_X1",
        "b22_quad_X2",
        "b12_interacao",
        "R2",
        "RMSE",
        "AIC",
        "BIC",
        "df_residual",
        "X1_estacionario",
        "X2_estacionario",
        "Y_estacionario",
        "classificacao_ponto"
      ),
      valor = c(
        b0,
        b1,
        b2,
        b11,
        b22,
        b12,
        R2,
        RMSE,
        AIC_val,
        BIC_val,
        df_res,
        X1s,
        X2s,
        Y_estacionario,
        classificacao
      ),
      stringsAsFactors = FALSE
    )

    equacao_str <- sprintf(
      "%s: Ŷ = %.4f + %.4fX1 + %.4fX2 + %.4fX1² + %.4fX2² + %.4fX1X2",
      nome_resposta,
      b0,
      b1,
      b2,
      b11, # Já substituído NA por 0
      b22, # Já substituído NA por 0
      b12
    )

    resultados_all[[nome_resposta]] <- resultado_df
    modelos_ajustados[[nome_resposta]] <- model_fit
    equacoes[[i]] <- equacao_str

    # Impressão Verbosa
    if (verbose) {
      cat(sprintf(
        "------------------------ %s ------------------------\n",
        nome_resposta
      ))
      cat(sprintf("Modelo: Superfície de Resposta (Quadrático Completo)\n"))
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

      cat("\n--- Ponto Estacionário ---\n")
      cat(sprintf("X1s: %.4f\n", X1s))
      cat(sprintf("X2s: %.4f\n", X2s))
      cat(sprintf("Y_estacionario: %.4f\n", Y_estacionario))
      cat(sprintf("Classificação: %s\n", classificacao))
      cat("\n")
    }

    # --- Gráfico de Superfície de Resposta 3D ---
    # Criar malha de pontos para a superfície
    X1_range <- seq(min(X1) - 0.5, max(X1) + 0.5, length.out = 30)
    X2_range <- seq(min(X2) - 0.5, max(X2) + 0.5, length.out = 30)
    grid_data <- expand.grid(X1 = X1_range, X2 = X2_range)

    # Extrair coeficientes originais (antes de substituir NA por 0)
    coefs_orig <- coef(model_fit)
    b0_plot <- coefs_orig[1]
    b1_plot <- coefs_orig[2]
    b2_plot <- coefs_orig[3]
    b11_plot <- ifelse(is.na(coefs_orig[4]), 0, coefs_orig[4])
    b22_plot <- ifelse(is.na(coefs_orig[5]), 0, coefs_orig[5])
    b12_plot <- ifelse(is.na(coefs_orig[6]), 0, coefs_orig[6])

    # Calcular Z (resposta predita)
    grid_data$Z <- b0_plot +
      b1_plot * grid_data$X1 +
      b2_plot * grid_data$X2 +
      b11_plot * grid_data$X1^2 +
      b22_plot * grid_data$X2^2 +
      b12_plot * grid_data$X1 * grid_data$X2

    # Converter para matriz para plotly
    Z_matrix <- matrix(
      grid_data$Z,
      nrow = length(X1_range),
      ncol = length(X2_range)
    )

    # Criar gráfico 3D com plotly
    p_3d <- plotly::plot_ly(
      x = X1_range,
      y = X2_range,
      z = Z_matrix,
      type = "surface",
      colorscale = "Viridis",
      showscale = TRUE
    ) %>%
      plotly::add_trace(
        x = X1,
        y = X2,
        z = resposta,
        mode = "markers",
        type = "scatter3d",
        marker = list(size = 6, color = "red", symbol = "circle"),
        name = "Observações",
        showlegend = TRUE
      ) %>%
      plotly::layout(
        title = sprintf("Superfície de Resposta - %s", nome_resposta),
        scene = list(
          xaxis = list(title = "X1"),
          yaxis = list(title = "X2"),
          zaxis = list(title = nome_resposta)
        ),
        showlegend = TRUE
      )

    graficos[[nome_resposta]] <- p_3d

    if (verbose) {
      print(p_3d)
    }
  }

  resultado_df_final <- do.call(rbind, resultados_all)

  return(list(
    resultados = resultado_df_final,
    modelos = modelos_ajustados,
    equacoes = equacoes,
    graficos = graficos
  ))
}
