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
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline labs theme_minimal
#'
#' @examples
#' \dontrun{
#' dose <- c(0, 50, 100, 150, 200, 250)
#' resposta <- c(10, 18, 24, 28, 30, 31)
#'
#' # Ajusta modelo quadrático
#' modelo_quad <- ajustar_quadratico(dose, Y = resposta, verbose = FALSE)
#'
#' # Calcula DMET e DMEE
#' otimizacao <- otimizacao_insumos(
#'   modelo_quadratico = modelo_quad,
#'   preco_insumo = 10,
#'   preco_produto = 50,
#'   verbose = TRUE
#' )
#'
#' print(otimizacao)
#' }
#'
#' @export
otimizacao_insumos <- function(
  modelo_quadratico,
  preco_insumo,
  preco_produto,
  verbose = TRUE
) {
  # Evitar aviso de "no visible binding"
  resposta <- parametro <- valor <- dose <- Y <- NULL

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

    # Formatação de saída em tabela
    sep_line <- "───────────────────────────────────────────────────────────────────────"

    header <- "Resposta     DMET        Y_max       DMEE        Y_DMEE      Relação"

    # Prepara dados formatados
    output_rows <- apply(df_final, 1, function(row) {
      sprintf(
        "%-12s%-12s%-12s%-12s%-12s%-12s",
        row[1], # resposta
        sprintf("%.4f", as.numeric(row[2])), # DMET
        sprintf("%.4f", as.numeric(row[3])), # Y_max
        sprintf("%.4f", as.numeric(row[4])), # DMEE
        sprintf("%.4f", as.numeric(row[5])), # Y_DMEE
        sprintf("%.4f", as.numeric(row[6])) # Relação_Precos
      )
    })

    cat(sep_line, "\n")
    cat(header, "\n")
    cat(sep_line, "\n")
    cat(paste(output_rows, collapse = "\n"), "\n")
    cat(sep_line, "\n")

    # Aviso/Observações
    cat("\nObservações:\n")
    for (i in seq_len(nrow(df_final))) {
      cat(sprintf(
        "%s: %s | %s\n",
        df_final$resposta[i],
        df_final$Aviso_DMET[i],
        df_final$Aviso_DMEE[i]
      ))
    }

    # Criar gráfico de otimização
    cat("\n")

    # Extrair coeficientes do modelo para gerar a curva
    coefs_resp <- modelo_quadratico$resultados %>%
      dplyr::filter(resposta == df_final$resposta[1]) %>%
      tidyr::pivot_wider(names_from = parametro, values_from = valor)

    b0 <- coefs_resp$b0_intercepto
    b1 <- coefs_resp$b1_linear
    b2 <- coefs_resp$b2_quadratico

    # Gerar sequência de doses para a curva
    dose_range <- seq(0, df_final$DMET[1] * 1.2, length.out = 200)
    Y_curve <- b0 + b1 * dose_range + b2 * dose_range^2

    df_curva <- data.frame(
      dose = dose_range,
      Y = Y_curve,
      tipo = "Resposta"
    )

    # Criar gráfico
    p_otm <- ggplot2::ggplot(df_curva, ggplot2::aes(x = dose, y = Y)) +
      ggplot2::geom_line(linewidth = 1, color = "blue") +
      ggplot2::geom_point(
        data = data.frame(dose = df_final$DMET[1], Y = df_final$Y_max[1]),
        ggplot2::aes(x = dose, y = Y),
        color = "red",
        size = 4,
        shape = 17,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_vline(
        xintercept = df_final$DMET[1],
        linetype = "dashed",
        color = "red",
        alpha = 0.5
      ) +
      ggplot2::geom_point(
        data = data.frame(dose = df_final$DMEE[1], Y = df_final$Y_DMEE[1]),
        ggplot2::aes(x = dose, y = Y),
        color = "green",
        size = 4,
        shape = 15,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_vline(
        xintercept = df_final$DMEE[1],
        linetype = "dotted",
        color = "green",
        alpha = 0.5
      ) +
      ggplot2::labs(
        title = "Otimização de Uso de Insumos - DMET vs DMEE",
        subtitle = sprintf("Relação de Preços: %.4f", relacao_precos),
        x = "Dose",
        y = "Resposta (Y)",
        caption = "Triângulo Vermelho = DMET (Máxima Eficiência Técnica)\nQuadrado Verde = DMEE (Máxima Eficiência Econômica)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "top",
        plot.caption = ggplot2::element_text(hjust = 0, size = 9)
      )

    print(p_otm)
    grafico_otimizacao <- p_otm
  }

  return(list(
    resultados = df_final,
    grafico = grafico_otimizacao
  ))
}
