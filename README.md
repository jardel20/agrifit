# agrifit <small>`0.1.0`</small>

## Nonlinear Model Fitting for Agricultural Experiments

**agrifit** ajusta modelos não-lineares para experimentos agrícolas de dose-resposta (fertilizantes, defensivos, etc.). Implementa o modelo **Linear Response Plateau (LRP)** com detecção automática de breakpoint pelo maior R², suporte a múltiplas respostas simultâneas e gráficos profissionais (ggplot2 + plotly interativo).

**Oferece duas versões da função de ajuste:**
- `ajustar_lrp()` - Versão padrão com estimativas básicas
- `ajustar_lrp2()` - Versão expandida com estatísticas avançadas (AIC, BIC, RMSE, testes de significância)

### ✨ **Instalação**

```r
# Do GitHub (versão atual)
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jardel20/agrifit")
```

```r
library(agrifit)
```

### 📊 **Exemplo Básico com `ajustar_lrp()`**

```r
# Dados: Matéria seca vs Dose de fósforo
dose_P <- c(0.0, 32.5, 65.0, 97.5, 130.0, 195.0)
materia_seca <- c(6.74, 8.73, 10.89, 12.56, 14.11, 15.21)

# Ajusta LRP automaticamente (versão padrão)
resultado <- ajustar_lrp(
  dose = dose_P,
  MS = materia_seca,
  title = "Matéria Seca vs Dose de Fósforo",
  xlab = "P (mg/dm³)",
  ylab = "MS (g/vaso)"
)
```

**Saídas automáticas:**
```
══ Ajuste LRP Múltiplo | LRP - Linear Response Plateau ══
Dose: 6 pontos | Respostas: 1 (MS)

------------------------ MS ------------------------
R²: 0.9923
Ponto de Intersecção (Xi): 142.35
Ponto de Quebra na Tabela (Xk): 130.00 (idx 5)
Equação:
MS: Ŷ = 6.5124 + 0.0547X (X < 142.35); Ŷ = 15.2147

Gráfico salvo como: 'lrp_multiplo.png'
```

**Gráficos gerados:**
- `lrp_multiplo.png` (12x8 polegadas, 300 DPI)
- Plotly interativo na tela

### 🔬 **Exemplo Múltiplas Respostas com `ajustar_lrp()`**

```r
# Duas respostas simultâneas
raizes <- c(2.1, 3.2, 4.5, 5.1, 5.2, 5.3)

ajustar_lrp(
  dose = dose_P,
  MS = materia_seca,
  Raiz = raizes,
  adjustment_color = "Dark2"
)
```

### 🔧 **Exemplo Avançado com `ajustar_lrp2()` (com Estatísticas Completas)**

```r
# Dados com mais pontos para melhor estimativa de significância
dose_P <- c(0.0, 32.5, 65.0, 97.5, 130.0, 195.0, 260.0, 325.0)
materia_seca <- c(6.74, 8.73, 10.89, 12.56, 14.11, 15.21, 15.50, 15.60)

# Ajusta LRP com estatísticas avançadas
resultado_adv <- ajustar_lrp2(
  dose = dose_P,
  MS = materia_seca,
  title = "Matéria Seca vs Dose de Fósforo",
  xlab = "P (mg/dm³)",
  ylab = "MS (g/vaso)",
  dashed = TRUE,
  show_intersection = TRUE
)
```

**Saídas automáticas (versão expandida):**
```
══ Ajuste LRP Múltiplo | LRP - Linear Response Plateau ══
Dose: 8 pontos | Respostas: 1 (MS)

------------------------ MS ------------------------
R²: 0.9923 | RMSE: 0.3214 | AIC: -18.52 | BIC: -14.89
Ponto de Intersecção (Xi): 142.35
Ponto de Quebra na Tabela (Xk): 130.00 (idx 5)
Equação:
MS: Ŷ = 6.5124 + 0.0547X (X < 142.35); Ŷ = 15.2147

             Estimate Std. Error t value Pr(>|t|)    
(Intercepto)    6.5124      0.1234   52.78  0.0001 ***
X               0.0547      0.0012   45.58  0.0002 ***
Plateau (b2)   15.2147      0.0856  177.62  < .0001 ***

Graus de Liberdade Residual (Linear): 2
Graus de Liberdade Residual (Plateau): 3

Gráfico salvo como: 'lrp_multiplo.png'
```

### 🎯 **Comparação das Duas Funções**

| Aspecto | `ajustar_lrp()` | `ajustar_lrp2()` |
|---------|------------------|------------------|
| **R²** | ✅ | ✅ |
| **Parâmetros (b0, b1, b2)** | ✅ | ✅ |
| **Ponto de Intersecção (Xi)** | ✅ | ✅ |
| **Gráficos (ggplot2 + plotly)** | ✅ | ✅ |
| **RMSE** | ❌ | ✅ |
| **AIC / BIC** | ❌ | ✅ |
| **P-valores** | ❌ | ✅ |
| **Erro Padrão (SE)** | ❌ | ✅ |
| **t-valores** | ❌ | ✅ |
| **Tabela de Coeficientes** | ❌ | ✅ |
| **Ideal para** | Exploração rápida | Análises detalhadas |
| **Requisitos mínimos** | N ≥ 4 | N ≥ 5 (recomendado) |

### 🎛️ **Parâmetros Personalizáveis**

| Parâmetro | Descrição | Padrão |
|-----------|-----------|---------|
| `title` | Título do gráfico | `"Linear Response Plateau (LRP)"` |
| `xlab` | Rótulo eixo X | `"Dose"` |
| `ylab` | Rótulo eixo Y | `"Response"` |
| `adjustment_color` | Paleta de cores | `"Set1"` |
| `dashed` | Linhas tracejadas | `FALSE` |
| `show_intersection` | Mostra Xi | `TRUE` |
| `verbose` | Gráficos/prints | `TRUE` |

### 📈 **Retorno da Função**

```r
str(resultado)
```
```
List of 6
 $ resultados     :Data frame com b0, b1, b2, Xi, R²
 $ modelos        :Lista com parâmetros de cada modelo
 $ equacoes       :Strings formatadas das equações
 $ nomes_respostas:Character vector
 $ grafico_ggplot :Objeto ggplot2
 $ grafico_plotly :Objeto plotly
```

### 🔍 **Visualização dos Resultados**

```r
# Tabela de parâmetros
print(resultado$resultados)

# Equações
print(resultado$equacoes)

# Gráfico ggplot
print(resultado$grafico_ggplot)

# Gráfico interativo
resultado$grafico_plotly
```

## 🛠️ **Dependências**

| Pacote | Uso |
|--------|-----|
| `ggplot2` | Gráficos estáticos profissionais |
| `plotly` | Gráficos interativos |
| `dplyr` | Manipulação de dados |
| `tidyr` | Pivot para múltiplas respostas |

## 🚀 **Roadmap**

- [x] Modelo LRP (funcional)
- [x] Múltiplas respostas
- [x] Gráficos ggplot2 + plotly
- [ ] Modelos: Quadrático, Mitscherlich, Gompertz
- [ ] Seleção automática de melhor modelo
- [ ] Intervalos de confiança (bootstrap)
- [ ] Exportação para Word/LaTeX

### 📚 **Documentação**

```r
?ajustar_lrp
?ajustar_lrp2
```

## 📄 **Licença**

Este projeto está sob a licença [MIT](LICENSE). Veja o arquivo [LICENSE](LICENSE) para detalhes.

## 🙏 **Contato**

- **Autor**: Jardel Fialho
- **Email**: jardelllfialho@gmail.com
- **GitHub**: [jardel20](https://github.com/jardel20)

---

**agrifit** © 2025 Jardel Fialho. Construído para ciência agrícola.
