set.seed(42)
n <- 100

# Dados físicos
k <- runif(n, 100, 300)
x <- runif(n, 1, 3)
y <- runif(n, 1, 3)
theta <- runif(n, 20, 60) * pi/180  # em radianos
delta_theta <- runif(n, 0, pi/6)
R <- runif(n, 0.5, 2.5)
M <- sample(c(1, 2, 3), n, replace = TRUE)
d <- runif(n, 5, 20)
d_otimo <- runif(n, 5, 20)
d_max <- runif(n, 20, 30)
g <- 9.8
h0 <- runif(n, 0, 2)
t <- runif(n, 0.5, 2.0)

# Energia elástica
E_passaro <- k * x * sqrt(x^2 + y^2)
v0 <- sqrt(E_passaro)

# Substituição: y(t)
y_t <- -(g / (2 * v0^2 * cos(theta)^2)) * t^2 + tan(theta) * t + h0

# Probabilidade física (modelo)
z <- (y_t * cos(delta_theta)) / (R * M) * (1 - abs(d - d_otimo)/d_max)
p_model <- pmin(1, z)

# Tipos de pássaros
passaros <- sample(c("vermelho", "azul", "preto", "grande", "branco", "cinza"), n, replace = TRUE)

# Construção do dataframe
df <- data.frame(passaro = passaros, p = p_model)

# P(sucesso) geral
P_sucesso <- mean(df$p)

# P(passaro)
P_passaro <- table(df$passaro) / n

# P(sucesso | passaro)
P_sucesso_dado_passaro <- tapply(df$p, df$passaro, mean)

# Teorema de Bayes: P(passaro | sucesso)
# Evita divisão por zero
if (P_sucesso > 0) {
  P_passaro_dado_sucesso <- (P_sucesso_dado_passaro * P_passaro) / P_sucesso
} else {
  P_passaro_dado_sucesso <- rep(0, length(P_passaro))
  names(P_passaro_dado_sucesso) <- names(P_passaro)
}

# Resultado final
resultado <- data.frame(
  P_sucesso_dado_passaro = as.numeric(P_sucesso_dado_passaro),
  P_passaro = as.numeric(P_passaro),
  P_passaro_dado_sucesso = as.numeric(P_passaro_dado_sucesso)
)

# Adiciona os nomes dos pássaros como row names
rownames(resultado) <- names(P_passaro)

#resultado <- resultado[order(-resultado$P_passaro_dado_sucesso), ]
print(round(resultado, 3))

# Matriz de correlação das variáveis físicas
variaveis_fisicas <- data.frame(
  k = k,
  x = x,
  y = y,
  theta = theta,
  delta_theta = delta_theta,
  R = R,
  M = M,
  d = d,
  d_otimo = d_otimo,
  d_max = d_max,
  h0 = h0,
  t = t,
  E_passaro = E_passaro,
  v0 = v0,
  y_t = y_t,
  p_model = p_model
)

# Calcula a matriz de correlação
matriz_correlacao <- cor(variaveis_fisicas)

# Exibe a matriz de correlação
cat("\n=== MATRIZ DE CORRELAÇÃO DAS VARIÁVEIS FÍSICAS ===\n")
print(round(matriz_correlacao, 3))

# Instala e carrega o pacote corrplot se necessário
if (!require(corrplot, quietly = TRUE)) {
  install.packages("corrplot")
  library(corrplot)
}

# Cria um heatmap da matriz de correlação
corrplot(matriz_correlacao, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black", 
         title = "Matriz de Correlação - Variáveis Físicas",
         mar = c(0,0,1,0))

# RELATÓRIO DETALHADO DA MATRIZ DE CORRELAÇÃO
cat("\n" , "="*60, "\n")
cat("           RELATÓRIO DA MATRIZ DE CORRELAÇÃO\n")
cat("="*60, "\n\n")

# Encontra correlações fortes (|r| > 0.7)
correlacoes_fortes <- which(abs(matriz_correlacao) > 0.7 & matriz_correlacao != 1, arr.ind = TRUE)
cat("CORRELAÇÕES FORTES (|r| > 0.7):\n")
cat("--------------------------------\n")
if (nrow(correlacoes_fortes) > 0) {
  for (i in 1:nrow(correlacoes_fortes)) {
    var1 <- rownames(matriz_correlacao)[correlacoes_fortes[i,1]]
    var2 <- colnames(matriz_correlacao)[correlacoes_fortes[i,2]]
    valor <- round(matriz_correlacao[correlacoes_fortes[i,1], correlacoes_fortes[i,2]], 3)
    cat(sprintf("• %s ↔ %s: r = %s\n", var1, var2, valor))
  }
} else {
  cat("Nenhuma correlação forte encontrada.\n")
}

# Encontra correlações moderadas (0.5 < |r| ≤ 0.7)
correlacoes_moderadas <- which(abs(matriz_correlacao) > 0.5 & abs(matriz_correlacao) <= 0.7, arr.ind = TRUE)
cat("\nCORRELAÇÕES MODERADAS (0.5 < |r| ≤ 0.7):\n")
cat("--------------------------------------\n")
if (nrow(correlacoes_moderadas) > 0) {
  for (i in 1:nrow(correlacoes_moderadas)) {
    var1 <- rownames(matriz_correlacao)[correlacoes_moderadas[i,1]]
    var2 <- colnames(matriz_correlacao)[correlacoes_moderadas[i,2]]
    valor <- round(matriz_correlacao[correlacoes_moderadas[i,1], correlacoes_moderadas[i,2]], 3)
    cat(sprintf("• %s ↔ %s: r = %s\n", var1, var2, valor))
  }
} else {
  cat("Nenhuma correlação moderada encontrada.\n")
}

# Análise das correlações com p_model (variável dependente)
cat("\nCORRELAÇÕES COM P_MODEL (Probabilidade):\n")
cat("---------------------------------------\n")
correlacoes_p_model <- matriz_correlacao[,"p_model"]
correlacoes_p_model <- correlacoes_p_model[names(correlacoes_p_model) != "p_model"]
correlacoes_p_model_ordenadas <- sort(abs(correlacoes_p_model), decreasing = TRUE)

for (i in 1:length(correlacoes_p_model_ordenadas)) {
  var <- names(correlacoes_p_model_ordenadas)[i]
  valor <- round(correlacoes_p_model[var], 3)
  interpretacao <- ifelse(abs(valor) > 0.7, "(FORTE)", 
                   ifelse(abs(valor) > 0.5, "(MODERADA)", 
                   ifelse(abs(valor) > 0.3, "(FRACA)", "(MUITO FRACA)")))
  cat(sprintf("• %s: r = %s %s\n", var, valor, interpretacao))
}

# Resumo estatístico
cat("\nRESUMO ESTATÍSTICO:\n")
cat("-------------------\n")
correlacoes_sem_diagonal <- matriz_correlacao[upper.tri(matriz_correlacao)]
cat(sprintf("• Correlação média: %.3f\n", mean(correlacoes_sem_diagonal)))
cat(sprintf("• Desvio padrão: %.3f\n", sd(correlacoes_sem_diagonal)))
cat(sprintf("• Correlação máxima: %.3f\n", max(correlacoes_sem_diagonal)))
cat(sprintf("• Correlação mínima: %.3f\n", min(correlacoes_sem_diagonal)))
cat(sprintf("• Total de variáveis: %d\n", ncol(matriz_correlacao)))
cat(sprintf("• Total de correlações analisadas: %d\n", length(correlacoes_sem_diagonal)))

# Interpretações físicas importantes
cat("\nINTERPRETAÇÕES FÍSICAS RELEVANTES:\n")
cat("----------------------------------\n")
cat("• E_passaro vs v0: Relação direta esperada (energia → velocidade)\n")
cat("• theta vs y_t: Ângulo de lançamento influencia trajetória\n")
cat("• h0 vs y_t: Altura inicial afeta posição final\n")
cat("• R vs p_model: Raio pode afetar probabilidade de sucesso\n")
cat("• M vs p_model: Massa influencia dinâmica do sistema\n")

cat("\n" , "="*60, "\n")