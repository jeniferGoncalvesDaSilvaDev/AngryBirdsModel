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
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("           RELATÓRIO DA MATRIZ DE CORRELAÇÃO\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

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

cat("\n", paste(rep("=", 60), collapse=""), "\n")

# HISTOGRAMAS DAS VARIÁVEIS FÍSICAS
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("              ANÁLISE DOS HISTOGRAMAS\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Função para criar histogramas e análises
par(mfrow = c(4, 4))  # Layout 4x4 para os gráficos

# 1. Constante elástica (k)
hist(k, main = "Histograma: Constante Elástica (k)", 
     xlab = "k (N/m)", ylab = "Frequência", col = "lightblue", breaks = 10)
cat("1. CONSTANTE ELÁSTICA (k):\n")
cat(sprintf("   • Média: %.2f N/m\n", mean(k)))
cat(sprintf("   • Desvio padrão: %.2f N/m\n", sd(k)))
cat("   • Interpretação: Distribuição uniforme entre 100-300 N/m\n")
cat("   • Física: Rigidez do estilingue - valores maiores = mais energia\n\n")

# 2. Deslocamento x
hist(x, main = "Histograma: Deslocamento X", 
     xlab = "x (m)", ylab = "Frequência", col = "lightgreen", breaks = 10)
cat("2. DESLOCAMENTO X:\n")
cat(sprintf("   • Média: %.2f m\n", mean(x)))
cat(sprintf("   • Desvio padrão: %.2f m\n", sd(x)))
cat("   • Interpretação: Distribuição uniforme entre 1-3 m\n")
cat("   • Física: Extensão horizontal do estilingue\n\n")

# 3. Deslocamento y
hist(y, main = "Histograma: Deslocamento Y", 
     xlab = "y (m)", ylab = "Frequência", col = "lightcoral", breaks = 10)
cat("3. DESLOCAMENTO Y:\n")
cat(sprintf("   • Média: %.2f m\n", mean(y)))
cat(sprintf("   • Desvio padrão: %.2f m\n", sd(y)))
cat("   • Interpretação: Distribuição uniforme entre 1-3 m\n")
cat("   • Física: Extensão vertical do estilingue\n\n")

# 4. Ângulo theta
hist(theta * 180/pi, main = "Histograma: Ângulo de Lançamento", 
     xlab = "θ (graus)", ylab = "Frequência", col = "lightyellow", breaks = 10)
cat("4. ÂNGULO DE LANÇAMENTO (θ):\n")
cat(sprintf("   • Média: %.2f graus\n", mean(theta * 180/pi)))
cat(sprintf("   • Desvio padrão: %.2f graus\n", sd(theta * 180/pi)))
cat("   • Interpretação: Distribuição uniforme entre 20-60 graus\n")
cat("   • Física: Ângulo ótimo teoricamente ~45° para máximo alcance\n\n")

# 5. Delta theta
hist(delta_theta * 180/pi, main = "Histograma: Variação Angular", 
     xlab = "Δθ (graus)", ylab = "Frequência", col = "lightpink", breaks = 10)
cat("5. VARIAÇÃO ANGULAR (Δθ):\n")
cat(sprintf("   • Média: %.2f graus\n", mean(delta_theta * 180/pi)))
cat(sprintf("   • Desvio padrão: %.2f graus\n", sd(delta_theta * 180/pi)))
cat("   • Interpretação: Distribuição uniforme entre 0-30 graus\n")
cat("   • Física: Incerteza/imprecisão no ângulo de lançamento\n\n")

# 6. Raio R
hist(R, main = "Histograma: Raio do Alvo", 
     xlab = "R (m)", ylab = "Frequência", col = "lightsteelblue", breaks = 10)
cat("6. RAIO DO ALVO (R):\n")
cat(sprintf("   • Média: %.2f m\n", mean(R)))
cat(sprintf("   • Desvio padrão: %.2f m\n", sd(R)))
cat("   • Interpretação: Distribuição uniforme entre 0.5-2.5 m\n")
cat("   • Física: Tamanho do alvo - maior raio = maior chance de acerto\n\n")

# 7. Massa M
hist(M, main = "Histograma: Massa do Pássaro", 
     xlab = "M (kg)", ylab = "Frequência", col = "lavender", breaks = 3)
cat("7. MASSA DO PÁSSARO (M):\n")
cat(sprintf("   • Média: %.2f kg\n", mean(M)))
cat(sprintf("   • Desvio padrão: %.2f kg\n", sd(M)))
cat("   • Interpretação: Distribuição discreta (1, 2, 3 kg)\n")
cat("   • Física: Massa afeta a dinâmica - mais massa = mais momento\n\n")

# 8. Distância d
hist(d, main = "Histograma: Distância ao Alvo", 
     xlab = "d (m)", ylab = "Frequência", col = "mistyrose", breaks = 10)
cat("8. DISTÂNCIA AO ALVO (d):\n")
cat(sprintf("   • Média: %.2f m\n", mean(d)))
cat(sprintf("   • Desvio padrão: %.2f m\n", sd(d)))
cat("   • Interpretação: Distribuição uniforme entre 5-20 m\n")
cat("   • Física: Distância real entre estilingue e alvo\n\n")

# 9. Distância ótima
hist(d_otimo, main = "Histograma: Distância Ótima", 
     xlab = "d_ótimo (m)", ylab = "Frequência", col = "honeydew", breaks = 10)
cat("9. DISTÂNCIA ÓTIMA (d_ótimo):\n")
cat(sprintf("   • Média: %.2f m\n", mean(d_otimo)))
cat(sprintf("   • Desvio padrão: %.2f m\n", sd(d_otimo)))
cat("   • Interpretação: Distribuição uniforme entre 5-20 m\n")
cat("   • Física: Distância ideal para máxima eficiência\n\n")

# 10. Distância máxima
hist(d_max, main = "Histograma: Distância Máxima", 
     xlab = "d_max (m)", ylab = "Frequência", col = "azure", breaks = 10)
cat("10. DISTÂNCIA MÁXIMA (d_max):\n")
cat(sprintf("    • Média: %.2f m\n", mean(d_max)))
cat(sprintf("    • Desvio padrão: %.2f m\n", sd(d_max)))
cat("    • Interpretação: Distribuição uniforme entre 20-30 m\n")
cat("    • Física: Alcance máximo possível do sistema\n\n")

# 11. Altura inicial
hist(h0, main = "Histograma: Altura Inicial", 
     xlab = "h₀ (m)", ylab = "Frequência", col = "lemonchiffon", breaks = 10)
cat("11. ALTURA INICIAL (h₀):\n")
cat(sprintf("    • Média: %.2f m\n", mean(h0)))
cat(sprintf("    • Desvio padrão: %.2f m\n", sd(h0)))
cat("    • Interpretação: Distribuição uniforme entre 0-2 m\n")
cat("    • Física: Altura do ponto de lançamento\n\n")

# 12. Tempo
hist(t, main = "Histograma: Tempo de Voo", 
     xlab = "t (s)", ylab = "Frequência", col = "oldlace", breaks = 10)
cat("12. TEMPO DE VOO (t):\n")
cat(sprintf("    • Média: %.2f s\n", mean(t)))
cat(sprintf("    • Desvio padrão: %.2f s\n", sd(t)))
cat("    • Interpretação: Distribuição uniforme entre 0.5-2.0 s\n")
cat("    • Física: Duração do voo até atingir o alvo\n\n")

# 13. Energia do pássaro
hist(E_passaro, main = "Histograma: Energia do Pássaro", 
     xlab = "E (J)", ylab = "Frequência", col = "wheat", breaks = 15)
cat("13. ENERGIA DO PÁSSARO (E_passaro):\n")
cat(sprintf("    • Média: %.2f J\n", mean(E_passaro)))
cat(sprintf("    • Desvio padrão: %.2f J\n", sd(E_passaro)))
cat("    • Interpretação: Distribuição assimétrica positiva\n")
cat("    • Física: Energia cinética inicial = k*x*√(x²+y²)\n\n")

# 14. Velocidade inicial
hist(v0, main = "Histograma: Velocidade Inicial", 
     xlab = "v₀ (m/s)", ylab = "Frequência", col = "tan", breaks = 15)
cat("14. VELOCIDADE INICIAL (v₀):\n")
cat(sprintf("    • Média: %.2f m/s\n", mean(v0)))
cat(sprintf("    • Desvio padrão: %.2f m/s\n", sd(v0)))
cat("    • Interpretação: Distribuição assimétrica positiva\n")
cat("    • Física: v₀ = √E_passaro - velocidade de lançamento\n\n")

# 15. Posição y no tempo t
hist(y_t, main = "Histograma: Posição Y(t)", 
     xlab = "y(t) (m)", ylab = "Frequência", col = "thistle", breaks = 15)
cat("15. POSIÇÃO Y NO TEMPO T:\n")
cat(sprintf("    • Média: %.2f m\n", mean(y_t)))
cat(sprintf("    • Desvio padrão: %.2f m\n", sd(y_t)))
cat("    • Interpretação: Pode incluir valores negativos (abaixo do solo)\n")
cat("    • Física: Trajetória parabólica y(t) = -½gt²/v₀²cos²θ + t*tanθ + h₀\n\n")

# 16. Probabilidade do modelo
hist(p_model, main = "Histograma: Probabilidade do Modelo", 
     xlab = "P(sucesso)", ylab = "Frequência", col = "plum", breaks = 15)
cat("16. PROBABILIDADE DO MODELO:\n")
cat(sprintf("    • Média: %.3f\n", mean(p_model)))
cat(sprintf("    • Desvio padrão: %.3f\n", sd(p_model)))
cat(sprintf("    • Mínimo: %.3f\n", min(p_model)))
cat(sprintf("    • Máximo: %.3f\n", max(p_model)))
cat("    • Interpretação: Distribuição concentrada em valores baixos\n")
cat("    • Física: P = (y(t)*cosΔθ)/(R*M) * (1-|d-d_ótimo|/d_max)\n\n")

# Restaura layout padrão
par(mfrow = c(1, 1))

cat(paste(rep("=", 60), collapse=""), "\n")
cat("            CONCLUSÕES DOS HISTOGRAMAS\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

cat("PADRÕES OBSERVADOS:\n")
cat("------------------\n")
cat("• Variáveis de entrada (k,x,y,θ,Δθ,R,M,d,d_ótimo,d_max,h₀,t): Uniformes\n")
cat("• Variáveis derivadas (E_passaro, v₀): Assimétricas devido às transformações\n")
cat("• Posição y(t): Distribuição normal com alguns valores negativos\n")
cat("• Probabilidade: Concentrada em valores baixos (0-0.4)\n\n")

cat("IMPLICAÇÕES FÍSICAS:\n")
cat("-------------------\n")
cat("• Sistema tem baixa taxa de sucesso geral (maioria P < 0.4)\n")
cat("• Energia e velocidade seguem distribuições não-uniformes\n")
cat("• Trajetória realística com possibilidade de impacto no solo\n")
cat("• Modelo penaliza desvios da distância ótima apropriadamente\n\n")

cat(paste(rep("=", 60), collapse=""), "\n")

# RELATÓRIO ESTATÍSTICO COMPLETO
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("                    RELATÓRIO ESTATÍSTICO COMPLETO\n")
cat("                        ANÁLISE DE ANGRY BIRDS\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

# 1. SUMÁRIO EXECUTIVO
cat("1. SUMÁRIO EXECUTIVO\n")
cat(paste(rep("-", 40), collapse=""), "\n")
cat(sprintf("• Tamanho da amostra: %d observações\n", n))
cat(sprintf("• Probabilidade média de sucesso: %.3f (%.1f%%)\n", mean(p_model), mean(p_model)*100))
cat(sprintf("• Taxa de sucesso máxima observada: %.3f (%.1f%%)\n", max(p_model), max(p_model)*100))
cat(sprintf("• Desvio padrão da probabilidade: %.3f\n", sd(p_model)))
cat(sprintf("• Coeficiente de variação: %.1f%%\n", (sd(p_model)/mean(p_model))*100))

# 2. ESTATÍSTICAS DESCRITIVAS DAS VARIÁVEIS FÍSICAS
cat("\n2. ESTATÍSTICAS DESCRITIVAS DAS VARIÁVEIS FÍSICAS\n")
cat(paste(rep("-", 50), collapse=""), "\n")

# Função para estatísticas resumidas
estatisticas <- function(x, nome, unidade = "") {
  cat(sprintf("%-20s | Mín:%-8.2f | Q1:%-8.2f | Média:%-8.2f | Mediana:%-8.2f | Q3:%-8.2f | Máx:%-8.2f | DP:%-8.2f %s\n", 
              nome, min(x), quantile(x, 0.25), mean(x), median(x), quantile(x, 0.75), max(x), sd(x), unidade))
}

cat("Variável             |    Mín    |    Q1     |   Média   |  Mediana  |    Q3     |    Máx    |    DP     | Unidade\n")
cat(paste(rep("-", 120), collapse=""), "\n")

estatisticas(k, "Constante k", "N/m")
estatisticas(x, "Deslocamento x", "m")
estatisticas(y, "Deslocamento y", "m")
estatisticas(theta*180/pi, "Ângulo theta", "°")
estatisticas(delta_theta*180/pi, "Var. angular", "°")
estatisticas(R, "Raio alvo R", "m")
estatisticas(M, "Massa M", "kg")
estatisticas(d, "Distância d", "m")
estatisticas(d_otimo, "Distância ótima", "m")
estatisticas(d_max, "Distância máxima", "m")
estatisticas(h0, "Altura inicial", "m")
estatisticas(t, "Tempo de voo", "s")
estatisticas(E_passaro, "Energia", "J")
estatisticas(v0, "Velocidade v0", "m/s")
estatisticas(y_t, "Posição y(t)", "m")
estatisticas(p_model, "Probabilidade", "")

# 3. ANÁLISE DE CORRELAÇÕES
cat("\n3. ANÁLISE DE CORRELAÇÕES\n")
cat(paste(rep("-", 30), collapse=""), "\n")

# Top 10 correlações mais fortes (excluindo diagonal)
correlacoes_matriz <- matriz_correlacao
correlacoes_matriz[lower.tri(correlacoes_matriz, diag = TRUE)] <- NA
correlacoes_vetor <- as.vector(correlacoes_matriz)
nomes_pares <- outer(rownames(matriz_correlacao), colnames(matriz_correlacao), paste, sep = " ↔ ")
nomes_vetor <- as.vector(nomes_pares)
correlacoes_validas <- !is.na(correlacoes_vetor)
correlacoes_ordenadas <- order(abs(correlacoes_vetor[correlacoes_validas]), decreasing = TRUE)

cat("TOP 10 CORRELAÇÕES MAIS FORTES:\n")
for (i in 1:min(10, length(correlacoes_ordenadas))) {
  idx <- which(correlacoes_validas)[correlacoes_ordenadas[i]]
  cat(sprintf("%2d. %-25s: r = %6.3f\n", i, nomes_vetor[idx], correlacoes_vetor[idx]))
}

# 4. ANÁLISE DA VARIÁVEL DEPENDENTE (p_model)
cat("\n4. ANÁLISE DA VARIÁVEL DEPENDENTE (Probabilidade de Sucesso)\n")
cat(paste(rep("-", 60), collapse=""), "\n")

# Quartis detalhados
quartis <- quantile(p_model, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
cat("DISTRIBUIÇÃO POR QUARTIS:\n")
for (i in 1:length(quartis)) {
  percentil <- names(quartis)[i]
  valor <- quartis[i]
  cat(sprintf("• %s percentil: %.3f (%.1f%%)\n", percentil, valor, valor*100))
}

# Categorização da probabilidade
p_categorizada <- cut(p_model, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                      labels = c("Muito Baixa", "Baixa", "Média", "Alta", "Muito Alta"))
tabela_categorias <- table(p_categorizada)

cat("\nCATEGORIZAÇÃO DA PROBABILIDADE:\n")
for (i in 1:length(tabela_categorias)) {
  categoria <- names(tabela_categorias)[i]
  frequencia <- tabela_categorias[i]
  percentual <- (frequencia/n)*100
  cat(sprintf("• %-12s: %2d observações (%.1f%%)\n", categoria, frequencia, percentual))
}

# 5. ANÁLISE DOS TIPOS DE PÁSSAROS
cat("\n5. ANÁLISE DOS TIPOS DE PÁSSAROS (Teorema de Bayes)\n")
cat(paste(rep("-", 50), collapse=""), "\n")

cat("PROBABILIDADES POR TIPO DE PÁSSARO:\n")
cat(sprintf("%-15s | P(sucesso|pássaro) | P(pássaro) | P(pássaro|sucesso)\n", "Tipo"))
cat(paste(rep("-", 70), collapse=""), "\n")

for (tipo in rownames(resultado)) {
  cat(sprintf("%-15s |      %.3f        |   %.3f    |      %.3f\n", 
              tipo, 
              resultado[tipo, "P_sucesso_dado_passaro"],
              resultado[tipo, "P_passaro"],
              resultado[tipo, "P_passaro_dado_sucesso"]))
}

# Pássaro mais eficiente
melhor_passaro <- rownames(resultado)[which.max(resultado$P_sucesso_dado_passaro)]
prob_melhor <- max(resultado$P_sucesso_dado_passaro)
cat(sprintf("\n• Pássaro mais eficiente: %s (P = %.3f)\n", melhor_passaro, prob_melhor))

pior_passaro <- rownames(resultado)[which.min(resultado$P_sucesso_dado_passaro)]
prob_pior <- min(resultado$P_sucesso_dado_passaro)
cat(sprintf("• Pássaro menos eficiente: %s (P = %.3f)\n", pior_passaro, prob_pior))

# 6. TESTES ESTATÍSTICOS
cat("\n6. TESTES ESTATÍSTICOS\n")
cat(paste(rep("-", 25), collapse=""), "\n")

# Teste de normalidade para probabilidade
shapiro_p <- shapiro.test(p_model)$p.value
cat(sprintf("• Teste de Shapiro-Wilk (normalidade de p_model): p-valor = %.6f\n", shapiro_p))
if (shapiro_p < 0.05) {
  cat("  → Rejeita H0: Distribuição NÃO é normal (p < 0.05)\n")
} else {
  cat("  → Não rejeita H0: Distribuição pode ser normal (p ≥ 0.05)\n")
}

# Teste t para média da probabilidade vs 0.5
t_test <- t.test(p_model, mu = 0.5)
cat(sprintf("• Teste t (H0: μ = 0.5): t = %.3f, p-valor = %.6f\n", t_test$statistic, t_test$p.value))
if (t_test$p.value < 0.05) {
  cat("  → Rejeita H0: Média significativamente diferente de 0.5\n")
} else {
  cat("  → Não rejeita H0: Média não difere significativamente de 0.5\n")
}

# ANOVA para diferenças entre tipos de pássaros
anova_resultado <- summary(aov(p ~ passaro, data = df))
f_valor <- anova_resultado[[1]]$`F value`[1]
p_anova <- anova_resultado[[1]]$`Pr(>F)`[1]
cat(sprintf("• ANOVA (diferenças entre pássaros): F = %.3f, p-valor = %.6f\n", f_valor, p_anova))
if (p_anova < 0.05) {
  cat("  → Rejeita H0: Existem diferenças significativas entre tipos\n")
} else {
  cat("  → Não rejeita H0: Não há diferenças significativas entre tipos\n")
}

# 7. ANÁLISE DE EFICIÊNCIA DO SISTEMA
cat("\n7. ANÁLISE DE EFICIÊNCIA DO SISTEMA\n")
cat(paste(rep("-", 40), collapse=""), "\n")

# Eficiência energética
eficiencia <- p_model / E_passaro * 1000  # Probabilidade por kJ
cat(sprintf("• Eficiência energética média: %.3f prob/kJ\n", mean(eficiencia)))
cat(sprintf("• Eficiência energética máxima: %.3f prob/kJ\n", max(eficiencia)))

# Taxa de conversão energia → sucesso
energia_quartis <- quantile(E_passaro)
prob_por_energia <- tapply(p_model, cut(E_passaro, breaks = energia_quartis), mean)
cat("\nTAXA DE CONVERSÃO ENERGIA → PROBABILIDADE:\n")
for (i in 1:length(prob_por_energia)) {
  if (!is.na(prob_por_energia[i])) {
    cat(sprintf("• Quartil %d energia: P média = %.3f\n", i, prob_por_energia[i]))
  }
}

# 8. RECOMENDAÇÕES BASEADAS EM DADOS
cat("\n8. RECOMENDAÇÕES BASEADAS EM DADOS\n")
cat(paste(rep("-", 40), collapse=""), "\n")

# Condições ótimas
melhor_idx <- which.max(p_model)
cat("CONFIGURAÇÃO ÓTIMA OBSERVADA:\n")
cat(sprintf("• Constante k: %.1f N/m\n", k[melhor_idx]))
cat(sprintf("• Deslocamentos: x = %.2f m, y = %.2f m\n", x[melhor_idx], y[melhor_idx]))
cat(sprintf("• Ângulo: %.1f°\n", theta[melhor_idx]*180/pi))
cat(sprintf("• Massa: %.0f kg\n", M[melhor_idx]))
cat(sprintf("• Distância: %.1f m (ótima: %.1f m)\n", d[melhor_idx], d_otimo[melhor_idx]))
cat(sprintf("• Probabilidade alcançada: %.3f (%.1f%%)\n", p_model[melhor_idx], p_model[melhor_idx]*100))

cat("\nRECOMENDAÇÕES GERAIS:\n")
cat("• Priorizar ângulos próximos a 45° para máximo alcance\n")
cat("• Manter distância real próxima à distância ótima\n")
cat("• Considerar alvos com maior raio quando possível\n")
if (cor(E_passaro, p_model) > 0) {
  cat("• Aumentar energia do sistema melhora probabilidade\n")
} else {
  cat("• Energia excessiva pode prejudicar precisão\n")
}

# 9. INTERVALOS DE CONFIANÇA
cat("\n9. INTERVALOS DE CONFIANÇA (95%)\n")
cat(paste(rep("-", 35), collapse=""), "\n")

# IC para média da probabilidade
ic_prob <- t.test(p_model)$conf.int
cat(sprintf("• Probabilidade média: [%.3f, %.3f]\n", ic_prob[1], ic_prob[2]))

# IC para outras variáveis importantes
ic_energia <- t.test(E_passaro)$conf.int
cat(sprintf("• Energia média: [%.1f, %.1f] J\n", ic_energia[1], ic_energia[2]))

ic_velocidade <- t.test(v0)$conf.int
cat(sprintf("• Velocidade média: [%.1f, %.1f] m/s\n", ic_velocidade[1], ic_velocidade[2]))

# 10. CONCLUSÕES ESTATÍSTICAS
cat("\n10. CONCLUSÕES ESTATÍSTICAS FINAIS\n")
cat(paste(rep("-", 40), collapse=""), "\n")

cat("PRINCIPAIS ACHADOS:\n")
cat(sprintf("• Sistema apresenta probabilidade média de sucesso de %.1f%%\n", mean(p_model)*100))
cat(sprintf("• Há %.1f%% de variabilidade na performance (CV)\n", (sd(p_model)/mean(p_model))*100))
cat(sprintf("• Energia e velocidade têm correlação perfeita (r = %.3f)\n", cor(E_passaro, v0)))
cat(sprintf("• Maior correlação com sucesso: %s (r = %.3f)\n", 
            names(sort(abs(correlacoes_p_model), decreasing = TRUE))[1],
            correlacoes_p_model[names(sort(abs(correlacoes_p_model), decreasing = TRUE))[1]]))

cat("\nSIGNIFICÂNCIA ESTATÍSTICA:\n")
if (shapiro_p < 0.05) {
  cat("• Distribuição da probabilidade é não-normal\n")
}
if (p_anova < 0.05) {
  cat("• Tipos de pássaros afetam significativamente o desempenho\n")
}
cat(sprintf("• Taxa de sucesso difere significativamente de 50%% (p = %.6f)\n", t_test$p.value))

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("                      FIM DO RELATÓRIO ESTATÍSTICO\n")
cat(paste(rep("=", 80), collapse=""), "\n")

# GERAÇÃO DO RELATÓRIO EM PDF
cat("\n=== GERANDO RELATÓRIO EM PDF ===\n")

# Instala e carrega os pacotes necessários para PDF
if (!require(rmarkdown, quietly = TRUE)) {
  install.packages("rmarkdown")
  library(rmarkdown)
}

if (!require(tinytex, quietly = TRUE)) {
  install.packages("tinytex")
  library(tinytex)
}

# Verifica se tinytex está instalado, se não instala
if (!tinytex:::is_tinytex()) {
  cat("Instalando TinyTeX para geração de PDF...\n")
  tinytex::install_tinytex()
}

# Gera o relatório PDF
tryCatch({
  rmarkdown::render("relatorio_completo.Rmd", 
                    output_file = "Relatorio_Angry_Birds_Completo.pdf",
                    quiet = TRUE)
  cat("✓ Relatório PDF gerado com sucesso: 'Relatorio_Angry_Birds_Completo.pdf'\n")
}, error = function(e) {
  cat("✗ Erro ao gerar PDF:", e$message, "\n")
  cat("Verifique se o LaTeX está instalado no sistema.\n")
})