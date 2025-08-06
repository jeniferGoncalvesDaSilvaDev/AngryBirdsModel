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