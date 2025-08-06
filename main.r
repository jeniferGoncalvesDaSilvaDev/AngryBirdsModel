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

# INTERVALOS DE CONFIANÇA (95%)
cat("=== INTERVALOS DE CONFIANÇA (95%) ===\n")
cat("--------------------------------------\n")

# IC para probabilidade
ic_prob <- t.test(p_model)$conf.int
cat(sprintf("Probabilidade: [%.3f, %.3f]\n", ic_prob[1], ic_prob[2]))

# IC para energia
ic_energia <- t.test(E_passaro)$conf.int
cat(sprintf("Energia (J): [%.1f, %.1f]\n", ic_energia[1], ic_energia[2]))

# IC para velocidade
ic_velocidade <- t.test(v0)$conf.int
cat(sprintf("Velocidade (m/s): [%.1f, %.1f]\n", ic_velocidade[1], ic_velocidade[2]))

# TESTE DE NORMALIDADE
cat("\n=== TESTE DE NORMALIDADE (Shapiro-Wilk) ===\n")
cat("--------------------------------------------\n")
shapiro_resultado <- shapiro.test(p_model)
cat(sprintf("H0: A distribuição da probabilidade é normal\n"))
cat(sprintf("Estatística W: %.6f\n", shapiro_resultado$statistic))
cat(sprintf("P-valor: %.6f\n", shapiro_resultado$p.value))
if (shapiro_resultado$p.value < 0.05) {
  cat("Conclusão: REJEITA H0 - Distribuição NÃO é normal (p < 0.05)\n")
} else {
  cat("Conclusão: NÃO REJEITA H0 - Distribuição pode ser normal (p ≥ 0.05)\n")
}

# TESTE T
cat("\n=== TESTE T (Média vs 0.5) ===\n")
cat("-------------------------------\n")
t_resultado <- t.test(p_model, mu = 0.5)
cat(sprintf("H0: μ = 0.5 (probabilidade média = 50%%)\n"))
cat(sprintf("Estatística t: %.3f\n", t_resultado$statistic))
cat(sprintf("Graus de liberdade: %d\n", t_resultado$parameter))
cat(sprintf("P-valor: %.6f\n", t_resultado$p.value))
cat(sprintf("Média observada: %.3f\n", t_resultado$estimate))
if (t_resultado$p.value < 0.05) {
  cat("Conclusão: REJEITA H0 - Média significativamente diferente de 0.5 (p < 0.05)\n")
} else {
  cat("Conclusão: NÃO REJEITA H0 - Média não difere significativamente de 0.5 (p ≥ 0.05)\n")
}

# ANOVA
cat("\n=== ANOVA (Diferenças entre tipos de pássaros) ===\n")
cat("---------------------------------------------------\n")
anova_resultado <- aov(p ~ passaro, data = df)
anova_summary <- summary(anova_resultado)
cat(sprintf("H0: Não há diferenças entre os tipos de pássaros\n"))
cat(sprintf("Estatística F: %.3f\n", anova_summary[[1]]$`F value`[1]))
cat(sprintf("Graus de liberdade: %d, %d\n", 
            anova_summary[[1]]$Df[1], anova_summary[[1]]$Df[2]))
cat(sprintf("P-valor: %.6f\n", anova_summary[[1]]$`Pr(>F)`[1]))

if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("Conclusão: REJEITA H0 - Existem diferenças significativas entre tipos (p < 0.05)\n")
} else {
  cat("Conclusão: NÃO REJEITA H0 - Não há diferenças significativas entre tipos (p ≥ 0.05)\n")
}

# Médias por tipo de pássaro
cat("\nMÉDIAS POR TIPO DE PÁSSARO:\n")
medias_tipos <- aggregate(p ~ passaro, data = df, FUN = mean)
for (i in 1:nrow(medias_tipos)) {
  cat(sprintf("%s: %.3f\n", medias_tipos$passaro[i], medias_tipos$p[i]))
}

# TEOREMA DE BAYES
cat("\n=== TEOREMA DE BAYES ===\n")
cat("-------------------------\n")

# Calcular probabilidades
tipos_unicos <- unique(passaros)
resultado <- data.frame(
  Passaro = character(),
  P_sucesso_dado_passaro = numeric(),
  P_passaro = numeric(),
  P_passaro_dado_sucesso = numeric(),
  stringsAsFactors = FALSE
)

# Define sucesso como p > 0.5
sucesso <- p_model > 0.5

for (tipo in tipos_unicos) {
  # P(sucesso|passaro)
  indices_tipo <- which(passaros == tipo)
  p_sucesso_dado_passaro <- mean(sucesso[indices_tipo])
  
  # P(passaro)
  p_passaro <- length(indices_tipo) / n
  
  # P(passaro|sucesso) usando Teorema de Bayes
  # P(A|B) = P(B|A) * P(A) / P(B)
  p_sucesso <- mean(sucesso)
  if (p_sucesso > 0) {
    p_passaro_dado_sucesso <- (p_sucesso_dado_passaro * p_passaro) / p_sucesso
  } else {
    p_passaro_dado_sucesso <- 0
  }
  
  resultado <- rbind(resultado, data.frame(
    Passaro = tipo,
    P_sucesso_dado_passaro = p_sucesso_dado_passaro,
    P_passaro = p_passaro,
    P_passaro_dado_sucesso = p_passaro_dado_sucesso
  ))
}

cat("TABELA DO TEOREMA DE BAYES (Sucesso = P > 0.5):\n")
cat("Pássaro\t\tP(S|P)\t\tP(P)\t\tP(P|S)\n")
cat("-------\t\t------\t\t----\t\t------\n")
for (i in 1:nrow(resultado)) {
  cat(sprintf("%-10s\t%.3f\t\t%.3f\t\t%.3f\n", 
              resultado$Passaro[i], 
              resultado$P_sucesso_dado_passaro[i],
              resultado$P_passaro[i],
              resultado$P_passaro_dado_sucesso[i]))
}

cat("\nLegenda:\n")
cat("P(S|P) = Probabilidade de sucesso dado o tipo de pássaro\n")
cat("P(P) = Probabilidade a priori do tipo de pássaro\n")
cat("P(P|S) = Probabilidade do tipo de pássaro dado o sucesso\n")

cat("\n===============================================\n")