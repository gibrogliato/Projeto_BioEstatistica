#Vamos analisar os casos de doença cardiovascular, diabetes, depressão, hipertensão,
#colesterol total, LDL, HDL e triglicerideos. 

#Doença Cardiovascular
media_card <- mean(dados_alzheimer$CardiovascularDisease)
cat(media_card)
mediana_card <- median(dados_alzheimer$CardiovascularDisease)
cat(mediana_card)
moda_card <- as.numeric(names(sort(table(dados_alzheimer$CardiovascularDisease), decreasing = TRUE)[1]))
cat(moda_card)
#Medidas de Dispersão
dp_card <- sd(dados_alzheimer$CardiovascularDisease)
cat(dp_card)
var_card <- var(dados_alzheimer$CardiovascularDisease)
cat(var_card)
cv_card <- (dp_card/media_card)*100
cat("CV =", round(cv_card, 2),"%")
#Visualizando
# Proporção dos valores
prop_dc <- prop.table(table(dados_alzheimer$CardiovascularDisease))
barplot(prop,
        main = "Proporção de Doenças Cardiovasculares",
        names.arg = c("Negativo", "Positivo"),
        col = c("lightblue", "salmon"),
        ylab = "Proporção")

#Diabetes
media_diabetes <- mean(dados_alzheimer$Diabetes)
cat(media_diabetes)
mediana_diabetes <- median(dados_alzheimer$Diabetes)
cat(mediana_diabetes)
moda_diabetes <- as.numeric(names(sort(table(dados_alzheimer$Diabetes), decreasing = TRUE)[1]))
cat(moda_diabetes)
#Medidas de Dispersão
dp_diabetes <- sd(dados_alzheimer$Diabetes)
cat(dp_diabetes)
var_diabetes <- var(dados_alzheimer$Diabetes)
cat(var_diabetes)
cv_diabetes <- (dp_diabetes/media_diabetes)*100
cat("CV =", round(cv_diabetes, 2),"%")
#Visualizando
# Proporção dos valores
prop_diab <- prop.table(table(dados_alzheimer$Diabetes))
barplot(prop,
        main = "Proporção de Pacientes com Diagnóstico de Diabetes",
        names.arg = c("Negativo", "Positivo"),
        col = c("lightblue", "salmon"),
        ylab = "Proporção")

#Depressão
media_depressao <- mean(dados_alzheimer$Depression)
cat(media_depressao)
mediana_depressao <- median(dados_alzheimer$Depression)
cat(mediana_depressao)
moda_depressao <- as.numeric(names(sort(table(dados_alzheimer$Depression), decreasing = TRUE)[1]))
cat(moda_depressao)
#Medidas de Dispersão
dp_depressao <- sd(dados_alzheimer$Depression)
cat(dp_depressao)
var_depressao <- var(dados_alzheimer$Depression)
cat(var_depressao)
cv_depressao <- (dp_depressao/media_depressao)*100
cat("CV =", round(cv_depressao, 2),"%")
#Visualizando
# Proporção dos valores
prop_depressao <- prop.table(table(dados_alzheimer$Depression))
barplot(prop,
        main = "Proporção de Pacientes com Diagnóstico de Depressão",
        names.arg = c("Negativo", "Positivo"),
        col = c("lightblue", "salmon"),
        ylab = "Proporção")

#Hipertensão
media_hipertensao <- mean(dados_alzheimer$Hypertension)
cat(media_hipertensao)
mediana_hipertensao <- median(dados_alzheimer$Hypertension)
cat(mediana_hipertensao)
moda_hipertensao <- as.numeric(names(sort(table(dados_alzheimer$Hypertension), decreasing = TRUE)[1]))
cat(moda_hipertensao)
#Medidas de Dispersão
dp_hipertensao <- sd(dados_alzheimer$Hypertension)
cat(dp_hipertensao)
var_hipertensao <- var(dados_alzheimer$Hypertension)
cat(var_hipertensao)
cv_hipertensao <- (dp_hipertensao/media_hipertensao)*100
cat("CV =", round(cv_hipertensao, 2),"%")
#Visualizando
# Proporção dos valores
prop_hipertensao <- prop.table(table(dados_alzheimer$Hypertension))
barplot(prop,
        main = "Proporção de Pacientes com Diagnóstico de Hipertensão",
        names.arg = c("Negativo", "Positivo"),
        col = c("lightblue", "salmon"),
        ylab = "Proporção")

#Colesterol Total
summary(dados_alzheimer$CholesterolTotal)
media_colt <- mean(dados_alzheimer$CholesterolTotal)
mediana_colt <- median(dados_alzheimer$CholesterolTotal)
moda_colt <- as.numeric(names(sort(table(dados_alzheimer$CholesterolTotal), decreasing = TRUE)[1]))
cat(moda_colt)
#Medidas de Dispersão
dp_colt <- sd(dados_alzheimer$CholesterolTotal)
cat(dp_colt)
var_colt <- var(dados_alzheimer$CholesterolTotal)
cat(var_colt)
cv_colt <- (dp_colt/media_colt)*100
cat("CV =", round(cv_colt, 2),"%")
#Visualizando
# Proporção dos valores
#teste de normalidade
shapiro.test(dados_alzheimer$CholesterolTotal)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de IDADE
x_values <- seq(min(dados_alzheimer$CholesterolTotal), max(dados_alzheimer$CholesterolTotal), length = 200)
normal_curve <- dnorm(x_values, mean = media_id, sd = dp_id)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$CholesterolTotal,
     main = "Histograma de Colesterol Total",
     xlab = "Valor",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_colt, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_colt, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_colt, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")


#LDL
summary(dados_alzheimer$CholesterolLDL)
media_ldl <- mean(dados_alzheimer$CholesterolLDL)
mediana_ldl <- median(dados_alzheimer$CholesterolLDL)
moda_ldl <- as.numeric(names(sort(table(dados_alzheimer$CholesterolLDL), decreasing = TRUE)[1]))
cat(moda_ldl)
#Medidas de Dispersão
dp_ldl <- sd(dados_alzheimer$CholesterolLDL)
cat(dp_ldl)
var_ldl <- var(dados_alzheimer$CholesterolLDL)
cat(var_ldl)
cv_ldl <- (dp_ldl/media_ldl)*100
cat("CV =", round(cv_ldl, 2),"%")
#Visualizando
# Proporção dos valores
#teste de normalidade
shapiro.test(dados_alzheimer$CholesterolLDL)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de IDADE
x_values <- seq(min(dados_alzheimer$CholesterolLDL), max(dados_alzheimer$CholesterolLDL), length = 200)
normal_curve <- dnorm(x_values, mean = media_id, sd = dp_id)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$CholesterolLDL,
     main = "Histograma de Colesterol - LDL",
     xlab = "Valor",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_ldl, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_ldl, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_ldl, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#HDL

summary(dados_alzheimer$CholesterolHDL)
media_hdl <- mean(dados_alzheimer$CholesterolHDL)
mediana_hdl <- median(dados_alzheimer$CholesterolHDL)
moda_hdl <- as.numeric(names(sort(table(dados_alzheimer$CholesterolHDL), decreasing = TRUE)[1]))
cat(moda_hdl)
#Medidas de Dispersão
dp_hdl <- sd(dados_alzheimer$CholesterolHDL)
cat(dp_hdl)
var_hdl <- var(dados_alzheimer$CholesterolHDL)
cat(var_hdl)
cv_hdl <- (dp_hdl/media_hdl)*100
cat("CV =", round(cv_hdl, 2),"%")
#Visualizando
# Proporção dos valores
#teste de normalidade
shapiro.test(dados_alzheimer$CholesterolHDL)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de IDADE
x_values <- seq(min(dados_alzheimer$CholesterolHDL), max(dados_alzheimer$CholesterolHDL), length = 200)
normal_curve <- dnorm(x_values, mean = media_id, sd = dp_id)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$CholesterolHDL,
     main = "Histograma de Colesterol - HDL",
     xlab = "Valor",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_hdl, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_hdl, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_hdl, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#Triglicerideos
summary(dados_alzheimer$CholesterolTriglycerides)
media_trigl <- mean(dados_alzheimer$CholesterolTriglycerides)
mediana_trigl <- median(dados_alzheimer$CholesterolTriglycerides)
moda_trigl <- as.numeric(names(sort(table(dados_alzheimer$CholesterolTriglycerides), decreasing = TRUE)[1]))
cat(moda_trigl)
#Medidas de Dispersão
dp_trigl <- sd(dados_alzheimer$CholesterolTriglycerides)
cat(dp_trigl)
var_trigl <- var(dados_alzheimer$CholesterolTriglycerides)
cat(var_trigl)
cv_trigl <- (dp_trigl/media_trigl)*100
cat("CV =", round(cv_trigl, 2),"%")
#Visualizando
# Proporção dos valores
#teste de normalidade
shapiro.test(dados_alzheimer$CholesterolTriglycerides)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de IDADE
x_values <- seq(min(dados_alzheimer$CholesterolTriglycerides), max(dados_alzheimer$CholesterolTriglycerides), length = 200)
normal_curve <- dnorm(x_values, mean = media_id, sd = dp_id)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$CholesterolTriglycerides,
     main = "Histograma de Tricligerides",
     xlab = "Valor",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_trigl, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_trigl, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_trigl, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#Correlatos para esses novos dados
variaveis <- c("CardiovascularDisease", "Diabetes", "Depression", "Hypertension",
               "CholesterolTotal", "CholesterolLDL", "CholesterolHDL", "CholesterolTriglycerides")

# Converter Diagnosis para numérico
dados_alzheimer$Diagnosis <- as.numeric(as.character(dados_alzheimer$Diagnosis))

correlacoes <- sapply(variaveis, function(var) {
  dados_alzheimer[[var]] <- as.numeric(as.character(dados_alzheimer[[var]]))  # garante que está numérico
  cor(dados_alzheimer[[var]], dados_alzheimer$Diagnosis, method = "pearson")
})

# Montar a tabela com os resultados
tabela_correlacoes <- data.frame(
  Variavel = variaveis,
  Correlacao_com_Diagnostico = round(correlacoes, 4)
)

# Ordenar pela correlação
tabela_correlacoes <- tabela_correlacoes[order(-abs(tabela_correlacoes$Correlacao_com_Diagnostico)), ]
print(tabela_correlacoes, row.names = FALSE)