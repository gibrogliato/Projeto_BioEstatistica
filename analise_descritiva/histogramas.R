
#Agora com os dados de 'analise_descritiva.R' em maõs, vamos plotar os histogramas
#e suas curvas para os valores de 1)idade, 2) BMI, 3) consumo de álcool,
#4) atividade física e 5) qualidade do sono.

#IDADE

#teste de normalidade
shapiro.test(dados_alzheimer$Age)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de IDADE
x_values <- seq(min(dados_alzheimer$Age), max(dados_alzheimer$Age), length = 200)
normal_curve <- dnorm(x_values, mean = media_id, sd = dp_id)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$Age,
     main = "Histograma da Idade",
     xlab = "Anos",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_id, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_id, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_id, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#BMI

#teste de normalidade
shapiro.test(dados_alzheimer$BMI)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de BMI
x_values <- seq(min(dados_alzheimer$BMI), max(dados_alzheimer$BMI), length = 200)
normal_curve <- dnorm(x_values, mean = media_bmi, sd = dp_bmi)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$BMI,
     main = "Histograma do IMC",
     xlab = "kg/m^2",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_bmi, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_bmi, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_bmi, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#CONSUMO DE ÁLCOOL

#teste de normalidade
shapiro.test(dados_alzheimer$AlcoholConsumption)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de BMI
x_values <- seq(min(dados_alzheimer$AlcoholConsumption), max(dados_alzheimer$AlcoholConsumption), length = 200)
normal_curve <- dnorm(x_values, mean = media_alcool, sd = dp_alcool)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$AlcoholConsumption,
     main = "Histograma do Consumo de Álcool",
     xlab = "Unidades",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_alcool, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_alcool, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_alcool, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#ATIVIDADE FÍSICA

#teste de normalidade
shapiro.test(dados_alzheimer$PhysicalActivity)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de BMI
x_values <- seq(min(dados_alzheimer$PhysicalActivity), max(dados_alzheimer$PhysicalActivity), length = 200)
normal_curve <- dnorm(x_values, mean = media_af, sd = dp_af)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$PhysicalActivity,
     main = "Histograma de Atividade Física",
     xlab = "Horas",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_af, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_af, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_af, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")

#QUALIDADE DO SONO

#teste de normalidade
shapiro.test(dados_alzheimer$SleepQuality)  # Teste de Shapiro-Wilk

#Histograma de Distribuição de BMI
x_values <- seq(min(dados_alzheimer$SleepQuality), max(dados_alzheimer$SleepQuality), length = 200)
normal_curve <- dnorm(x_values, mean = media_sono, sd = dp_sono)
# Ajustando o limite superior do eixo y com base no pico da curva normal
max_density <- max(normal_curve) * 1.2 # Adicionando uma margem de 20%
# Criando um histograma do peso ao nascer
hist(dados_alzheimer$SleepQuality,
     main = "Histograma de Qualidade do Sono",
     xlab = "Score",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     probability = TRUE,
     ylim = c(0, max_density))
# Adicionando linhas de referência para média, mediana e moda
abline(v = media_sono, col = "red", lwd = 2, lty = 2) # Média
abline(v = mediana_sono, col = "green", lwd = 2, lty = 2) # Mediana
abline(v = moda_sono, col = "blue", lwd = 2, lty = 2) # Moda
# Adicionando a curva normal ao histograma - falaremos sobre isso na próxima aula
lines(x_values, normal_curve, col = "darkred", lwd = 2, lty = 1)
# Adicionando legenda
legend("topright",
       legend = c("Curva Normal", "Média", "Mediana", "Moda"),
       col = c("darkred", "red", "green", "blue"),
       lty = c(1, 2, 2, 2),
       lwd = 2,
       bty = "n")
