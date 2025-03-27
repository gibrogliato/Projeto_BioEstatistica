
#Fazendo a Análise Desciritiva dos dados

dados_alzheimer <- read.csv('alzheimers_disease_data.csv')
str(dados_alzheimer)  # Estrutura dos dados
head(dados_alzheimer)  # Primeiras linhas
summary(dados_alzheimer)  # Estatísticas resumidas


#Vamos trabalhar com a análise de 1) idade, 2) gênero, 
# 3) BMI, 4) consumo de álcool, 5) atividade física e 6) qualidade do sono.

#Média, Mediana e Moda
#Idade
media_id <- mean(dados_alzheimer$Age)
cat(media_id)
mediana_id <- median(dados_alzheimer$Age)
cat(mediana_id)
moda_id <- as.numeric(names(sort(table(dados_alzheimer$Age), decreasing = TRUE)[1]))
cat(moda_id)
#Medidas de Dispersão
dp_id <- sd(dados_alzheimer$Age)
cat(dp_id)
var_id <- var(dados_alzheimer$Age)
cat(var_id)
cv_id <- (dp_id/media_id)*100
cat("CV =", round(cv_id, 2),"%")
#Boxplot
boxplot(dados_alzheimer$Age,
        main = "Boxplot das Idades dos Pacientes",
        ylab = "Anos",
        col = "pink")

#BMI
media_bmi <- mean(dados_alzheimer$BMI)
cat(media_bmi)
mediana_bmi <- median(dados_alzheimer$BMI)
cat(mediana_bmi)
moda_bmi <- as.numeric(names(sort(table(dados_alzheimer$BMI), decreasing = TRUE)[1]))
cat(moda_bmi)
#Medidas de Dispersão
dp_bmi <- sd(dados_alzheimer$BMI)
cat(dp_bmi)
var_bmi <- var(dados_alzheimer$BMI)
cat(var_bmi)
cv_bmi <- (dp_bmi/media_bmi)*100
cat("CV =", round(cv_bmi, 2),"%")
#Boxplot
boxplot(dados_alzheimer$BMI,
        main = "Boxplot IMC dos Pacientes",
        ylab = "kg/m^2",
        col = "purple")

#Consumo de álcool
media_alcool <- mean(dados_alzheimer$AlcoholConsumption)
cat(media_alcool)
mediana_alcool <- median(dados_alzheimer$AlcoholConsumption)
cat(mediana_alcool)
moda_alcool <- as.numeric(names(sort(table(dados_alzheimer$AlcoholConsumption), decreasing = TRUE)[1]))
cat(moda_alcool)
#Medidas de Dispersão
dp_alcool <- sd(dados_alzheimer$AlcoholConsumption)
cat(dp_alcool)
var_alcool <- var(dados_alzheimer$AlcoholConsumption)
cat(var_alcool)
cv_alcool <- (dp_alcool/media_alcool)*100
cat("CV =", round(cv_alcool, 2),"%")
#Boxplot
boxplot(dados_alzheimer$AlcoholConsumption,
        main = "Boxplot do Consumo de Alcool dos Pacientes",
        ylab = "Unidades",
        col = "maroon")

#Atividade física
media_af <- mean(dados_alzheimer$PhysicalActivity)
cat(media_af)
mediana_af <- median(dados_alzheimer$PhysicalActivity)
cat(mediana_af)
moda_af <- as.numeric(names(sort(table(dados_alzheimer$PhysicalActivity), decreasing = TRUE)[1]))
cat(moda_af)
#Medidas de Dispersão
dp_af <- sd(dados_alzheimer$PhysicalActivity)
cat(dp_af)
var_af <- var(dados_alzheimer$PhysicalActivity)
cat(var_af)
cv_af <- (dp_af/media_af)*100
cat("CV =", round(cv_alcool, 2),"%")
#Boxplot
boxplot(dados_alzheimer$PhysicalActivity,
        main = "Boxplot da Quantidade de Atividade Física dos Pacientes",
        ylab = "Horas",
        col = "brown")

#Qualidade de Sono
media_sono <- mean(dados_alzheimer$SleepQuality)
cat(media_sono)
mediana_sono <- median(dados_alzheimer$SleepQuality)
cat(mediana_sono)
moda_sono <- as.numeric(names(sort(table(dados_alzheimer$SleepQuality), decreasing = TRUE)[1]))
cat(moda_sono)
#Medidas de Dispersão
dp_sono <- sd(dados_alzheimer$SleepQuality)
cat(dp_sono)
var_sono <- var(dados_alzheimer$SleepQuality)
cat(var_sono)
cv_sono <- (dp_sono/media_sono)*100
cat("CV =", round(cv_sono, 2),"%")
#Boxplot
boxplot(dados_alzheimer$SleepQuality,
        main = "Boxplot do Score de Sono dos Pacientes",
        ylab = "Pontos",
        col = "darkorange")

#Gênero
#Nesse caso temos uma variável que segue a distribuição de Bernoulli por só ter duas possíveis respostas.
# Suponha que temos uma variável binária
dados_genero <- dados_alzheimer$Gender  
# Verificar se é uma variável binária (só contém 0 e 1)
unique(dados_genero)
# Cálculo das estatísticas
p <- mean(dados_genero, na.rm = TRUE)  # Proporção de sucessos (média)
variancia <- p * (1 - p)  # Variância da Bernoulli
desvio_padrao <- sqrt(variancia)  # Desvio padrão
moda <- as.numeric(names(which.max(table(dados_genero))))  # Moda (0 ou 1)
freq_tabela <- table(dados_genero)  # Tabela de frequências
print(freq_tabela)

