#Análise Descritiva 1
# DENGUE - Notificações registradas no Sistema de Informação de Agravos de Notificação - São Paulo 2024

colunas <- read.csv("dengue_etaria.csv", sep = ";", fileEncoding = "latin1", skip = 3, nrows = 1, header = FALSE)
dados <- read.csv("dengue_etaria.csv",
                  sep = ";",
                  fileEncoding = "latin1",
                  skip = 4,
                  header = FALSE,
                  col.names = unlist(colunas),
                  check.names = FALSE)
View(dados)
#Conseguimos ver os dados do Data Sus agora
#Porém, esses dados contam com informações faltantes, vamos tratar essa tabela para análise.
faixa <- dados[[1]]
#Limpando as colunas agora
numeros <- as.data.frame(lapply(dados[ , -1], function(x) {
  x <- as.character(x)
  x <- gsub("\\.", "", x)         # remove pontos (separador de milhar)
  x <- gsub(",", "", x)           # remove vírgulas
  x <- gsub("\"", "", x)          # remove aspas
  x <- trimws(x)                  # remove espaços antes/depois
  x[x == ""] <- NA                # substitui string vazia por NA
  as.numeric(x)
}))

# Junta tudo de novo com a coluna de faixa etária
dados_limpos <- cbind(Faixa_Etaria = faixa, numeros)
names(dados_limpos) <- c("Faixa_Etaria", "Ign_Branco", "Laboratorial", "Clinico", "Investigacao", "Total")
#Agora temos dados limpos (sem NAs) para trabalhar. vamos começar a analisar.
#Temos as seguintes formas de confirmação de casos de dengue notificados: 1)Ign/Branco, 2)Laboratorial, 3)Clínico-Epidemiológico, 4) Em investigação e 5)Total
#Começaremos obtendo as medidas de Tendência Central (média, moda, mediana) para as notificações do caso 2. 

#Caso 1 - Ign/Branco
#Média, Moda, Mediana
media_ignb <- mean(dados_limpos$Ign_Branco, na.rm = TRUE)
moda_ignb <- as.numeric(names(sort(table(dados_limpos$Ign_Branco),decreasing = TRUE) [1]))
mediana_ignb <- median(dados_limpos$Ign_Branco, na.rm = TRUE)
cat(media_ignb, moda_ignb, mediana_ignb)

#Medidas de Dispersão
#Variância, Erro, Coeficiente de Variação
var_ignb <- var(dados_limpos$Ign_Branco, na.rm = TRUE)
dp_ignb <- sd(dados_limpos$Ign_Branco, na.rm = TRUE)
cv_ignb <- (dp_ignb/media_ignb)*100
cat(var_ignb, dp_ignb, cv_ignb)

#Tabela de Contigência para visualização
tab_ignb <-  cbind("Media", media_ignb, "Moda", moda_ignb, "Mediana", mediana_ignb, "Variancia", var_ignb, "Desvio Padrao", dp_ignb, "Coef. Variancia", cv_ignb)
tab_ignb

#Criando um boxplot para visualizar melhor
boxplot((dados_limpos$Ign_Branco),
        main = "Boxplot do casos Ignorados e em Branco de dengue",
        xlab = "Total de casos",
        col = c("lightblue", "lightgreen", "orange", "pink", "lavender"),
        border = "darkgray")

#Caso 2 - Laboratórial
#Média, Moda, Mediana
media_lab <- mean(dados_limpos$Laboratorial, na.rm = TRUE)
moda_lab <- as.numeric(names(sort(table(dados_limpos$Laboratorial),decreasing = TRUE)[1]))
mediana_lab <- median(dados_limpos$Laboratorial, na.rm = TRUE)
cat(media_lab, moda_lab, mediana_lab)

#Medidas de Dispersão
#Variância, Erro, Coeficiente de Variação
var_lab <- var(dados_limpos$Laboratorial, na.rm = TRUE)
dp_lab <- sd(dados_limpos$Laboratorial, na.rm = TRUE)
cv_lab <- (dp_lab/media_lab)*100
cat(var_lab, dp_lab, cv_lab)

#Tabela de Contigência para visualização
tab_lab <-  cbind("Media", media_lab, "Moda", moda_lab, "Mediana", mediana_lab, "Variancia", var_lab, "Desvio Padrao", dp_lab, "Coef. Variancia", cv_lab)
tab_lab

#Criando um boxplot para visualizar melhor
boxplot((dados_limpos$Laboratorial),
        main = "Boxplot de casos de dengue comprovados em Laboratório",
        xlab = "Total de casos",
        col = c("lightblue", "lightgreen", "orange", "pink", "lavender"),
        border = "darkgray")

#Caso 3 - Clínico-Epidemiológico
#Média, Moda, Mediana
media_clin <- mean(dados_limpos$Clinico, na.rm = TRUE)
moda_clin <- as.numeric(names(sort(table(dados_limpos$Clinico),decreasing = TRUE)[1]))
mediana_clin <- median(dados_limpos$Clinico, na.rm = TRUE)
cat(mediana_clin,moda_clin, mediana_clin)


#Medidas de Dispersão
#Variância, Erro, Coeficiente de Variação
var_clin <- var(dados_limpos$Clinico, na.rm = TRUE)
dp_clin <- sd(dados_limpos$Clinico, na.rm = TRUE)
cv_clin <- (dp_clin/media_clin)*100
cat(var_clin, dp_clin, cv_clin)

#Tabela de Contigência para visualização
tab_clin <-  cbind("Media", media_clin, "Moda", moda_clin, "Mediana", mediana_clin, "Variancia", var_clin, "Desvio Padrao", dp_clin, "Coef. Variancia", cv_clin)
tab_clin

#Criando um boxplot para visualizar melhor
boxplot((dados_limpos$Clinico),
        main = "Boxplot de casos de dengue comprovados em Consulta Clínica",
        xlab = "Total de casos",
        col = c("lightblue", "lightgreen", "orange", "pink", "lavender"),
        border = "darkgray")

#Caso 4 - Em Investigação
#Média, Moda, Mediana
media_in <- mean(dados_limpos$Investigacao, na.rm = TRUE)
moda_in <- as.numeric(names(sort(table(dados_limpos$Investigacao),decreasing = TRUE)[1]))
mediana_in <- median(dados_limpos$Investigacao, na.rm = TRUE)
cat(media_in, moda_in, mediana_in)


#Medidas de Dispersão
#Variância, Erro, Coeficiente de Variação
var_in <- var(dados_limpos$Investigacao, na.rm = TRUE)
dp_in <- sd(dados_limpos$Investigacao, na.rm = TRUE)
cv_in <- (dp_in/media_in)*100
cat(var_in, dp_in, cv_in)

#Tabela de Contigência para visualização
tab_in <-  cbind("Media", media_in, "Moda", moda_in, "Mediana", mediana_in, "Variancia", var_in, "Desvio Padrao", dp_in, "Coef. Variancia", cv_in)
tab_in

#Criando um boxplot para visualizar melhor
boxplot((dados_limpos$Investigacao),
        main = "Boxplot de casos de dengue em Investigação",
        xlab = "Total de casos",
        col = c("lightblue", "lightgreen", "orange", "pink", "lavender"),
        border = "darkgray")

#Caso 5 - Total
#Média, Moda, Mediana
media_total <- mean(dados_limpos$Total, na.rm = TRUE)
moda_total <- as.numeric(names(sort(table(dados_limpos$Total),decreasing = TRUE)[1]))
mediana_total <- median(dados_limpos$Total, na.rm = TRUE)
cat(media_total, moda_total, mediana_total)

#Medidas de Dispersão
#Variância, Erro, Coeficiente de Variação
var_tot <- var(dados_limpos$Total, na.rm = TRUE)
dp_tot <- sd(dados_limpos$Total, na.rm = TRUE)
cv_tot <- (dp_tot/media_total)*100
cat(var_tot, dp_tot, cv_tot)

#Tabela de Contigência para visualização
tab_tot <-  cbind("Media", media_total, "Moda", moda_total, "Mediana", mediana_total, "Variancia", var_tot, "Desvio Padrao", dp_tot, "Coef. Variancia", cv_tot)
tab_tot

#Criando um boxplot para visualizar melhor
boxplot((dados_limpos$Total),
        main = "Boxplot do Total de casos de dengue",
        xlab = "Total de casos",
        col = c("lightblue", "lightgreen", "orange", "pink", "lavender"),
        border = "darkgray")


