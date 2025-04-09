# análises das frequências dos sintomas e medidas das pontuações de MMSE e ADL. obs: ainda falta analiser os resultados das medidas
# abrindo o banco de dados
dados <- read.csv('alzheimers_disease_data.csv')
# calculando a frequência de pacientes com e sem alzheimer que relataram ou não o sintoma de esquecimento
ctable(x = dados$Diagnosis, y = dados$Forgetfulness, prop = "r", headings = FALSE)
#----------- --------------- -------------- ------------- ---------------
#          Forgetfulness      0             1           Total
#Diagnosis                                                             
#0                       970 (69.8%)   419 (30.2%)   1389 (100.0%)
#1                       531 (69.9%)   229 (30.1%)    760 (100.0%)
#Total                  1501 (69.8%)   648 (30.2%)   2149 (100.0%)
#----------- --------------- -------------- ------------- ---------------

# dos pacientes que foram diagnosticados com alzheimer (1), 30.1% (229) relataram esquecimento e 69.9% (531) não
# já os pacientes sem a doença, 30.2% (419) relataram esquecimento e 69.8% (970) não

# freuência dos pacientes com e sem alzheimer que apresentaram ou não reclamações de memória
ctable(x = dados$Diagnosis, y = dados$MemoryComplaints, prop = "r", headings = FALSE)

#----------- ------------------ -------------- ------------- ---------------
#          MemoryComplaints         0             1           Total
#Diagnosis                                                                
#0                           1228 (88.4%)   161 (11.6%)   1389 (100.0%)
#1                            474 (62.4%)   286 (37.6%)    760 (100.0%)
#Total                       1702 (79.2%)   447 (20.8%)   2149 (100.0%)
#----------- ------------------ -------------- ------------- ---------------
# dos pacientes que foram diagnosticados com alzheimer (1), 37.6% (286) apresentaram reclamações de memória e 62.4% (474) não
# já os pacientes sem a doença, 11.6% (161) tiveram reclamações de memória e 88.4% (1228) não

# criando tabela com todos os sintomas e verificando a presença de alzheimer
tabela_sintomas <- table(esquecimento = dados$Forgetfulness, reclamações_memória = dados$MemoryComplaints, confusão = dados$Confusion, desorientação = dados$Disorientation, mudança_personalidade = dados$PersonalityChanges, dificuldade_tarefas = dados$DifficultyCompletingTasks, alzheimer = dados$Diagnosis)
ftable(tabela_sintomas)
# a partir desta tabela, e analisando somente os diagnosticados com alzheimer, podemos ver que:
# 162 pacientes não apresentaram nenhum dos sintomas 
# 31 apresentaram somente dificuldade em concluir tarefas
# 24 tiveram mudanças de personalidade
# 10 tiveram mudanças de personalidade e dificuldade em concluir tarefas
# 29 apresentaram desorientação
# 5 apresentaram desorientação e dificuldade em concluir tarefas
# 4 apresentaram desorientação e mudanças de personalidade
# 1 teve desorientação, mudança de personalidade e dificuldade em concluir tarefas
# 44 apresentaram confusão
# 4 apresentaram confusão e dificuldade em concluir tarefas
# 9 tiveram confusão e mudanças de personalidade
# 11 tiveram confusão e desorientação
# 1 teve confusão, desorientação e mudanças na personalidade
# 86 tiveram reclamações de memória
# 23 tiveram reclamações de memória e dificuldade em concluir tarefas
# 16 tiveram reclamações de memória e mudanças na personalidade
# 6 tiveram reclamações de memória, mudanças na personalidade e dificuldade em concluir tarefas
# 22 tiveram reclamações de memória e desorientação
# 3 tiveram reclamações de memória, desorientação e dificuldade em finalizar tarefas
# 4 tiveram reclamações de memória, desorientação e mudanças na personalidade
# 21 tiveram reclamações de memória e confusão
# 6 tiveram reclamações de memória, confusão e dificuldade em concluir tarefas
# 5 tiveram reclamações de memória, confusão e mudanças de personalidade
# 4 tiveram reclamações de memória, confusão e desorientação
# 74 apresentaram esquecimento
# 12 apresentaram esquecimento e dificuldade em concluir tarefas
# 8 apresentaram esquecimento e mudanças de personalidade
# 4 apresentaram esquecimento, mudanças de personalidade e dificuldade em concluir tarefas
# 10 apresentaram esquecimento e desorientação
# 1 apresentou esquecimento, desorientação e dificuldade em concluir tarefas
# 2 apresentaram esquecimento, desorientação e mudanças na personalidade
# 18 apresentaram esquecimento e confusão
# 4 apresentaram esquecimento, confusão e dificuldade em concluir tarefas
# 3 apresentaram esquecimento, confusão e mudanças na personalidade
# 1 apresentou esquecimento, confusão e desorientação
# 2 apresentaram esquecimento, confusão, desorientação e mudanças na personalidade
# 51 apresentaram esquecimento e reclamações de memória
# 12 apresentaram esquecimento, reclamações de memória e dificuldade em concluir tarefas
# 3 apresentaram esquecimento, reclamações de memória e mudanças de personalidade
# 1 apresentou esquecimento, reclamações de memória, mudanças de personalidade e dificuldade em concluir tarefas
# 5 apresentaram esquecimento, reclamações de memória e desorientação
# 1 apresentou esquecimento, reclamações de memória, desorientação e dificuldade em concluir tarefas
# 2 apresentaram esquecimento, reclamações de memória, desorientação e mudanças de personalidade
# 10 apresentaram esquecimento, reclamações de memória e confusão
# 2 apresentaram esquecimento, reclamações de memória, confusão e mudanças de personalidade
# 3 apresentaram esquecimento, reclamações de memória, confusão e desorientação

# a partir desses dados, podemos ver que dos 760 pacientes que foram diagnosticados com alzheimer, 162 (21.31%) não apresentaram nenhum dos sintomas e 598 apresentaram 1 ou mais sintomas
# os sintomas mais frequentes foram esquecimento e reclamações de memória (425 ou 55.92% pacientes apresentaram pelo menos um desses sintomas), enquanto confusão, desorientação, mudanças de personalidade e dificuldade em concluir tarefas foram sintomas que aparecem bastante em conjunto com os dois sintomas principais
# 173 (22.76%) pacientes apresentaram os sintomas confusão, desorientação, mudanças de personalidade e dificuldade em concluir tarefas (apresentando um ou mais desses sintomas mas sem esquecimento e reclamações de memória)

# análise da pontuação do MMSE (mini exame de estado mental pontuações variam de 0 a 30), onde pontuações mais baixas indicam comprometimento cognitivo (diminuição das funçõescognitivas como memória, atenção, raciocínio etc)
# cálculo média, moda, mediana

# arredondando os valores e criando uma nova variavel
dados$mmse_arred <- round(dados$MMSE)

media_mmse_arred <- mean(dados$mmse_arred)
cat(media_mmse_arred)
# 14.76

mediana_mmse_arred <- median(dados$mmse_arred)
cat(mediana_mmse_arred)
# 14
moda_mmse_arred <- as.numeric(names(sort(table(dados$mmse_arred), decreasing = TRUE)[1]))
cat(moda_mmse_arred)
# 6
# média e mediana são próximas e moda indica um valor mais baixo
boxplot(dados$mmse_arred,
                 main = "Boxplot do MMSE",
                 ylab = "Pontuação",
                 col = "lightgreen")
# o boxplot mostra uma grande variabilidade nas pontuações
# boxplot comparando pontuações de pacientes com e sem alzheimer
boxplot(mmse_arred ~ Diagnosis, data = dados,
                 main = "Pontuação no MMSE",
                 xlab = "Alzheimer",
                 ylab = "Pontuação",
                 col = c("lightcoral", "lightgreen"),
                 names = c("Não", "Sim"))
# Podemos ver que pacientes com alzheimer possuem pontuações mais baixas  
# Pacientes com alzheimer possuem dados assimétricos positivos, mediana está levemente deslocada para baixo
# a caixa com alzheimer está deslocada para baixo e a caixa sem alzheimer está levemente deslocada para cima

# cálculo medidas de dispersão
amplitude_mmse_arred <- max(dados$mmse_arred)- min(dados$mmse_arred)
cat(amplitude_mmse_arred)
# 30

desvio_padrao_mmse_arred <- sd(dados$mmse_arred)
variancia_mmse_arred <- var(dados$mmse_arred)
cat(variancia_mmse_arred) # 74.22
cat(desvio_padrao_mmse_arred) # 8.61 pontuações do teste variam 8.61 pontos para mais ou para menos da média

coef_var_mmse_arred <- (desvio_padrao_mmse_arred / media_mmse_arred)*100
cat("CV =", round(coef_var_mmse_arred, 2),"%") # 58.37%, alta variabilidade 


#Criando um vetor de valores para o eixox
 x_values<-seq(min(dados$mmse_arred),max(dados$mmse_arred),length=2149)
 #Calculando a densidade da distribuição normal com média e desvio padrão dos dados
   normal_curve <-dnorm(x_values,mean= media_mmse_arred,sd= desvio_padrao_mmse_arred)
 #Ajustando o limite superior do eixo y com base no pico da curva normal
   max_density <-max(normal_curve) * 1.2 #Adicionando uma margem de20%
 #Criando um histograma das pontuações
   hist(dados$mmse_arred,
               main= "Histograma da pontuação no MMSE",
               xlab= "Pontuação",
               ylab= "Frequência",
               col= "lightblue",
               border= "black",
               probability= TRUE,
               ylim= c(0,max_density))
 #Adicionando linhas de referência para média,mediana e moda
 abline(v= media_mmse_arred,col= "red",lwd= 2, lty= 2) #Média
 abline(v= mediana_mmse_arred, col= "green", lwd= 2,lty= 2) #Mediana
 abline(v= moda_mmse_arred, col="blue",lwd= 2, lty= 2) #Moda
 #Adicionando a curva normal
   lines(x_values,normal_curve, col= "darkred", lwd=2, lty= 1)
 #Adicionando legenda
   legend("topright",
                   legend= c("CurvaNormal", "Média", "Mediana", "Moda"),
                   col= c("darkred", "red", "green", "blue"),
                   lty= c(1, 2, 2, 2),
                   lwd= 2,
                   bty= "n")
# pelo histograma podemos ver que as pontuações estão bem distribuídas, já que temos dados de pacientes com e sem alzheimer misturados

# removendo pacientes sem diagnóstico para alzheimer
df_filtrado <- subset(dados, Diagnosis != 0)
# cálculo média
media_mmse <- mean(df_filtrado$mmse_arred)
cat("a média do teste para pacientes com alzheimer é", media_mmse)
# 12

mediana_mmse <- median(df_filtrado$mmse_arred)
cat(mediana_mmse)
# 11.5
# média e mediana são menores para pacientes com alzheimer
moda_mmse <- as.numeric(names(sort(table(df_filtrado$mmse_arred), decreasing = TRUE)[1]))
cat(moda_mmse)
# 8

# cálculo medidas de dispersão
amplitude_mmse <- max(df_filtrado$mmse_arred)- min(df_filtrado$mmse_arred)
cat(amplitude_mmse) # 30

desvio_padrao_mmse <- sd(df_filtrado$mmse_arred)
variancia_mmse <- var(df_filtrado$mmse_arred)
cat(variancia_mmse) # 52.23
cat(desvio_padrao_mmse) # 7.23


coef_var_mmse <- (desvio_padrao_mmse / media_mmse)*100
cat("CV =", round(coef_var_mmse, 2),"%") #60.21%


#criando uma amostra estratificada para realizar o teste t para amostras independentes e verificar se a presença de alzheimer influencia nas pontuações do teste MMSE
library(sampling)
amostra_estratificada <- strata(dados, stratanames = "Diagnosis", size = c("0" = 20, "1" = 13), method = "srswor")
amostra_final <- getdata(dados, amostra_estratificada)
library(RVAideMemoire)
byf.shapiro(MMSE ~ Diagnosis, amostra_final)
#Shapiro-Wilk normality tests (p valor é maior do que 0.05, então os dados podem ser considerados como uma distribuição normal)

#data:  MMSE by Diagnosis 
#       W p-value  
#0 0.9105 0.06503 .
#1 0.9010 0.13805  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

amostra_final$alzheimer <- ifelse(amostra_final$Diagnosis == 0, "não", "sim")
library(car)
leveneTest(MMSE ~ alzheimer, amostra_final, center=mean)
#      Df F value  Pr(>F)  
#group  1  3.3875 0.07528 .
#     31                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

t.test(MMSE ~ Diagnosis, amostra_final, var.equal=FALSE)
#Welch Two Sample t-test (p valor é maior do que 0.05, logo hipótese nula não é rejeitada, não há diferenças significativas entre as médias das pontuações

#data:  MMSE by Diagnosis
#t = 0.78549, df = 30.858, p-value = 0.4382
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -3.854578  8.681993
#sample estimates:
#  mean in group 0 mean in group 1 
#15.43867        13.02497

# não podemos afirmar que pacientes com alzheimer aprentam menores pontuações de MMSE (comprometimento cognitivo), pois não há diferenças significativas nas médias das pontuações
# existem pacientes que tiveram pontuações mais baixas e não possuem alzheimer, pois o resultado do teste pode ter tido influência de outros fatores
boxplot(MMSE ~ Diagnosis, data = amostra_final, ylab="Pontuação MMSE",
         xlab="Alzheimer")
# gráfico
boxplot(MMSE ~ Diagnosis, data = amostra_final, ylab="Pontuação MMSE",
        xlab="Alzheimer")
resultado_t <- t.test(MMSE ~ Diagnosis, amostra_final, var.equal=FALSE)
t_obs <- resultado_t$statistic
gl <- resultado_t$parameter
alpha <- 0.05
t_crit <- qt(1- alpha/2, df = gl)
x <- seq(-4, 4, length = 200)
y <- dt(x, df = gl)
plot(x, y, type = "l", lwd = 2, col = "black",
     main = "Distribuição t de Student — Pontuação MMSE",
     ylab = "Densidade", xlab = "Valor t")
polygon(c(x[x <=-t_crit],-t_crit), c(y[x <=-t_crit], 0), col = rgb(1, 0, 0, 0.3), border = NA)
polygon(c(x[x >= t_crit], t_crit), c(y[x >= t_crit], 0), col = rgb(1, 0, 0, 0.3), border = NA)
abline(v = t_obs, col = "blue", lwd = 2, lty = 2)
text(t_obs, dt(t_obs, df = gl) + 0.01,
     paste0("t calculado = ", round(t_obs, 2)), col = "blue", pos = 4, cex = 0.9)
abline(v = c(-t_crit, t_crit), col = "red", lty = 3, lwd = 2)
text(-t_crit, 0.03, paste0("-t crítico = ", round(-t_crit, 2)), col = "red", pos = 2, cex = 0.9)
text(t_crit, 0.03, paste0("t crítico = ", round(t_crit, 2)), col = "red", pos = 4, cex = 0.9)

# calculo medidas para ADL (Escore de Atividades de Vida Diária, variando de 0 a 10. Pontuações mais baixas indicam maior comprometimento)
# arredondando os valores e criando uma nova variavel
dados$adl_arred <- round(dados$ADL)

media_adl_arred <- mean(dados$adl_arred)
cat(media_adl_arred)

mediana_adl_arred <- median(dados$adl_arred)
cat(mediana_adl_arred)

moda_adl_arred <- as.numeric(names(sort(table(dados$adl_arred), decreasing = TRUE)[1]))
cat(moda_adl_arred)

boxplot(dados$adl_arred,
                 main = "Boxplot do ADL",
                 ylab = "Pontuação",
                 col = "lightgreen")

amplitude_adl_arred <- max(dados$adl_arred)- min(dados$adl_arred)
cat(amplitude_adl_arred)

desvio_padrao_adl_arred <- sd(dados$adl_arred)
variancia_adl_arred <- var(dados$adl_arred)
cat(variancia_adl_arred)
cat(desvio_padrao_adl_arred)
coef_var_adl_arred <- (desvio_padrao_adl_arred / media_adl_arred)*100
cat("CV =", round(coef_var_adl_arred, 2),"%")

# boxplot comparando pontuações de pacientes com e sem alzheimer
boxplot(adl_arred ~ Diagnosis, data = dados,
        main = "Pontuação no ADL",
        xlab = "Alzheimer",
        ylab = "Pontuação",
        col = c("lightcoral", "lightgreen"),
        names = c("Não", "Sim"))
# pacientes sem alzheimer apresentam pontuações maiores (menor comprometimento)

#Criando um vetor de valores para o eixox
x_values2<-seq(min(dados$adl_arred),max(dados$adl_arred),length=2149)
#Calculando a densidade da distribuição normal com média e desvio padrão dos dados
normal_curve2 <-dnorm(x_values2,mean= media_adl_arred,sd= desvio_padrao_adl_arred)
#Ajustando o limite superior do eixo y com base no pico da curva normal
max_density2 <-max(normal_curve2) * 1.2 #Adicionando uma margem de20%
#Criando um histograma das pontuações
hist(dados$adl_arred,
     main= "Histograma da pontuação ADL",
     xlab= "Pontuação",
     ylab= "Frequência",
     col= "lightblue",
     border= "black",
     probability= TRUE,
     ylim= c(0,max_density2))
#Adicionando linhas de referência para média,mediana e moda
abline(v= media_adl_arred,col= "red",lwd= 2, lty= 2) #Média
abline(v= mediana_adl_arred, col= "green", lwd= 2,lty= 2) #Mediana
abline(v= moda_adl_arred, col="blue",lwd= 2, lty= 2) #Moda
#Adicionando a curva normal
lines(x_values2,normal_curve2, col= "darkred", lwd=2, lty= 1)
#Adicionando legenda
legend("topright",
       legend= c("CurvaNormal", "Média", "Mediana", "Moda"),
       col= c("darkred", "red", "green", "blue"),
       lty= c(1, 2, 2, 2),
       lwd= 2,
       bty= "n")
# pontuações estão bem distribuídas porque temos pacientes com e sem alzheimer misturados

# criando uma amostra estratificada e realizando teste t para amostras independentes para verificar se a presença de alzheimer influencia na pontuação do ADL
amostra_estratificada <- strata(dados, stratanames = "Diagnosis", size = c("0" = 20, "1" = 13), method = "srswor")
amostra_final <- getdata(dados, amostra_estratificada)
byf.shapiro(ADL ~ Diagnosis, amostra_final)
#	Shapiro-Wilk normality tests (se p valor for maior que 0.05, significa que os dados podem ser considerados com distribuição normal)

#data:  ADL by Diagnosis 
#W p-value
#0 0.9323  0.1708
#1 0.9075  0.1697

amostra_final$alzheimer <- ifelse(amostra_final$Diagnosis == 0, "não", "sim")
leveneTest(ADL ~ alzheimer, amostra_final, center=mean)
#Levene's Test for Homogeneity of Variance (center = mean)
#      Df F value Pr(>F)
#group  1   6e-04 0.9807
#      26 

t.test(ADL ~ Diagnosis, amostra_final, var.equal=FALSE)
#Welch Two Sample t-test (p valor é menor do que 0.05, então hipótese alternativa é aceita, existem diferenças significativas entre as médias)

#data:  ADL by Diagnosis
#data:  ADL by Diagnosis
#t = 2.196, df = 22.601, p-value = 0.03863
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  0.1220627 4.1566942
#sample estimates:
#  mean in group 0 mean in group 1 
#6.045988        3.906610

# A presença de Alzheimer influencia na pontuação do ADL, pacientes sem alzheimer tiverem pontuações mais altas do que pacientes com alzheimer
# pacientes com alzheimer apresentam maior comprometimento/dificuldade ao realizar atividades diárias
boxplot(ADL ~ Diagnosis, data = amostra_final, ylab="Pontuação ADL",
                 xlab="Alzheimer")
# distribuição t de student
boxplot(ADL ~ Diagnosis, data = amostra_final, ylab="Pontuação ADL",
                          xlab="Alzheimer")
resultado_t <- t.test(ADL ~ Diagnosis, amostra_final, var.equal=FALSE)
t_obs <- resultado_t$statistic
gl <- resultado_t$parameter
alpha <- 0.05
t_crit <- qt(1- alpha/2, df = gl)
x <- seq(-4, 4, length = 200)
y <- dt(x, df = gl)
plot(x, y, type = "l", lwd = 2, col = "black",
                     main = "Distribuição t de Student — Pontuação ADL",
                     ylab = "Densidade", xlab = "Valor t")
polygon(c(x[x <=-t_crit],-t_crit), c(y[x <=-t_crit], 0), col = rgb(1, 0, 0, 0.3), border = NA)
polygon(c(x[x >= t_crit], t_crit), c(y[x >= t_crit], 0), col = rgb(1, 0, 0, 0.3), border = NA)
abline(v = t_obs, col = "blue", lwd = 2, lty = 2)
text(t_obs, dt(t_obs, df = gl) + 0.01,
                     paste0("t calculado = ", round(t_obs, 2)), col = "blue", pos = 4, cex = 0.9)
abline(v = c(-t_crit, t_crit), col = "red", lty = 3, lwd = 2)
text(-t_crit, 0.03, paste0("-t crítico = ", round(-t_crit, 2)), col = "red", pos = 2, cex = 0.9)
text(t_crit, 0.03, paste0("t crítico = ", round(t_crit, 2)), col = "red", pos = 4, cex = 0.9)

#Calculando medidas descritivas e dispersão para nível de educação

media_educ <- mean(dados$EducationLevel)
mediana_educ <- median(dados$EducationLevel)
moda_educ <- as.numeric(names(sort(table(dados$EducationLevel), decreasing = TRUE)[1]))
boxplot(dados$EducationLevel,
                   main = "Boxplot do nível de educação",
                   ylab = "Pontuação",
                   col = "lightgreen")
amplitude_educ <- max(dados$EducationLevel)- min(dados$EducationLevel)
desvio_padrao_educ <- sd(dados$EducationLevel)
variancia_mmse_educ <- var(dados$EducationLevel)
coef_var_educ <- (desvio_padrao_educ / media_educ)*100
hist(dados$EducationLevel)
educ <- table(nivel=dados$EducationLevel, alzheimer=dados$Diagnosis)
educ
#      alzheimer
#nivel   0   1
#0      272 174
#1      552 302
#2      419 217
#3      146  67
