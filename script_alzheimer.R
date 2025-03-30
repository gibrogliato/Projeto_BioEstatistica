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

# análise da pontuação do MMSE (mini exame de estado mental), onde pontuações mais baixas indicam comprometimento cognitivo (diminuição das funçõescognitivas como memória, atenção, raciocínio etc)
# cálculo média, moda, mediana
media_mmse_geral <- mean(dados$MMSE)
cat(media_mmse_geral)

mediana_mmse_geral <- median(dados$MMSE)
cat(mediana_mmse_geral)

moda_mmse_geral <- as.numeric(names(sort(table(dados$MMSE), decreasing = TRUE)[1]))
cat(moda_mmse_geral)

boxplot(dados$MMSE,
                 main = "Boxplot do MMSE",
                 ylab = "Pontuação",
                 col = "lightgreen")
# o boxplot mostra uma grande variabilidade nas pontuações
# boxplot comparando pontuações de pacientes com e sem alzheimer
boxplot(MMSE ~ alzheimer, data = dados,
                 main = "Pontuação no MMSE",
                 xlab = "Alzheimer (0 = Nao, 1 = Sim)",
                 ylab = "Pontuação",
                 col = c("lightcoral", "lightgreen"),
                 names = c("Sem alzheimer", "Com alzheimer"))
# Podemos ver que pacientes com alzheimer possuem pontuações mais baixas e pacientes sem alzheimer apresentam maior variação 


# cálculo medidas de dispersão
amplitude_mmse_geral <- max(dados$MMSE)- min(dados$MMSE)
cat(amplitude_mmse_geral)
#29.98607

desvio_padrao_mmse_geral <- sd(dados$MMSE)
variancia_mmse_geral <- var(dados$MMSE)
cat(variancia_mmse_geral)
#74.18638
cat(desvio_padrao_mmse_geral)
#8.613151

coef_var_mmse_geral <- (desvio_padrao_mmse_geral / media_mmse_geral)*100
cat("CV =", round(coef_var_mmse_geral, 2),"%")
#CV = 58.37 %

#Criando um vetor de valores para o eixox
 x_values<-seq(min(dados$MMSE),max(dados$MMSE),length=2149)
 #Calculando a densidade da distribuição normal com média e desvio padrão dos dados
   normal_curve <-dnorm(x_values,mean= media_mmse_geral,sd= desvio_padrao_mmse_geral)
 #Ajustando o limite superior do eixo y com base no pico da curva normal
   max_density <-max(normal_curve) * 1.2 #Adicionando uma margem de20%
 #Criando um histograma das pontuações
   hist(dados$MMSE,
               main= "Histograma da pontuação no MMSE",
               xlab= "Pontuação",
               ylab= "Frequência",
               col= "lightblue",
               border= "black",
               probability= TRUE,
               ylim= c(0,max_density))
 #Adicionando linhas de referência para média,mediana e moda
 abline(v= media_mmse_geral,col= "red",lwd= 2, lty= 2) #Média
 abline(v= mediana_mmse_geral, col= "green", lwd= 2,lty= 2) #Mediana
 abline(v= moda_mmse_geral, col="blue",lwd= 2, lty= 2) #Moda
 #Adicionando a curva normal
   lines(x_values,normal_curve, col= "darkred", lwd=2, lty= 1)
 #Adicionando legenda
   legend("topright",
                   legend= c("CurvaNormal", "Média", "Mediana", "Moda"),
                   col= c("darkred", "red", "green", "blue"),
                   lty= c(1, 2, 2, 2),
                   lwd= 2,
                   bty= "n")

# removendo pacientes sem diagnóstico para alzheimer
df_filtrado <- subset(dados, Diagnosis != 0)
# cálculo média
media_mmse <- mean(df_filtrado$MMSE)
cat("a média do teste para pacientes com alzheimer é", media_mmse)

mediana_mmse <- median(df_filtrado$MMSE)
cat(mediana_mmse)

moda_mmse <- as.numeric(names(sort(table(df_filtrado$MMSE), decreasing = TRUE)[1]))
cat(moda_mmse)

# cálculo medidas de dispersão
amplitude_mmse <- max(df_filtrado$MMSE)- min(df_filtrado$MMSE)
cat(amplitude_mmse)

desvio_padrao_mmse <- sd(df_filtrado$MMSE)
variancia_mmse <- var(df_filtrado$MMSE)
cat(variancia_mmse)
cat(desvio_padrao_mmse)


coef_var_mmse <- (desvio_padrao_mmse / media_mmse)*100
cat("CV =", round(coef_var_mmse, 2),"%")


#Criando um vetor de valores para o eixox
x_values3<-seq(min(df_filtrado$MMSE),max(df_filtrado$MMSE),length=760)
#Calculando a densidade da distribuição normal com média e desvio padrão dos dados
normal_curve3 <-dnorm(x_values3,mean= media_mmse,sd= desvio_padrao_mmse)
#Ajustando o limite superior do eixo y com base no pico da curva normal
max_density3 <-max(normal_curve3) * 1.2 #Adicionando uma margem de20%
#Criando um histograma das pontuações
hist(df_filtrado$MMSE,
     main= "Histograma da pontuação no MMSE para pacientes com alzheimer",
     xlab= "Pontuação",
     ylab= "Frequência",
     col= "lightblue",
     border= "black",
     probability= TRUE,
     ylim= c(0,max_density3))
#Adicionando linhas de referência para média,mediana e moda
abline(v= media_mmse,col= "red",lwd= 2, lty= 2) #Média
abline(v= mediana_mmse, col= "green", lwd= 2,lty= 2) #Mediana
abline(v= moda_mmse, col="blue",lwd= 2, lty= 2) #Moda
#Adicionando a curva normal
lines(x_values3,normal_curve3, col= "darkred", lwd=2, lty= 1)
#Adicionando legenda
legend("topright",
       legend= c("CurvaNormal", "Média", "Mediana", "Moda"),
       col= c("darkred", "red", "green", "blue"),
       lty= c(1, 2, 2, 2),
       lwd= 2,
       bty= "n")


# calculo medidas para ADL (Escore de Atividades de Vida Diária, variando de 0 a 10. Pontuações mais baixas indicam maior comprometimento)
media_adl_geral <- mean(dados$ADL)
cat(media_adl_geral)

mediana_adl_geral <- median(dados$ADL)
cat(mediana_adl_geral)

moda_adl_geral <- as.numeric(names(sort(table(dados$ADL), decreasing = TRUE)[1]))
cat(moda_adl_geral)

boxplot(dados$ADL,
                 main = "Boxplot do ADL",
                 ylab = "Pontuação",
                 col = "lightgreen")

amplitude_adl_geral <- max(dados$ADL)- min(dados$ADL)
cat(amplitude_adl_geral)

desvio_padrao_adl_geral <- sd(dados$ADL)
variancia_adl_geral <- var(dados$ADL)
cat(variancia_adl_geral)
cat(desvio_padrao_adl_geral)
coef_var_adl_geral <- (desvio_padrao_adl_geral / media_adl_geral)*100
cat("CV =", round(coef_var_adl_geral, 2),"%")

#Criando um vetor de valores para o eixox
x_values2<-seq(min(dados$ADL),max(dados$ADL),length=2149)
#Calculando a densidade da distribuição normal com média e desvio padrão dos dados
normal_curve2 <-dnorm(x_values2,mean= media_adl_geral,sd= desvio_padrao_adl_geral)
#Ajustando o limite superior do eixo y com base no pico da curva normal
max_density2 <-max(normal_curve2) * 1.2 #Adicionando uma margem de20%
#Criando um histograma das pontuações
hist(dados$ADL,
     main= "Histograma da pontuação ADL",
     xlab= "Pontuação",
     ylab= "Frequência",
     col= "lightblue",
     border= "black",
     probability= TRUE,
     ylim= c(0,max_density2))
#Adicionando linhas de referência para média,mediana e moda
abline(v= media_adl_geral,col= "red",lwd= 2, lty= 2) #Média
abline(v= mediana_adl_geral, col= "green", lwd= 2,lty= 2) #Mediana
abline(v= moda_adl_geral, col="blue",lwd= 2, lty= 2) #Moda
#Adicionando a curva normal
lines(x_values2,normal_curve2, col= "darkred", lwd=2, lty= 1)
#Adicionando legenda
legend("topright",
       legend= c("CurvaNormal", "Média", "Mediana", "Moda"),
       col= c("darkred", "red", "green", "blue"),
       lty= c(1, 2, 2, 2),
       lwd= 2,
       bty= "n")

# boxplot comparando pontuações de pacientes com e sem alzheimer
boxplot(ADL ~ alzheimer, data = dados,
        main = "Pontuação no ADL",
        xlab = "Alzheimer (0 = Nao, 1 = Sim)",
        ylab = "Pontuação",
        col = c("lightcoral", "lightgreen"),
        names = c("Sem alzheimer", "Com alzheimer"))
