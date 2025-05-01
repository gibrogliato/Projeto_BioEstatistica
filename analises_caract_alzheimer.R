# Alice Fernanda Oliveira Mercado, Breno Dias Besenbruch Caruso, Giovanna de Oliveira Brogliato, Larissa Hikaru Watanabe, Tiffany Guimarães Müller de Souza Soderi 
# data de criação:
# bibliotecas utilizadas:summarytools, sampling, RVAideMemoire, car, dplyr

# análises do histórico familiar, frequências dos sintomas, frequência de problemas comportamentais e medidas das pontuações de MMSE e ADL. 
# abrindo o banco de dados
dados <- read.csv('alzheimers_disease_data.csv')

# verificando a frequência do histórico familiar de alzheimer
ctable(x = dados$Diagnosis, y = dados$FamilyHistoryAlzheimers, prop = "r", headings = FALSE)

#----------- ------------------------- -------------- ------------- ---------------
#  FamilyHistoryAlzheimers              0             1           Total
#Diagnosis                                                                       
#0                             1024 (73.7%)   365 (26.3%)   1389 (100.0%)
#1                              583 (76.7%)   177 (23.3%)    760 (100.0%)
#Total                             1607 (74.8%)   542 (25.2%)   2149 (100.0%)
#----------- ------------------------- -------------- ------------- ---------------

# realizando um qui-quadrado para verificar se o histórico familiar tem relação com o alzheimer
chisq.test(table(dados$Diagnosis, dados$FamilyHistoryAlzheimers))

#Pearson's Chi-squared test with Yates' continuity correction

#data:  table(dados$Diagnosis, dados$FamilyHistoryAlzheimers)
#X-squared = 2.1703, df = 1, p-value = 0.1407

# análises dos sintomas
# criando tabela com todos os sintomas para verificar as frequências em pacientes com e sem alzheimer
tabela_sintomas <- table(esquecimento = dados$Forgetfulness, reclamações_memória = dados$MemoryComplaints, confusão = dados$Confusion, desorientação = dados$Disorientation, mudança_personalidade = dados$PersonalityChanges, dificuldade_tarefas = dados$DifficultyCompletingTasks, alzheimer = dados$Diagnosis)
ftable(tabela_sintomas)


# verificando se existe associação entre os sintomas e o diagnóstico
# como são variáveis categóricas, usamos o método qui-quadrado

chisq.test(table(dados$Diagnosis, dados$Forgetfulness))

#Pearson's Chi-squared test with Yates' continuity correction
#data:  table(dados$Diagnosis, dados$Forgetfulness)
#X-squared = 1.2942e-29, df = 1, p-value = 1


chisq.test(table(dados$Diagnosis, dados$MemoryComplaints))

#Pearson's Chi-squared test with Yates' continuity correction
#data:  table(dados$Diagnosis, dados$MemoryComplaints)
#X-squared = 200.62, df = 1, p-value < 2.2e-16


chisq.test(table(dados$Diagnosis, dados$Confusion))

#Pearson's Chi-squared test with Yates' continuity correction
#data:  table(dados$Diagnosis, dados$Confusion)
#X-squared = 0.69479, df = 1, p-value = 0.4045

chisq.test(table(dados$Diagnosis, dados$Disorientation))

#Pearson's Chi-squared test with Yates' continuity correction
#data:  table(dados$Diagnosis, dados$Disorientation)
#X-squared = 1.1681, df = 1, p-value = 0.2798

chisq.test(table(dados$Diagnosis, dados$PersonalityChanges))

#Pearson's Chi-squared test with Yates' continuity correction
#data:  table(dados$Diagnosis, dados$PersonalityChanges)
#X-squared = 0.79778, df = 1, p-value = 0.3718


# criando uma tabela de frequência para os problemas comportamentais em pessoas com e sem Alzheimer
ctable(x = dados$Diagnosis, y = dados$BehavioralProblems, prop = "r", headings = FALSE)

#----------- -------------------- -------------- ------------- ---------------
#              BehavioralProblems              0             1           Total
#  Diagnosis                                                                  
#          0                        1255 (90.4%)   134 ( 9.6%)   1389 (100.0%)
#          1                         557 (73.3%)   203 (26.7%)    760 (100.0%)
#      Total                        1812 (84.3%)   337 (15.7%)   2149 (100.0%)
#----------- -------------------- -------------- ------------- ---------------

# teste qui-quadrado para verificar se problemas comportamentais possuem relação com o Alzheimer
chisq.test(table(dados$Diagnosis, dados$BehavioralProblems))

#	Pearson's Chi-squared test with Yates' continuity correction

#data:  table(dados$Diagnosis, dados$BehavioralProblems)
#X-squared = 106.88, df = 1, p-value < 2.2e-16


# análise da pontuação do MMSE (mini exame de estado mental pontuações variam de 0 a 30), onde pontuações mais baixas indicam comprometimento cognitivo (diminuição das funçõescognitivas como memória, atenção, raciocínio etc)
# cálculo das medidas de tendência central

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


# boxplot comparando pontuações de pacientes com e sem alzheimer
boxplot(mmse_arred ~ Diagnosis, data = dados,
                 main = "Pontuação no MMSE",
                 xlab = "Alzheimer",
                 ylab = "Pontuação",
                 col = c("lightcoral", "lightgreen"),
                 names = c("Não", "Sim"))

#criando uma amostra estratificada para realizar o teste t para amostras independentes e verificar se a presença de alzheimer influencia nas pontuações do teste MMSE
library(sampling)
amostra_estratificada <- strata(dados, stratanames = "Diagnosis", size = c("0" = 30, "1" = 8), method = "srswor")
amostra_final <- getdata(dados, amostra_estratificada)
library(RVAideMemoire)

# realizando um teste de normalidade
byf.shapiro(MMSE ~ Diagnosis, amostra_final)
#Shapiro-Wilk normality tests 

#data:  MMSE by Diagnosis 
#       W p-value    
#0 0.9402 0.09229 .
#1 0.9417 0.62754  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# traduzindo 0 e 1 do diagnóstico em variáveis nominais para realizar o teste de levene
# obs: sem a realização deste passo o teste não estava funcionando
amostra_final$alzheimer <- ifelse(amostra_final$Diagnosis == 0, "não", "sim")
library(car)

# realizando um teste de homogeneidade
leveneTest(MMSE ~ alzheimer, amostra_final, center=mean)
#      Df F value  Pr(>F)  
#group  1  0.0579 0.8112
#      36 

# realizando o teste t
t.test(MMSE ~ Diagnosis, amostra_final, var.equal=FALSE)
#Welch Two Sample t-test 

#data:  MMSE by Diagnosis
#t = 1.434, df = 10.397, p-value = 0.181
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -2.197179 10.248415
#sample estimates:
#  mean in group 0 mean in group 1 
#19.25132        15.22571

# gerando o gráfico da distribuição t de student
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

# cálculos das medidas de tendência central
# arredondando os valores e criando uma nova variavel
dados$adl_arred <- round(dados$ADL)

media_adl_arred <- mean(dados$adl_arred)
cat(media_adl_arred)
# 4.985575

mediana_adl_arred <- median(dados$adl_arred)
cat(mediana_adl_arred)
# 5

moda_adl_arred <- as.numeric(names(sort(table(dados$adl_arred), decreasing = TRUE)[1]))
cat(moda_adl_arred)
# 9

# cálculos das medidas de dispersão
amplitude_adl_arred <- max(dados$adl_arred)- min(dados$adl_arred)
cat(amplitude_adl_arred)
# 10

desvio_padrao_adl_arred <- sd(dados$adl_arred)
variancia_adl_arred <- var(dados$adl_arred)
cat(variancia_adl_arred)
# 8.949978
cat(desvio_padrao_adl_arred)
# 2.991651
coef_var_adl_arred <- (desvio_padrao_adl_arred / media_adl_arred)*100
cat("CV =", round(coef_var_adl_arred, 2),"%")
# CV = 60.01 %


# boxplot comparando pontuações de pacientes com e sem alzheimer
boxplot(adl_arred ~ Diagnosis, data = dados,
        main = "Pontuação no ADL",
        xlab = "Alzheimer",
        ylab = "Pontuação",
        col = c("lightcoral", "lightgreen"),
        names = c("Não", "Sim"))


# utilizando a amostra estratificada e realizando teste t para amostras independentes para verificar se a presença de alzheimer influencia na pontuação do ADL


# realizando o teste de normalidade
byf.shapiro(ADL ~ Diagnosis, amostra_final)
#	Shapiro-Wilk normality tests 
#data:  ADL by Diagnosis 
#W p-value
#0 0.9668  0.4555
#1 0.9505  0.7167


# realizando o teste de homogeneidade
leveneTest(ADL ~ alzheimer, amostra_final, center=mean)
#Levene's Test for Homogeneity of Variance (center = mean)
#      Df F value Pr(>F)
#group  1  1.1286 0.2952
#       36 

# realizando teste t
t.test(ADL ~ Diagnosis, amostra_final, var.equal=FALSE)
#Welch Two Sample t-test 
#data:  ADL by Diagnosis
#t = 6.3194, df = 15.129, p-value = 1.322e-05
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  2.847304 5.742348
#sample estimates:
#  mean in group 0 mean in group 1 
#6.434223        2.139396


# gerando o gráfico da distribuição t de student
resultado_t <- t.test(ADL ~ Diagnosis, amostra_final, var.equal=FALSE)
t_obs <- resultado_t$statistic
gl <- resultado_t$parameter
alpha <- 0.05
t_crit <- qt(1- alpha/2, df = gl)
x <- seq(-7, 7, length = 200)
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

