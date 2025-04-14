#Agora entendendo que algumas variáveis podem ter correlações, vamos propor hipóteses.

#Verificando componentes do teste
shapiro.test(dados_alzheimer$Age[dados_alzheimer$Diagnosis == "Positivo"])
# Teste F para comparar as variâncias entre duas variáveis contínuas
#Para esses testes vamos assumir a H0 de que as variâncias são iguais e H1 de que não são iguais
var.test(Age ~ Diagnosis, data = dados_alzheimer)
var.test(BMI ~ Diagnosis, data = dados_alzheimer)
var.test(PhysicalActivity ~ Diagnosis, data = dados_alzheimer)
var.test(SleepQuality ~ Diagnosis, data = dados_alzheimer)
var.test(AlcoholConsumption ~ Diagnosis, data = dados_alzheimer)

#Podemos proceder para estudos por ANOVA para as variáveis.

#Idade e Diagnóstico
#Hipótese Nula (H0): Não há diferença na média de idade entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa na média de idade entre os pacientes com diagnóstico positivo e negativo.

anova_result_id <- aov(Age ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_id)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#IMC e Diagnóstico
#Hipótese Nula (H0): Não há diferença significativa no IMC entre pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no IMC entre pacientes com diagnóstico positivo e negativo.

anova_result_bmi <- aov(BMI ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_bmi)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Qualidade de Sono e Diagnóstico
#Hipótese Nula (H0): Não há diferença na qualidade do sono entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa na qualidade do sono entre os pacientes com diagnóstico positivo e negativo.


anova_result_sono <- aov(SleepQuality ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_sono)

#Há diferença significativa entre os grupos e eu posso rejeitar a hipótese nula.


#Consumo de Álcool e Diagnóstico
#Hipótese Nula (H0): Não há diferença no consumo de álcool entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no consumo de álcool entre os pacientes com diagnóstico positivo e negativo.


anova_result_alcool <- aov(AlcoholConsumption ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_alcool)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Atividade Física e Diagnóstico
#Hipótese Nula (H0): Não há diferença na atividade física entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa na atividade física entre os pacientes com diagnóstico positivo e negativo.

anova_result_af <- aov(PhysicalActivity ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_af)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Como apenas no caso da Qualidade de Sono temos uma diferença significativa dos dados, quero saber onde ela está. 
#Por isso farei um teste HSD Tukey

# Realizando o teste de Tukey para identificar quais grupos são diferentes
# Convertendo a variável Diagnosis para fator, para o Tukey funcionar
dados_alzheimer$Diagnosis <- as.factor(dados_alzheimer$Diagnosis)
anova_result_sono <- aov(SleepQuality ~ Diagnosis, data = dados_alzheimer)
TukeyHSD(anova_result_sono)

#Visualizando
boxplot(SleepQuality ~ Diagnosis, data = dados_alzheimer, col = "lightblue")

#A análise post-hoc de Tukey revelou que os indivíduos com diagnóstico positivo de Alzheimer apresentaram qualidade de sono significativamente menor 
#do que os sem diagnóstico (p = 0.01), enquanto não houve diferença entre os demais grupos.

#Conclusão
#SleepQuality foi a única variável preditora significativa com p < 0.05. Isso significa que, mantidas as outras variáveis constantes, 
#pessoas com qualidade de sono mais baixa têm maior chance de diagnóstico positivo para Alzheimer.