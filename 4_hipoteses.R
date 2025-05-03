# Descrição do Script:
# Esse script realiza testes de hipótese (usando ANOVA) para verificar se existem diferenças significativas
# nas médias de diversas métricas de saúde (doenças cardiovasculares, diabetes, colesterol, etc) 
# entre pacientes com e sem diagnóstico de Alzheimer.

# Integrantes do Grupo:
# Alice Fernanda Oliveira Mercado (RA: 11202022127)
# Breno Dias Besenbruch Caruso (RA: 11202231651)
# Giovanna de Oliveira Brogliato (RA: 11202230923)
# Larissa Hikaru Watanabe (RA: 11202320482)
# Tiffany Guimarães Müller de Souza Soderi (RA: 11202130384)

# Data de Criação: 16 de abril de 2025

# Ordem de Execução:
# Execute os scripts na seguinte ordem:

# 1. '1_analise_descritiva.R'
# 2. '2_plot_histogramas.R'
# 3. '3_correlatos.R'
# 4. '4_hipoteses'
# '---------------------------------------------------------'

#Agora entendendo que algumas variáveis podem ter correlações, vamos propor hipóteses.

#Verificando componentes do teste
# Teste F para comparar as variâncias entre duas variáveis contínuas
#Para esses testes vamos assumir a H0 de que as variâncias são iguais e H1 de que não são iguais
#Variáveis: "CardiovascularDisease", "Diabetes", "Depression", "Hypertension",
#"CholesterolTotal", "CholesterolLDL", "CholesterolHDL", "CholesterolTriglycerides",
# "Age","BMI", "PhysicalActivity","SleepQuality", "AlcoholConsumption"

var.test(Age ~ Diagnosis, data = dados_alzheimer)
var.test(BMI ~ Diagnosis, data = dados_alzheimer)
var.test(PhysicalActivity ~ Diagnosis, data = dados_alzheimer)
var.test(SleepQuality ~ Diagnosis, data = dados_alzheimer)
var.test(AlcoholConsumption ~ Diagnosis, data = dados_alzheimer)

var.test(CardiovascularDisease ~ Diagnosis, data = dados_alzheimer)
var.test(Diabetes ~ Diagnosis, data = dados_alzheimer)
var.test(Depression ~ Diagnosis, data = dados_alzheimer)
var.test(Hypertension ~ Diagnosis, data = dados_alzheimer)
var.test(CholesterolTotal ~ Diagnosis, data = dados_alzheimer)
var.test(CholesterolLDL ~ Diagnosis, data = dados_alzheimer)
var.test(CholesterolHDL ~ Diagnosis, data = dados_alzheimer)
var.test(CholesterolTriglycerides ~ Diagnosis, data = dados_alzheimer)



#Podemos proceder para estudos por ANOVA para as variáveis.

#Doença cardiovascular e Diagnóstico
#Hipótese Nula (H0): Não há diferença na média de cd entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa na média de cd entre os pacientes com diagnóstico positivo e negativo.

anova_result_cd <- aov(CardiovascularDisease ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_cd)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.


#Diabetes e Diagnóstico
#Hipótese Nula (H0): Não há diferença na média de diabetes entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa na média de diabetes entre os pacientes com diagnóstico positivo e negativo.

anova_result_diabetes <- aov(Diabetes ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_diabetes)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Depressão e Diagnóstico
#Hipótese Nula (H0): Não há diferença significativa no depressão entre pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no depressão entre pacientes com diagnóstico positivo e negativo.

anova_result_dp <- aov(Depression ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_dp)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Hipertensão e Diagnóstico
#Hipótese Nula (H0): Não há diferença na hipertensão entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa na hipertensão entre os pacientes com diagnóstico positivo e negativo.

anova_result_hipertensão <- aov(Hypertension ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_hipertensão)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Colesterol Total e Diagnóstico
#Hipótese Nula (H0): Não há diferença no colesterol total entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no colesterol total entre os pacientes com diagnóstico positivo e negativo.

anova_result_ct <- aov(CholesterolTotal ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_ct)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Colesterol LDL e Diagnóstico
#Hipótese Nula (H0): Não há diferença no colesterol LDL entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no colesterol LDL entre os pacientes com diagnóstico positivo e negativo.

anova_result_ldl <- aov(CholesterolLDL ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_ldl)

#Não há diferença significativa entre os grupos e eu posso aceitar a hipótese nula.

#Colesterol HDL e Diagnóstico
#Hipótese Nula (H0): Não há diferença no colesterol HDL entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no colesterol HDL entre os pacientes com diagnóstico positivo e negativo.

anova_result_hdl <- aov(CholesterolHDL ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_hdl)

#Há diferença significativa entre os grupos e eu posso rejeitar a hipótese nula.

#Colesterol Triglicerides e Diagnóstico
#Hipótese Nula (H0): Não há diferença no colesterol triglicerides entre os pacientes com diagnóstico positivo e negativo.
#Hipótese Alternativa (H1): Existe uma diferença significativa no colesterol triglicerides entre os pacientes com diagnóstico positivo e negativo.

anova_result_trigl <- aov(CholesterolTriglycerides ~ Diagnosis, data = dados_alzheimer)
summary(anova_result_trigl)

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

# Visualizando os resultados da ANOVA
variaveis <- c("Age","BMI", "PhysicalActivity","SleepQuality", "AlcoholConsumption",
               "CardiovascularDisease", "Diabetes", "Depression", "Hypertension",
               "CholesterolTotal", "CholesterolLDL", "CholesterolHDL", "CholesterolTriglycerides")

# Criar uma tabela com os valores-p da ANOVA
anova_pvalores <- sapply(variaveis, function(var) {
  modelo <- aov(as.formula(paste(var, "~ Diagnosis")), data = dados_alzheimer)
  summary(modelo)[[1]][["Pr(>F)"]][1]  # extrai o valor-p
})

# Montar a tabela
tabela_anova <- data.frame(
  Variavel = variaveis,
  Valor_p = round(anova_pvalores, 4),
  Significativo = ifelse(anova_pvalores < 0.05, "Sim", "Não")
)

print(tabela_anova, row.names = FALSE)

#Como apenas no caso do Colesterol - HDL temos uma diferença significativa dos dados, quero saber onde ela está. 
#Por isso farei um teste HSD Tukey

# Realizando o teste de Tukey para identificar quais grupos são diferentes
# Convertendo a variável Diagnosis para fator, para o Tukey funcionar
dados_alzheimer$Diagnosis <- as.factor(dados_alzheimer$Diagnosis)
anova_result_hdl <- aov(CholesterolHDL ~ Diagnosis, data = dados_alzheimer)
TukeyHSD(anova_result_hdl)

#Visualizando
boxplot(CholesterolHDL ~ Diagnosis, data = dados_alzheimer, col = "lightblue")

#Existe uma diferença significativa nos níveis de colesterol HDL entre os grupos 
#com e sem Alzheimer, com níveis maiores no grupo com diagnóstico positivo.
#Isso indica que pacientes com Alzheimer (grupo 1) têm, em média, 2.06 unidades a mais de HDL que os pacientes sem Alzheimer.


#Como Na Qualidade de Sono também temos uma diferença significativa dos dados, quero saber onde ela está. 
#Por isso farei um teste HSD Tukey

# Realizando o teste de Tukey para identificar quais grupos são diferentes
# Convertendo a variável Diagnosis para fator, para o Tukey funcionar
dados_alzheimer$Diagnosis <- as.factor(dados_alzheimer$Diagnosis)
anova_result_sono <- aov(SleepQuality ~ Diagnosis, data = dados_alzheimer)
TukeyHSD(anova_result_sono)

#Visualizando
boxplot(SleepQuality ~ Diagnosis, data = dados_alzheimer, col = "lightblue")

#A análise post-hoc de Tukey revelou que os indivíduos com diagnóstico positivo de Alzheimer apresentaram qualidade de sono significativamente menor 
#do que os sem diagnóstico (p = 0.01),enquanto não houve diferença entre os demais grupos.


#Conclusão
#A partir da análise de variância realizada, o colesterol HDL apresentou diferença estatisticamente significativa entre os grupos com e sem diagnóstico de Alzheimer.
#Isso sugere que o HDL pode estar associado ao diagnóstico na amostra analisada.

#SleepQuality foi uma ariável preditora significativa com p < 0.05. Isso significa que, mantidas as outras variáveis constantes, 
#pessoas com qualidade de sono mais baixa têm maior chance de diagnóstico positivo para Alzheimer.
