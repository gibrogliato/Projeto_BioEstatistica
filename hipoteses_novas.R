#Agora entendendo que algumas variáveis podem ter correlações, vamos propor hipóteses.

#Verificando componentes do teste
# Teste F para comparar as variâncias entre duas variáveis contínuas
#Para esses testes vamos assumir a H0 de que as variâncias são iguais e H1 de que não são iguais
#Variáveis: "CardiovascularDisease", "Diabetes", "Depression", "Hypertension",
#"CholesterolTotal", "CholesterolLDL", "CholesterolHDL", "CholesterolTriglycerides"

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

# Visualizando os resultados da ANOVA
variaveis <- c("CardiovascularDisease", "Diabetes", "Depression", "Hypertension",
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


#Conclusão
#A partir da análise de variância realizada, apenas o colesterol HDL apresentou diferença estatisticamente significativa entre os grupos com e sem diagnóstico de Alzheimer.
#Isso sugere que o HDL pode estar associado ao diagnóstico na amostra analisada.