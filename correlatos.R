#Vamos estudar as correlações possíveis nas variáveis que analisamos antes. 
#Variáveis: idade, gênero, IMC, atividade física, qualidade de sono e consumo de álcool. 

#Veremos se a Idade tem correlação com o diagnóstico
correlacao_idade <- cor(dados_alzheimer$Age, dados_alzheimer$Diagnosis, method = "pearson")
sprintf("Coeficiente de Correlação de Pearson: %.4f", correlacao_idade)

#Veremos se o IMC tem correlação com o diagnóstico
correlacao_imc <- cor(dados_alzheimer$BMI, dados_alzheimer$Diagnosis, method = "pearson")
sprintf("Coeficiente de Correlação de Pearson: %.4f", correlacao_imc)

#Veremos se a atividade física tem correlação com o diagnóstico
correlacao_af <- cor(dados_alzheimer$PhysicalActivity, dados_alzheimer$Diagnosis, method = "pearson")
sprintf("Coeficiente de Correlação de Pearson: %.4f", correlacao_af)

#Veremos se a qualidade de sono tem correlação com o diagnóstico
correlacao_sono <- cor(dados_alzheimer$SleepQuality, dados_alzheimer$Diagnosis, method = "pearson")
sprintf("Coeficiente de Correlação de Pearson: %.4f", correlacao_sono)

#Por fim, veremos se o consumo de álcool tem correlação com o diagnóstico
correlacao_alcool <- cor(dados_alzheimer$AlcoholConsumption, dados_alzheimer$Diagnosis, method = "pearson")
sprintf("Coeficiente de Correlação de Pearson: %.4f", correlacao_alcool)