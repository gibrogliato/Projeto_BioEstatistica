
#Vamos estudar as correlações possíveis nas variáveis que analisamos antes. 
#Variáveis: idade, gênero, IMC, atividade física, qualidade de sono e consumo de álcool. 

variaveis <- c("Age", "Gender", "BMI", "PhysicalActivity", 
               "AlcoholConsumption", "SleepQuality")

# Converter Diagnosis para numérico
dados_alzheimer$Diagnosis <- as.numeric(as.character(dados_alzheimer$Diagnosis))

correlacoes <- sapply(variaveis, function(var) {
  dados_alzheimer[[var]] <- as.numeric(as.character(dados_alzheimer[[var]]))  # garante que está numérico
  cor(dados_alzheimer[[var]], dados_alzheimer$Diagnosis, method = "pearson")
})

# Montar a tabela com os resultados
tabela_correlacoes <- data.frame(
  Variavel = variaveis,
  Correlacao_com_Diagnostico = round(correlacoes, 4)
)

# Ordenar pela correlação
tabela_correlacoes <- tabela_correlacoes[order(-abs(tabela_correlacoes$Correlacao_com_Diagnostico)), ]
print(tabela_correlacoes, row.names = FALSE)
