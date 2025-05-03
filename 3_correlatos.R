# Descrição do Script:
# Esse script calcula a correlação de Pearson entre um conjunto selecionado de variáveis (Age, Gender, BMI, etc.) e a variável Diagnosis.

# Integrantes do Grupo:
# Alice Fernanda Oliveira Mercado (RA: 11202022127)
# Breno Dias Besenbruch Caruso (RA: 11202231651)
# Giovanna de Oliveira Brogliato (RA: 11202230923)
# Larissa Hikaru Watanabe (RA: 11202320482)
# Tiffany Guimarães Müller de Souza Soderi (RA: 11202130384)

# Data de Criação: 14 de abril de 2025

# Ordem de Execução:
# Execute os scripts na seguinte ordem:

# 1. '1_analise_descritiva.R'
# 2. '2_plot_histogramas.R'
# 2. '3_correlatos.R'
# '---------------------------------------------------------'


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
