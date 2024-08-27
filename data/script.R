library(dplyr)
library(readxl)
library(lubridate)
library(solitude)

cota19a23 <- readRDS('data/cota19a23.rds')

url24 <- "https://www.camara.leg.br/cotas/Ano-2024.xlsx"
arquivo_local <- "Ano-2024.xlsx"
download.file(url24, destfile = arquivo_local, mode = "wb")

cota24 <- read_excel(arquivo_local) |> 
  select(txNomeParlamentar, nuLegislatura, sgUF, sgPartido, txtDescricao, txtDescricaoEspecificacao, txtFornecedor, txtCNPJCPF, datEmissao, vlrLiquido, numAno, urlDocumento) |> 
  mutate(datEmissao = as.Date(datEmissao, format = "%Y-%m-%d"))

file.remove('Ano-2024.xlsx')

cota <- cota19a23 |> 
  full_join(cota24) 


# Função para identificar outliers usando Isolation Forest, ignorando valores negativos
isolation_forest_suspect <- function(data) {
  # Garantir que o tamanho mínimo do dataset seja suficiente
  if (nrow(data) < 5) {
    data$Suspeito <- 0  # Não marcar como suspeito se o dataset for muito pequeno
    return(data)
  }
  
  # Marcar valores negativos como "Não" suspeitos
  data$Suspeito <- 0
  
  # Aplicar o modelo apenas aos valores não negativos
  non_negative_data <- data[data$vlrLiquido >= 0, ]
  
  if (nrow(non_negative_data) >= 5) {
    isolation_model <- isolationForest$new(
      sample_size = min(256, nrow(non_negative_data)),  # Ajustar o tamanho da amostra
      max_depth = 8  # Definir explicitamente max_depth
    )
    
    isolation_model$fit(data.frame(vlrLiquido = non_negative_data$vlrLiquido))
    prediction <- isolation_model$predict(data.frame(vlrLiquido = non_negative_data$vlrLiquido))
    threshold <- quantile(prediction$anomaly_score, 0.95)
    
    non_negative_data$Suspeito <- ifelse(prediction$anomaly_score > threshold, 1, 0)
  }
  
  # Atualizar a coluna "Suspeito" nos dados originais
  data[data$vlrLiquido >= 0, ]$Suspeito <- non_negative_data$Suspeito
  
  return(data)
}


# Aplicar o modelo Isolation Forest por grupo e adicionar a coluna "Suspeito"
cota <- cota %>%
  group_by(numAno, txtDescricao) %>%
  do(isolation_forest_suspect(.)) %>%
  ungroup()


saveRDS(cota, 'data/cota.rds')
