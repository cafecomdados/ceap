library(dplyr)
library(readxl)
library(lubridate)
library(solitude)

cota19a23 <- readRDS('data/cota19a23.rds')

url24 <- "https://www.camara.leg.br/cotas/Ano-2024.xlsx"
arquivo_local <- "Ano-2024.xlsx"
download.file(url24, destfile = arquivo_local, mode = "wb")

cota24 <- read_excel(arquivo_local) |> 
  select(txNomeParlamentar, ideCadastro, sgUF, sgPartido, txtDescricao, txtDescricaoEspecificacao, txtFornecedor, txtCNPJCPF, datEmissao, vlrLiquido, numAno, urlDocumento) |> 
  mutate(datEmissao = as.Date(datEmissao, format = "%Y-%m-%d"))

file.remove('Ano-2024.xlsx')

cota <- cota19a23 |> 
  full_join(cota24) 


# Função para identificar outliers usando Isolation Forest, considerando todos os valores
isolation_forest_suspect <- function(data) {
  # Garantir que o tamanho mínimo do dataset seja suficiente
  if (nrow(data) < 5) {
    data$Suspeito <- 0  # Não marcar como suspeito se o dataset for muito pequeno
    return(data)
  }
  
  # Inicializar a coluna "Suspeito" com 0
  data$Suspeito <- 0
  
  # Aplicar o modelo Isolation Forest a todos os valores
  isolation_model <- isolationForest$new(
    sample_size = min(256, nrow(data)),  # Ajustar o tamanho da amostra
    max_depth = 8  # Definir explicitamente max_depth
  )
  
  isolation_model$fit(data.frame(vlrLiquido = data$vlrLiquido))
  prediction <- isolation_model$predict(data.frame(vlrLiquido = data$vlrLiquido))
  threshold <- quantile(prediction$anomaly_score, 0.95)
  
  data$Suspeito <- ifelse(prediction$anomaly_score > threshold, 1, 0)
  
  return(data)
}


# Aplicar o modelo Isolation Forest por grupo e adicionar a coluna "Suspeito"
cota <- cota %>%
  group_by(numAno, txtDescricao) %>%
  do(isolation_forest_suspect(.)) %>%
  ungroup()

cota <- cota |> 
  mutate(txtDescricao = case_when(
    txtDescricao == "ASSINATURA DE PUBLICAÇÕES" ~ "Assinaturas e Publicações",
    txtDescricao == "COMBUSTÍVEIS E LUBRIFICANTES." ~ "Combustíveis e Lubrificantes",
    txtDescricao == "CONSULTORIAS, PESQUISAS E TRABALHOS TÉCNICOS." ~ "Consultorias e Trabalhos Técnicos",
    txtDescricao == "DIVULGAÇÃO DA ATIVIDADE PARLAMENTAR." ~ "Divulgação Parlamentar",
    txtDescricao == "FORNECIMENTO DE ALIMENTAÇÃO DO PARLAMENTAR" ~ "Alimentação do Parlamentar",
    txtDescricao == "HOSPEDAGEM ,EXCETO DO PARLAMENTAR NO DISTRITO FEDERAL." ~ "Hospedagem (exceto no DF)",
    txtDescricao == "LOCAÇÃO OU FRETAMENTO DE AERONAVES" ~ "Locação de Aeronaves",
    txtDescricao == "LOCAÇÃO OU FRETAMENTO DE EMBARCAÇÕES" ~ "Locação de Embarcações",
    txtDescricao == "LOCAÇÃO OU FRETAMENTO DE VEÍCULOS AUTOMOTORES" ~ "Locação de Veículos",
    txtDescricao == "MANUTENÇÃO DE ESCRITÓRIO DE APOIO À ATIVIDADE PARLAMENTAR" ~ "Manutenção de Escritório Parlamentar",
    txtDescricao == "PARTICIPAÇÃO EM CURSO, PALESTRA OU EVENTO SIMILAR" ~ "Participação em Cursos e Eventos",
    txtDescricao == "PASSAGEM AÉREA - REEMBOLSO" ~ "Passagem Aérea - Reembolso",
    txtDescricao == "PASSAGEM AÉREA - RPA" ~ "Passagem Aérea - RPA",
    txtDescricao == "PASSAGEM AÉREA - SIGEPA" ~ "Passagem Aérea - SIGEPA",
    txtDescricao == "PASSAGENS TERRESTRES, MARÍTIMAS OU FLUVIAIS" ~ "Passagens Terrestres/Marítimas/Fluviais",
    txtDescricao == "SERVIÇO DE SEGURANÇA PRESTADO POR EMPRESA ESPECIALIZADA." ~ "Serviço de Segurança",
    txtDescricao == "SERVIÇO DE TÁXI, PEDÁGIO E ESTACIONAMENTO" ~ "Táxi, Pedágio e Estacionamento",
    txtDescricao == "SERVIÇOS POSTAIS" ~ "Serviços Postais",
    txtDescricao == "TELEFONIA" ~ "Telefonia",
    txtDescricao == "AQUISIÇÃO DE TOKENS E CERTIFICADOS DIGITAIS" ~ "Tokens e Certificados Digitais",
    TRUE ~ txtDescricao 
  ))

saveRDS(cota, 'data/cota.rds')
