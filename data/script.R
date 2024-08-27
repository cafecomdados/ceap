library(dplyr)
library(readxl)
library(lubridate)

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

saveRDS(cota, 'data/cota.rds')
