library(dplyr)
library(readxl)
library(lubridate)

cota08a23 <- readRDS('data/cota08a23.rds')

url24 <- "https://www.camara.leg.br/cotas/Ano-2024.xlsx"
arquivo_local <- "Ano-2024.xlsx"
download.file(url24, destfile = arquivo_local, mode = "wb")

cota24 <- read_excel(arquivo_local) |> 
  filter(sgUF == 'SE')

file.remove('Ano-2024.xlsx')

cota <- cota08a23 |> 
  mutate(cpf = as.numeric(gsub("\\.", "", cpf))) |> 
  full_join(cota24) |> 
  mutate(data = ymd_hms(datEmissao))

cota <- cota |> 
  select(txNomeParlamentar, nuLegislatura, sgUF, sgPartido, txtDescricao, txtDescricaoEspecificacao, txtFornecedor, txtCNPJCPF, datEmissao, vlrLiquido, numAno, urlDocumento, data)

saveRDS(cota, 'data/cota.rds')
