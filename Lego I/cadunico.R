# Pacote que ajuda a conectar o R à internet

library(httr)

# Link para extração

link <- "https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201812.zip"

# Download da extração

GET(link, write_disk("cadunico2018.zip"), progress())

# para baixar mais de um link por vez

links <- paste0("https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_", 2012:2018,"12.zip")

for(i in 1:7){
  GET(links[i], write_disk(paste0("cadunico", i, ".zip")), progress())
  
}
# -----------------------

library(tidyverse)
library(duckdb)
library(DBI)

#importando dados do cadunico

con <- dbConnect(duckdb())

#listando arquivos (para copiar o nome)
list.files()

# importando a tabela de pessoas em 2018
pessoas18 <- tbl(con, "nomedoarquivo")

# manipulando dados
dados <- pessoas18 |>
  group_by(cd_ibge)|>
  summarise(numero_linhas = n()) |>
  collect()

pretas <- pessoas18 |>
  filter(cod_raca_cor_pessoa == 2)
  group_by(cd_ibge)|>
  summarise(pretas = n()) |>
  collect()
  
pessoas |>
  mutate(cor_raca_preta = ifelse(cod_raca_cor_pessoa == 2, 1, 0))|>
  group_by(ano) |>
  summarise(n= sum(cor_raca_preta)/n())|>
  show_query()

pessoas |>
  mutate(cor_raca_preta = ifelse(cod_raca_cor_pessoa == 2, 1, 0))|>
  group_by(cd_ibge) |>
  summarise(n= sum(cor_raca_preta)/n()) |>
  collect()

  #empilhar bases
  union_all(arquivo, arquivo2)
  tabela <- tbl(con, "nomedoarquivo/*.csv") #funciona apenas se tiver as mesmas colunas