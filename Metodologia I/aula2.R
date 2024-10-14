# Instalar os pacotes necessários
if (!require(tidyverse)) {
  install.packages("tidyverse")
}
if (!require(readxl)) {
  install.packages("readxl")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}

if (!require(janitor)) {
  install.packages("janitor")
}

# Carregar o tidyverse
library(tidyverse)
library(dplyr)
# Carregar a base de dados
setwd("~/R_studies/Metodologia I") ## Aqui vocês colocam o caminho de vocês#

teste <- readxl::read_excel("teste.xlsx")

tabyl
# Verificar as primeiras linhas da base de dados com head
head(teste)

# Verificar as últimas linhas da base de dados com tail
tail(teste)

# Verificar a estrutura da base de dados com glimpse
glimpse(teste)

# Renomear as variáveis com nomes numéricos
teste <- teste |> 
  mutate(2.0, var2)  # substituir ... com os nomes desejados




# Calcular estatísticas descritivas para variáveis numéricas
teste %>% 
  summarise(
    media_filhos = mean(var1, na.rm = TRUE),
    moda_filhos = mode(var1, na.rm = TRUE),
    mediana_filhos = median(var1, na.rm = TRUE),
    desvio_padrao_filhos = sd(var1, na.rm = TRUE),
    media_idade = mean(var2, na.rm = TRUE),
    moda_idade = mode(var2, na.rm = TRUE),
    mediana_idade = median(var2, na.rm = TRUE),
    desvio_padrao_idade = sd(var2, na.rm = TRUE),
    media_renda = mean(var3, na.rm = TRUE),
    moda_renda = mode(var3, na.rm = TRUE),
    mediana_renda = median(var3, na.rm = TRUE),
    desvio_padrao_renda = sd(var3, na.rm = TRUE)
  )

# Padronizar os dados para comparar
teste_padronizado <- teste %>% 
  mutate(
    var1_padronizado = (var1 - mean(var1, na.rm = TRUE)) / sd(var1, na.rm = TRUE),
    var2_padronizado = (var2 - mean(var2, na.rm = TRUE)) / sd(var2, na.rm = TRUE),
    var3_padronizado = (var3 - mean(var3, na.rm = TRUE)) / sd(var3, na.rm = TRUE)
  )



#Agrupar os dados por cor/raça e calcular o desvio padrão para cada grupo
teste %>% 
  group_by(cor_raça) %>% 
  summarise(
    desvio_padrao_filhos = sd(var1, na.rm = TRUE),
    desvio_padrao_idade = sd(var2, na.rm = TRUE),
    desvio_padrao_renda = sd(var3, na.rm = TRUE)
  )