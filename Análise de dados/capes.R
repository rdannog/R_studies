###############################
#  Lista II: Análise de dados #
#                             #
#   Dan Nogueira da Silva     #
###############################

### Nesta lista, o objetivo será analisar a produção de teses e dissertações de programas de pós-graduação notas 4, 5, 6 e 7 na Capes, das áreas de Sociologia ou Ciência Política e Relações Internacionais, entre os anos de 1987 e 2022.

### ---------------------------------------------


### Carregando pacotes necessários

library(tidyverse)
library(rio)


### 1. Importando arquivos ###

# Usando map_df é possível combinar todas as planilhas contendo as teses e dissertações defendidas entre 1987 a 2022, em um único tibble

# Optei por utilizar o map_df pois nesse caso achei mais prático do que usar o loop, por economizar linhas e espaço de memória

banco_teses_total <- map_df(c("csvs/capes_1987-1992.csv", "csvs/capes_1993-1998.csv", "csvs/capes_1999-2004.csv", "csvs/capes_2005-2010.csv", "csvs/capes_2011-2016.csv", "csvs/capes_2017-2022.csv"),read_delim, delim = ",")

# Carregando a planilha com informações sobre os programas de pós-graduação

banco_programas_total <- import("programas.csv")  

# Juntando informações sobre os programas com informações de teses e dissertações

teses_e_programas <- banco_teses_total |>
  left_join(banco_programas_total, by = c("codigo_programa" = "CD_PROGRAMA"))

# Filtrando apenas observações da minha área de interesse, de programas com notas 4, 5, 6, e 7

teses_sociologia <- teses_e_programas |>
  filter(CONCEITO == 4|5|6|7) |>
  filter(str_detect(nome_programa, "SOCIOLOGIA"))


### 2. Seleção de palavras-chave ###

# Escolha 3 palavras-chave relevantes para o seu problema de pesquisa.

teses_filtradas <- teses_sociologia |>
  filter(str_detect(palavras_chave,"ensino superior|desigualdade|juventude"))


## 3. Evolução ao longo do tempo

#Crie uma visualização que reporte de forma sucinta e informativa a produção de teses e dissertações no seu tema defendidos por ano.



ggplot(teses_filtradas, aes(x = ano)) + 
  geom_bar() +
  labs(title = "Teses e dissertações de sociologia, defendidas entre 1987-2022", legend= "(Palavras-Chave: ensino superior; estratificação; educação; juventude)", y= "Frequência de Defesas", x= "Anos")

