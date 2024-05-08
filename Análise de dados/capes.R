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



### 3. Evolução ao longo do tempo ###

## Crie uma visualização que reporte de forma sucinta e informativa a produção de teses e dissertações no seu tema defendidos por ano.


# Criando uma tabela de contagem de teses por ano e palavra-chave

teses_por_ano_subtema <- teses_filtradas |>
  mutate(subtema = case_when(
    str_detect(palavras_chave, "ensino superior") ~ "Ensino Superior",
    str_detect(palavras_chave, "desigualdade") ~ "Desigualdade",
    str_detect(palavras_chave, "juventude") ~ "Juventude",
    TRUE ~ "Outros"
  )) |>
  count(ano, subtema) |>
  rename(frequencia = n)

# Criando o gráfico de barras

ggplot(teses_por_ano_subtema, aes(x = ano, y = frequencia, fill = subtema)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Produção de Teses e Dissertações em Sociologia, por Palavra-Chave (1987-2022)",
       x = "Ano de Defesa", y = "Número de Teses e Dissertações") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"), # Escolha de cores sóbrias
                    labels = c("Ensino Superior", "Desigualdade", "Juventude", "Outros")) + # Rótulos da legenda
  theme_minimal() +
  theme(legend.position = "bottom") # Posição da legenda
