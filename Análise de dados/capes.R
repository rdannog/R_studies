###############################
#  Lista II: Análise de dados #
#                             #
#   Dan Nogueira da Silva     #
###############################

### ---------------------------------------------

### Nesta lista, o objetivo será analisar a produção de teses e dissertações de programas de pós-graduação notas 4, 5, 6 e 7 na Capes, das áreas de Sociologia ou Ciência Política e Relações Internacionais, entre os anos de 1987 e 2022.

### ---------------------------------------------


### Carregando pacotes necessários

library(tidyverse)
library(rio)
library(sf)
library(geobr)
library(gt)

### 1. Importando arquivos ###

# Usando map_df é possível combinar todas as planilhas contendo as teses e dissertações defendidas entre 1987 a 2022, em um único tibble

# Optei por utilizar o map_df pois nesse caso achei mais prático do que usar o loop, por economizar linhas e espaço de memória

banco_teses_total <- map_df(c("csvs/capes_1987-1992.csv", "csvs/capes_1993-1998.csv", "csvs/capes_1999-2004.csv", "csvs/capes_2005-2010.csv", "csvs/capes_2011-2016.csv", "csvs/capes_2017-2022.csv"),read_delim, delim = ",")

# Carregando a planilha com informações sobre os programas de pós-graduação

banco_programas_total <- import("programas.csv")  

# Juntando informações sobre os programas com informações sobre as teses e dissertações

teses_e_programas <- banco_teses_total |>
  left_join(banco_programas_total, by = c("codigo_programa" = "CD_PROGRAMA"))

# Ao analisar a lista, percebi alguns missings na base e tentei arrumar a lista por siglas, estados e regiao

banco_tidy <- teses_e_programas |>
  mutate(UF = case_when( ##
    str_detect(sigla_ies, "RJ|RIO|UENF|UFF|UCAM") ~ "RJ",
    str_detect(sigla_ies, "SP|UNICAMP") ~ "SP",
    str_detect(sigla_ies, "ES|UVV") ~ "ES",
    str_detect(sigla_ies, "AC") ~ "AC",
    str_detect(sigla_ies, "AL") ~ "AL",
    str_detect(sigla_ies, "AP") ~ "AP",
    str_detect(sigla_ies, "UFAM") ~ "AM",
    str_detect(sigla_ies, "BA") ~ "BA",
    str_detect(sigla_ies, "CE|FJN|UFC") ~ "CE",
    str_detect(sigla_ies, "DF|UNB") ~ "DF",
    str_detect(sigla_ies, "GO|UFG") ~ "GO",
    str_detect(sigla_ies, "MA") ~ "MA",
    str_detect(sigla_ies, "MT") ~ "MT",
    str_detect(sigla_ies, "MS|UFGD") ~ "MS",
    str_detect(sigla_ies, "MG") ~ "MG",
    str_detect(sigla_ies, "PA") ~ "PA",
    str_detect(sigla_ies, "PB|UFCG") ~ "PB",
    str_detect(sigla_ies, "PR|UEL") ~ "PR",
    str_detect(sigla_ies, "PE|UNIVASF") ~ "PE",
    str_detect(sigla_ies, "PI") ~ "PI",
    str_detect(sigla_ies, "RN") ~ "RN",
    str_detect(sigla_ies, "RS|UFRGS") ~ "RS",
    str_detect(sigla_ies, "RO") ~ "RO",
    str_detect(sigla_ies, "RR") ~ "RR",
    str_detect(sigla_ies, "SC") ~ "SC",
    str_detect(sigla_ies, "SE") ~ "SE",
    str_detect(sigla_ies, "TO") ~ "TO",
    TRUE ~ "Outros"
  )) |>
  mutate(regiao = case_when(
    UF %in% c("AM", "RR", "AP", "PA", "TO", "RO", "AC") ~ "Norte",
    UF %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA") ~ "Nordeste",
    UF %in% c("MT", "MS", "GO", "DF") ~ "Centro Oeste",
    UF %in% c("PR", "SC", "RS") ~ "Sul",
    UF %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
    TRUE ~ "Não reportado"
  ))


# Filtrando apenas observações da minha área de interesse, de programas com notas 4, 5, 6, e 7

# Por algum motivo os missings continuavam a ser considerados, então achei melhor filtrar por negação

teses_sociologia <- banco_tidy |>
  filter(CONCEITO != "NA|3|A") |>
  filter(str_detect(nome_programa, "SOCIOLOGIA"))


### 2. Seleção de palavras-chave ###

# Escolha 3 palavras-chave relevantes para o seu problema de pesquisa.

teses_relevantes <- teses_sociologia |>
  filter(str_detect(palavras_chave,"ensino superior|desigualdade|educação"))



### 3. Evolução ao longo do tempo ###

## Crie uma visualização que reporte de forma sucinta e informativa a produção de teses e dissertações no seu tema defendidos por ano.


# Criando uma tabela de contagem de teses por ano e palavra-chave

teses_por_ano_subtema <- teses_relevantes |>
  mutate(subtema = case_when(
    str_detect(palavras_chave, "ensino superior") ~ "Ensino Superior",
    str_detect(palavras_chave, "desigualdade") ~ "Desigualdade",
    str_detect(palavras_chave, "educação") ~ "Educação",
    TRUE ~ "Outros"
  )) |>
  count(ano, subtema) |>
  rename(frequencia = n)

## Criando o gráfico

# Escolhi o gráfico de barras empilhadas para visualizar quantas observações foram feitas para cada ano do eixo x, qualificando a frequência por palavras-chave.Assim consigo ver quantas teses foram defendidas em cada ano, ao mesmo tempo em que consigo ver a frequência de cada subtema que considerei relevante para minha pesquisa.

ggplot(teses_por_ano_subtema, aes(x = ano, y = frequencia, fill = subtema)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Produção de Teses e Dissertações em Sociologia, por Palavra-Chave (1987-2022)",
       x = "
          Ano de Defesa", y = "Número de Teses e Dissertações
       ") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),
                    labels = c("Ensino Superior", "Desigualdade", "Educação", "Outros"),
                    name = "Legenda:") +
  scale_x_continuous(breaks = unique(teses_por_ano_subtema$ano)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


### 4. Diferenças regionais

## Calcule o total de trabalhos defendidos ao longo de todo o período por estado e crie duas visualizações: em uma, reporte a frequência de trabalhos por região; em outra, reporte a frequência de trabalhos por unidade da federação.


# Mapa da frequência de trabalhos por unidade da federação usando geobr

coordenadas_estados <- read_state() # função geobr

#Agrupando teses por estado e juntando com coordenadas do pacote geobr

teses_por_estado <- teses_sociologia |>
  group_by(UF) |>
  summarise(total_trabalhos_uf = n()) |>
  full_join(coordenadas_estados, by = c("UF" = "abbrev_state")) |>
  st_as_sf()

# Mapa da frequência de defesas por estado

  ggplot(teses_por_estado)+
    geom_sf(aes(fill = total_trabalhos_uf))+
    scale_fill_viridis_c(name = "Número de Trabalhos") +
    theme_void() +
    labs(title = "Frequência de Trabalhos de Sociologia Defendidos por Unidade da Federação",
         subtitle = "1987 - 2022",
         caption = "FONTE: CAPES") #usei a gambiarra sugerida pelo Rodrigo
  
#Agrupando teses por regiao e juntando com coordenadas do pacote geobr

  teses_por_regiao <- teses_sociologia |>
    group_by(regiao) |>
    summarise(total_trabalhos_regiao = n()) |>
    full_join(coordenadas_estados, by = c("regiao" = "name_region")) |>
    st_as_sf()
  
# Mapa da frequência de trabalhos por região

  
  ggplot(teses_por_regiao)+
    geom_sf(aes(fill = total_trabalhos_regiao))+
    scale_fill_viridis_c(name = "Número de Trabalhos") +
    theme_void() +
    labs(title = "Frequência de Trabalhos de Sociologia Defendidos por Região do País",
         subtitle = "1987 - 2022",
         caption = "FONTE: CAPES")

teses_por_regiao |>
  ggplot(aes(fill = total_trabalhos_regiao, label =  paste(abbrev_state, " (", total_trabalhos_regiao, ")", sep = "")))+
  geom_sf()+
  geom_sf_label(fill = "white", size = 1.9, nudge_x = 0.5)+
  scale_fill_viridis_c()+
  ggtitle("Produção de Teses e Dissertações por Região do país.")+
  theme_void()

# -----------------------------------------------------------------------------------
# Gráfico da frequência de trabalhos por unidade da federação

ggplot(teses_por_estado, aes(x = UF, y = total_trabalhos_uf, fill = UF)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequência de Trabalhos Defendidos por Unidade da Federação",
       x = "Unidade da Federação", y = "Total de Trabalhos Defendidos")+
theme_light() +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Gráfico da frequência de trabalhos por região

ggplot(teses_por_estado, aes(x = regiao, y = total_trabalhos, fill = regiao)) +
  geom_sf(stat = "identity") +
  labs(title = "Frequência de Trabalhos Defendidos por Região",
       x = "Região", y = "Total de Trabalhos Defendidos")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # rotacionar ajuda na legibilidade de legendas longas


# -----------------------------------------------------------------------------------------

### 5. Produção por programa

## Calcule o total de teses e de dissertações defendidas por programa de pós-graduação. Feito isso, reporte em uma tabela o número de trabalhos defendidos pelos 10 programas com maior produção.

## A tabela final deve reportar 4 colunas: nome do programa; nota na Capes; total de dissertações; etotal de teses defendidas no programa. Apresente o resultado da tabela ordenando os programas pelo total de teses defendidas, do maior para o menor. O resultado precisa ser uma tabela, e não output de console.


trabalhos_por_programa <- teses_sociologia |>
  mutate(tese_ou_defesa = case_when(
    str_detect(nivel, "Mestrado|MESTRADO|MESTRADO PROFISSIONAL") ~ "Defesa",
    str_detect(nivel, "Doutorado|DOUTORADO") ~ "Tese",
  )) |>
  count(sigla_ies,nome_programa, tese_ou_defesa) |>
  rename(trabalhos = n)

