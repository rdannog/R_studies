###############################
#  Lista II: Análise de dados #
#    Dan Nogueira da Silva    #
###############################

### ---------------------------------------------

### Nesta lista, o objetivo será analisar a produção de teses e dissertações de programas de pós-graduação com notas 4, 5, 6 e 7 na CAPES, da área de Sociologia, entre os anos de 1987 e 2022.

### ---------------------------------------------


### Carregando pacotes necessários ###

library(tidyverse)
library(rio)
library(sf)
library(geobr)
library(gt)
library(plotly)

### 1. Importando arquivos ###

banco_defesas <- map_df(c("csvs/capes_1987-1992.csv", "csvs/capes_1993-1998.csv", "csvs/capes_1999-2004.csv", "csvs/capes_2005-2010.csv", "csvs/capes_2011-2016.csv", "csvs/capes_2017-2022.csv"),read_delim, delim = ",")

banco_programas <- import("programas.csv") 


# Para importar a base de dados contendo as dissertações e teses de programas na CAPES, optei por utilizar o map_df pois nesse caso achei mais prático do que usar o loop, por economizar linhas e espaço de memória
# Usando map_df é possível combinar todas as planilhas contendo as teses e dissertações defendidas entre 1987 a 2022, em um único tibble. Essa planilha possui 13 variáveis: código do programa,ano, sigla,nome da instituição, nome do programa, grande área, área de conhecimento, área de avaliação,autor, titulo, nivel, palavas-chave e resumo. Cada observação diz respeito a uma defesa.

# Para carregar a planilha com informações sobre os programas de pós-graduação, usei a função import() do pacote rio.Essa planilha possui 3 variáveis: código do programa, estado e conceito CAPES. Cada observação diz respeito a um programa de pós-graduação.


teses_e_programas <- banco_defesas |>
  left_join(banco_programas, by = c("codigo_programa" = "CD_PROGRAMA"))

# Para concatenar as informações sobre os programas com as informações sobre as defesas, utilizei a função left_join para juntar as informações partindo de uma variável em comum, o código do programa.


banco_tidy <- teses_e_programas |>
  mutate(UF = case_when( 
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

# Ao analisar a nova base, percebi alguns missings na variável UF. Tentei arrumar a lista me guiando a partir das colunas siglas_ies e UF. Além disso, incluí a variável regiao, que vai ser útil mais à frente.


teses_sociologia <- banco_tidy |>
  filter(CONCEITO != "NA|3|A") |>
  filter(str_detect(nome_programa, "SOCIOLOGIA"))

# Após limpar a lista, filtrei apenas observações que continham programas com notas maiores que 4 sobre minha área de interesse (sociologia) 
# Por algum motivo os missings continuavam a ser considerados quando eu utilizava a lógica CONCEITO == 4|5|6|7, então achei melhor filtrar usando a negação das observações indesejadas.


### 2. Seleção de palavras-chave ###

teses_relevantes <- teses_sociologia |>
 filter(str_detect(palavras_chave,"ensino superior|desigualdade|educação"))

# Para o meu desenho de pesquisa, as 3 palavras-chave mais interessantes são ensino superior, desigualdade e educação. Filtrei a base para mostrar apenas defesas que se enquadravam em pelo menos uma das 3 palavras-chave.



### 3. Evolução ao longo do tempo ###

teses_por_ano_subtema <- teses_relevantes |>
 mutate(subtema = case_when(
  str_detect(palavras_chave, "ensino superior") ~ "Ensino Superior",
  str_detect(palavras_chave, "desigualdade") ~ "Desigualdade",
  str_detect(palavras_chave, "educação") ~ "Educação",
  TRUE ~ "Outros"
 )) |>
 count(ano, subtema) |>
 rename(frequencia = n) 

grafico_defesas_ano_subtema <- plot_ly(teses_por_ano_subtema, x = ~ano, y = ~frequencia, type = 'bar', name = ~subtema)
grafico_defesas_ano_subtema <- grafico_defesas_ano_subtema |> 
  layout(title = 'Produção de Teses e Dissertações em Sociologia, por Palavra-Chave (1987-2022)', yaxis = list(title = 'Número de teses e dissertações'), xaxis = list(title = 'Ano da defesa'), barmode = 'stack')

grafico_defesas_ano_subtema


  


# Para criar uma visualização que reporte de forma sucinta e informativa a produção de teses e dissertações no meu tema por ano, primeiro era preciso criar uma tabela de contagem das ocorrências de defesas por ano e palavra-chave. Criei a variável subtema, que classificava as defesas  por palavra-chave correspondente. Depois contei quantas ocorrências cada variável ano tinha em relação a cada observação da variável subtema.

# Para visualizar, escolhi o gráfico de barras empilhadas para ver quantas observações foram feitas para cada ano do eixo x, qualificando a frequência por palavras-chave. Assim consigo ver quantas teses foram defendidas em cada ano, ao mesmo tempo em que consigo ver a frequência de cada subtema que considerei relevante para minha pesquisa.O gráfico em barras empilhadas é perfeito para visualizar a relação entre uma variável numérica e uma variável categórica.

# Optei por utilizar o pacote plotly para tornar o gráfico interativo.


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

teses_por_estado |>
  ggplot(aes(fill = total_trabalhos_uf, label =  paste(UF, " (", total_trabalhos_uf, ")", sep = "")))+
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

## A tabela final deve reportar 4 colunas: nome do programa; nota na Capes; total de dissertações; e total de teses defendidas no programa. Apresente o resultado da tabela ordenando os programas pelo total de teses defendidas, do maior para o menor. O resultado precisa ser uma tabela, e não output de console.


trabalhos_por_programa <- teses_sociologia |>
  mutate(tese_ou_defesa = case_when(
    str_detect(nivel, "Mestrado|MESTRADO|MESTRADO PROFISSIONAL") ~ "dissertacao",
    str_detect(nivel, "Doutorado|DOUTORADO") ~ "tese",
  )) |>
  count(sigla_ies, nome_programa, tese_ou_defesa, CONCEITO) |>
  rename(trabalhos = n) 

trabalhos_por_programa <- pivot_wider(trabalhos_por_programa, names_from = tese_ou_defesa, values_from = trabalhos) |>
  mutate(tese = case_when(
    tese > 0  ~ tese,
    TRUE ~ 0
  )) |>
  mutate(total_defesas = dissertacao + tese) |>
  arrange(-total_defesas)|>
  slice(1:10)|>
  select(-total_defesas)


ggplot(trabalhos_por_programa, aes(x =reorder(sigla_ies, -(tese+dissertacao)), y=  tese + dissertacao, fill = nome_programa)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "   Produção de Teses e Dissertações em Sociologia (1987-2022)",
       x = "
            Sigla da instituição de ensino superior", y = "Número de defesas
        ") +
  theme_classic() +
  theme(panel.grid.minor = element_blank())+
  scale_fill_viridis_d(name = "Nome do programa")

# -----------------------------------------------------------------------------------------

### 6. Exportação

## Crie uma base menor que contenha apenas as seguintes variáveis: ano, estado, programa, título,resumo e autor(a). Exporte essa base para uma planilha de Excel.

resumo <- teses_relevantes |>
  select(ano, UF, nome_programa, titulo, resumo, autor)

# write_csv(resumo, "resumo.csv")
