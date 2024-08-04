
library(tidyverse)
library(janitor)
library(dplyr)
library(rio)
library(ggplot2) 


## Baixando tabela com ginis dos países 
load("swiid9_6.rda") 

## Transformando em objeto o que era lista
gini <- swiid_summary 

## Copiando objeto original para o que será modificado
gini_mod <- gini

### Modificando gini_mod 
# Guatemala e Nicaragua não tem valores de 2015 até 2020, o restante tem valores em tudo
# Por isso os dois paises foram tirados 
# Espanha também não tem dados em 2017


gini_mod <- gini_mod |> # mesmo nome 
  mutate(pais = # vou juntar essa variável com a "pais" do outro objeto
           case_when( # acrescentando mais uma variável (país) com os números correspondentes
    country == "Argentina" ~ 32,
    country == "Bolivia" ~ 68,
    country == "Brazil" ~ 76,
    country == "Chile" ~ 152,
    country == "Colombia" ~ 170,
    country == "Costa Rica" ~ 188,
    country == "Dominican Republic" ~ 214,
    country == "Ecuador" ~ 218,
    country == "El Salvador" ~ 222, # country == "Guatemala" ~ 320,
    country == "Honduras" ~ 340,
    country == "Mexico" ~ 484, # country == "Nicaragua" ~ 558,
    country == "Panama" ~ 591,
    country == "Paraguay" ~ 600,
    country == "Peru" ~ 604, # country == "Espanha" ~ 724,
    country == "Uruguay" ~ 858,
    country == "Venezuela" ~ 862), 
    ano = case_when( # adicionando variável "ano" só com os anos que tem as perguntas que precisamos
      year == 2018 ~ 2018,
      year == 2017 ~ 2017,
      year == 2016 ~ 2016,
      year == 2015 ~ 2015,
      TRUE ~ NA_real_ # Use NA_real_ para garantir que a coluna seja numérica
    )) |> 
  select(country, ano, gini_disp, pais) |> # Manter só paises, anos, gini
  na.omit() # todos os valores que tiverem NA, retirar da tabela



#### Importando dados ----------------------------------------------------------

### Baixando dados só dos anos que tem a pergunta que será analisada
## Os nomes dos arquivos estão padronizados, extrair do zip e trocar nome de endereço do dado

# Se quiserem só baixar os anos usados, usar: 2015 - 2016 - 2017 - 2018

latino15 <- import("Latinobarometro_2015.dta")
latino16 <- import("Latinobarometro_2016.dta")
latino17 <- import("Latinobarometro_2017.sav")
latino18 <- import("Latinobarometro_2018.dta")


# Os que vou usar (2015 - 2018) ----

# 2015

lat15 <- latino15 |> # Criando novo objeto
  select(numinves, 
         pais = idenpa, 
         justo_renda = P18ST) |> 
  mutate(ano = case_when( 
    numinves == 18 ~ 2015)) |> 
  select(-numinves)

# 2016

lat16 <- latino16 |> # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = P21ST)

# 2017

lat17 <- latino17 |> # Criando novo objeto
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P20ST)

# 2018

lat18 <- latino18 |> # Criando novo objeto
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P23ST)


# Usados 
glimpse(lat15)
glimpse(lat16)
glimpse(lat17)
glimpse(lat18)


# 2015

lat15 %>% 
  tabyl(pais)
# países mais regulares 
lat15 %>% 
  tabyl(pais, justo_renda)

# 2016

lat16 %>% 
  tabyl(pais)
# países mais regulares
lat16 %>% 
  tabyl(pais, justo_renda)

# 2017

lat17 %>% 
  tabyl(pais)
# países mais regulares
lat17 %>% 
  tabyl(pais, justo_renda)


# 2018

lat18 %>% 
  tabyl(pais)
# países mais regulares
lat18 %>% 
  tabyl(pais, justo_renda)

### Sugestão: 
# Usar dados de lat15 até lat_18, que são os anos que tem o número de respostas por
# paises mais regulares
# 1. Vai dar menos trabalho
# 2. As comparações vão ser menos enviesadas

### Juntando em uma tabela só (lat_bin) ----------------------------------------

lat_bin <- rbind(lat18, 
                 lat17,
                 lat16,
                 lat15)%>%
  mutate(justo_renda = case_when(
    justo_renda < 1 ~ NA_real_, # retirar todos os valores que não forem de 1 a 4 em NA
    TRUE ~ justo_renda)) %>% 
  na.omit() # todos os valores que tiverem NA, retirar da tabela
  
## Conferindo tabela

colnames(lat_bin)
colnames(gini_mod)
# Ano e pais são correspondentes em ambas tabelas

# Quais são os tipos das variáveis de gini_mod?
glimpse(gini_mod)
# Exceto por country (que é chr), o restante é dbl

# Quais são os tipos das variáveis de lat_bin?
glimpse(lat_bin)
# Todos dbl, ou seja, correspondente com o gini_mod

# Os 4 anos escolhidos (2015 - 2018) tem valores próximos de país a país?
lat_bin %>% 
  tabyl(ano, pais)
# parece que sim

# Como as respostas de cada ano se dispersam? 
# Não pode ter valores NA e nem menores que 1
lat_bin %>% 
  tabyl(justo_renda, ano)

### Legenda: 
# 1 - Muito justo
# 2 - Justo
# 3 - Injusto
# 4 - Muito injusto

# Como se mostram as tabelas?
lat_bin %>% 
  head(20)

gini_mod %>% 
  head(20)

#### Juntando gini_mod e lat_bin = lat_gini ------------------------------------

lat_gini <- left_join(lat_bin, gini_mod, by = c("pais", "ano")) %>% 
  select(ano, 
         iso = pais, # troquei nome para não confundir pais com o código
         pais = country, # troquei nome pra deixar tudo em portugues
         gini = gini_disp, # já que só usaremos esse gini, não precisa de um nome tão grande
         justo_renda) %>% 
  na.omit() %>% # para tirar todos os NA que são os da Guatemala e Nicaraguá
  arrange(ano) %>% 
  group_by(ano, iso, justo_renda) %>% 
  mutate(contagem = n()) %>% 
  ungroup()
  
# Conferindo 
sum(is.na(lat_gini)) # Não tem mais nenhum NA

glimpse(lat_gini) 

lat_gini %>% 
  count(iso, pais) %>% 
  mutate(Porcentagem = n / sum(n) * 100) %>% 
  arrange(Porcentagem)
# todos os valores estão entre 5.3% até 6.7% das respostas

lat_gini %>% 
  count(ano) %>% 
  mutate(Porcentagem = n / sum(n) * 100) %>% 
  arrange(Porcentagem)
# Todos os valores estão entre 24.82% e 25.13% 

lat_gini %>% 
  count(iso, gini, ano) 

head(lat_gini, 50)
count(lat_gini, pais)

count(iso, gini) 

lat_gini %>% 
  count(contagem, justo_renda) %>% 
  print(n = 260)

lat_gini %>% 
  tabyl(justo_renda, iso)
  
head(lat_gini, 30)


###############################################

### Inutil, mas não quis apagar porque vai que ---------
### Convertendo variáveis para padronizar a categoria dos dois anos: fazendo isso com "percep" e "ano" para que ambas virem dbl

# Convertendo a de 2018 -> de dbl-lbl para double utilizando as.numeric

LAT18BIND$distribuicao<- as.numeric(LAT18BIND$distribuicao)

class(LAT18BIND$distribuicao)

LAT18BIND$ano<- as.numeric(LAT18BIND$ano)

class(LAT18BIND$ano)

LAT18BIND$pais<- as.numeric(LAT18BIND$pais)

class(LAT18BIND$pais)

# Convertendo a de  2020 -> de integer para double utilizando as.numeric

LAT20BIND$distribuicao<- as.numeric(LAT20BIND$distribuicao)

class(LAT20BIND$distribuicao)

LAT20BIND$ano<- as.numeric(LAT20BIND$ano)

class(LAT20BIND$ano)

LAT20BIND$pais<- as.numeric(LAT20BIND$pais)

class(LAT20BIND$pais)


### Selecionando coluna

LAT20BIND <- latino20_novo %>%
            select(ano, pais, distribuicao)

LAT18BIND <- latino18_novo %>%
  select(ano, pais, distribuicao)


### Juntando com RBIND 

BIN1820 <- bind_rows(LAT20BIND, LAT18BIND)

BIN1820 <- BIN1820 %>%
  mutate(distribuicao = case_when(
    distribuicao < 1 ~ NA_real_,
    TRUE ~ distribuicao
  ))

tabyl(BIN1820$distribuicao)


### Criando siglas dos países
BIN1820 <- BIN1820 %>%
  mutate(iso = case_when(
    pais == 32 ~ "ARG",
    pais == 68 ~ "BOL",
    pais == 76 ~ "BRA",
    pais == 152 ~ "CHL",
    pais == 170 ~ "COL",
    pais == 188 ~ "CRI",
    pais == 214 ~ "DOM",
    pais == 218 ~ "ECU",
    pais == 222 ~ "SLV",
    pais == 320 ~ "GTM",
    pais == 340 ~ "HND",
    pais == 484 ~ "MEX",
    pais == 558 ~ "NIC",
    pais == 591 ~ "PAN",
    pais == 600 ~ "PRY",
    pais == 604 ~ "PER",
    pais == 724 ~ "ESP",
    pais == 858 ~ "URY",
    pais == 862 ~ "VEN"
  ))

tabyl(BIN1820$iso)



### Criando variável chave (para o join) na base original

swiid_summary <- swiid_summary %>%
mutate(pais = case_when(
  country == "Argentina" ~ 32,
  country == "Bolivia" ~ 68,
  country == "Brazil" ~ 76,
  country == "Chile" ~ 152,
  country == "Colombia" ~ 170,
  country == "Costa Rica" ~ 188,
  country == "Dominican Republic" ~ 214,
  country == "Ecuador" ~ 218,
  country == "El Salvador" ~ 222,
  country == "Guatemala" ~ 320,
  country == "Honduras" ~ 340,
  country == "Mexico" ~ 484,
  country == "Nicaragua" ~ 558,
  country == "Panama" ~ 591,
  country == "Paraguay" ~ 600,
  country == "Peru" ~ 604,
  country == "Espanha" ~ 724,
  country == "Uruguay" ~ 858,
  country == "Venezuela" ~ 862))

count(BIN1820, iso)


### Criando novo objeto com seleção de variaveis da base original

swiid_join <- swiid_summary %>% 
  select(country, year, gini_disp, pais)

### Filtrando anos na swiid_join

swiid_join <- swiid_join %>% 
  filter(year == 2018 | year == 2020)

### Juntando BIN1820 e swiid_join para obter os ginis

BIN1820_join <- left_join(BIN1820, swiid_join, by = c("pais", "ano" = "year"))

### Adicionando os valores de gini e o nome dos paises Guatemala e Nicaragua

BIN1820_join <- BIN1820_join %>%
  mutate(gini_disp = case_when(
    pais == 558 ~ 43.8,
    pais == 320 ~ 45.2,
    TRUE ~ gini_disp
  ))

BIN1820_join <- BIN1820_join %>%
  mutate(country = case_when(
    pais == 558 ~ "Nicaragua",
    pais == 320 ~ "Guatemala",
    TRUE ~ country
  ))


### Como referência: somando os resultados de cada resposta ----

BIN1820_join <- BIN1820_join %>% 
  mutate(Hum = 1) %>% 
  group_by(pais, ano, distribuicao) %>% 
  mutate(contagem = sum(Hum)) %>% 
  ungroup() %>% 
  group_by(ano, pais) %>% 
  mutate(tot_p_ano = sum(Hum)) # ungroup serve para poder desagrupar as variáveis e assim somar a quantidade total de respondentes por países em cada ano

# Outra maneira de somar as respostas
BIN1820_join <- BIN1820_join %>% 
  group_by(pais, ano, distribuicao) %>% 
mutate(contagem = n())


### Entendendo o percentual de cada resposta por país e por ano
BIN1820_join <- BIN1820_join %>% 
  group_by(pais, ano, distribuicao) %>% 
  mutate(pct_ano = (contagem/tot_p_ano)*100)

tabyl(BIN1820_join$contagem)


### TESTES QUE FALHARAM, NÃO É PARA UTILIZAR

resultados <- BIN1820_join %>% 
  unique() %>% 
  pivot_longer(
    names_from = distribuicao,
    values_from = pct_ano)



resultados <- BIN1820_join %>% 
  unique() %>% 
  mutate(r1 = case_when(
    distribuicao == 1 ~ pct_ano)) %>% 
  mutate(r2 = case_when(
        distribuicao == 2 ~ pct_ano)) %>% 
  mutate(r3 = case_when(
        distribuicao == 3 ~ pct_ano)) %>% 
  mutate(r4 = case_when(
        distribuicao == 4 ~ pct_ano))
  

### Plotando o objeto "resultados", não funcionou!! ----

# ESSE PLOT NÃO FUNCIONOU, IGNORAR. MANTENDO APENAS PARA REUTILIZAR O CÓDIGO.

ggplot(resultados, aes(
  x = pais, 
  y = pct_ano, 
  fill = as.factor(distribuicao))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ country, scales = "free") + 
  labs(x = "País", y = "Porcentagem", fill = "Distribuição") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # DEU ERRADO


### PLOT QUE FUNCIONOU!!!----

ggplot(resultados, aes(
  x = country, 
  y = pct_ano, 
  fill = as.factor(distribuicao))) +
  geom_bar(
    stat = "identity", 
    position = "dodge") + 
  facet_grid(
    ano ~ country, 
    scales = "free", 
    space = "free") + 
  labs(
    x = "País", 
    y = "Porcentagem", 
    fill = "Distribuição") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()) 



## Respostas em ca