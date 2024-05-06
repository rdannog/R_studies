library(tidyverse)
library(janitor)
library(dplyr)
library(rio)
library(ggplot2) 


### Importando dados ----
#Obs: prestar atenção no TIPO do arquivo

latino18 <- import("C:/Users/paula/Desktop/NIED/Pesquisa 2024/F00008548-Latinobarometro_2018_Esp_R_v20190303.zip")
latino20 <- import("C:/Users/paula/Desktop/NIED/Pesquisa 2024/F00011657-Latinobarometro_2020_Esp_R_Rdata_v1_0.zip")
idh <- import("C:/Users/NIED/Desktop/paises.xlsx")
gini <- import("C:/Users/NIED/Downloads/swiid9_6.rda")
load("swiid9_6.rda")
latino18 <- import("C:/Users/NIED/Downloads/lat18.rds")
latino20 <- import("C:/Users/NIED/Downloads/lat20.rdata")

### Criando um novo objeto para cada base original ----

latino18 <- latino18
latino18_novo <- latino18

latino20 <- latino20
latino20_novo <- latino20

### Criando novas colunas "ano" para cada objeto novo ----

latino20_novo <- latino20 %>%
  mutate(ano = numinves)%>%
  mutate(pais = idenpa)%>%
  mutate(distribuicao = p19st.a)

glimpse(latino20_novo)

latino18_novo <- latino18 %>%
  mutate(ano = NUMINVES)%>%
  mutate(pais = IDENPA)%>%
  mutate(distribuicao = P23ST)

glimpse(latino18_novo)

### Convertendo variáveis para padronizar a categoria dos dois anos: fazendo isso com "percep" e "ano" para que ambas virem dbl ----

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


### Selecionando coluna ----

LAT20BIND <- latino20_novo %>%
            select(ano, pais, distribuicao)

LAT18BIND <- latino18_novo %>%
  select(ano, pais, distribuicao)


### Juntando com RBIND ----

BIN1820 <- bind_rows(LAT20BIND, LAT18BIND)

BIN1820 <- BIN1820 %>%
  mutate(distribuicao = case_when(
    distribuicao < 1 ~ NA_real_,
    TRUE ~ distribuicao
  ))

tabyl(BIN1820$distribuicao)


### Criando siglas dos países ----
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



### Criando variável chave (para o join) na base original ----

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


### Criando novo objeto com seleção de variaveis da base original ----

swiid_join <- swiid_summary %>% 
  select(country, year, gini_disp, pais)

### Filtrando anos na swiid_join ----

swiid_join <- swiid_join %>% 
  filter(year == 2018 | year == 2020)

### Juntando BIN1820 e swiid_join para obter os ginis ----

BIN1820_join <- left_join(BIN1820, swiid_join, by = c("pais", "ano" = "year"))

### Adicionando os valores de gini e o nome dos paises Guatemala e Nicaragua ----

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


### Somando os resultados de cada resposta ----

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


### Entendendo o percentual de cada resposta por país e por ano ----
BIN1820_join <- BIN1820_join %>% 
  group_by(pais, ano, distribuicao) %>% 
  mutate(pct_ano = (contagem/tot_p_ano)*100)

tabyl(BIN1820_join$contagem)


### TESTES QUE FALHARAM, NÃO É PARA UTILIZAR ----

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
  

### Plotando o objeto "resultados" ----

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


# PLOT QUE FUNCIONOU!!!

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
