
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
# Guatemala e Nicaragua não tem valores de 2015 até 2020
# Espanha também não tem dados em 2017


gini_mod <- gini_mod |>  
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
  select(country,gini_disp, pais, ano) |> 
  na.omit() # todos os valores que tiverem NA, retirar da tabela



#### Importando dados ----------------------------------------------------------

### Baixando dados só dos anos que tem a pergunta que será analisada: 2015 - 2016 - 2017 - 2018

latino15 <- import("Latinobarometro_2015.dta")
latino16 <- import("Latinobarometro_2016.dta")
latino17 <- import("Latinobarometro_2017.sav")
latino18 <- import("Latinobarometro_2018.dta")


# 2015

lat15 <- latino15 |> 
  select(numinves, 
         pais = idenpa, 
         justo_renda = P18ST) |> 
  mutate(ano = case_when( 
    numinves == 18 ~ 2015)) |> 
  select(-numinves)

# 2016

lat16 <- latino16 |> 
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = P21ST)

# 2017

lat17 <- latino17 |> 
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P20ST)

# 2018

lat18 <- latino18 |> # Criando novo objeto
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P23ST)



### Juntando em uma tabela só (lat_bin) ----------------------------------------

lat_bin <- rbind(lat18, 
                 lat17,
                 lat16,
                 lat15)%>%
  mutate(justo_renda = case_when(
    justo_renda < 1 ~ NA_real_,# retirar todos os valores que não forem de 1 a 4 em NA
    TRUE ~ justo_renda)) %>% 
  na.omit() # todos os valores que tiverem NA, retirar da tabela
  

# Como as respostas de cada ano se dispersam? 
lat_bin %>% 
  tabyl(justo_renda, ano)

### Legenda: 
# 1 - Muito justo
# 2 - Justo
# 3 - Injusto
# 4 - Muito injusto


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


ggplot(lat_gini, aes(
  x = pais, 
  y = gini,
  fill = as.factor(justo_renda))) +
  geom_bar(
    stat = "identity", 
    position = "dodge"
  )+
  facet_grid(
    ano ~ pais, 
    scales = "free", 
    space = "free") + 
  labs(
    x = "País", 
    y = "Percepção", 
    fill = "Distribuição") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()) 

 
  