
#### Trabalho LEGO I 
## Percepções sobre desigualdade com índice de Gini

# Baixando library -------------------------------------------------------------

library(tidyverse)
library(janitor)
library(dplyr)
library(rio)
library(ggplot2) 

# Pergunta de pesquisa ---------------------------------------------------------

# How fair is income distribution

# 0 corresponde à completa igualdade de renda (onde todos têm a mesma renda) e 1 corresponde à completa desigualdade (onde uma pessoa tem toda a renda, e as demais nada têm).

# Anos que aparece: 

# 2020 - P19ST.A  # Não é essa a variável 
# 2018 - P23ST
# 2017 - P20ST
# 2016 - P21ST
# 2015 - P18ST
# 2013 - P27ST
# 2011 - P12ST
# 2010 - P12ST
# 2009 - p14st
# 2007 - p17st
# 2002 - p16st
# 2001 - p11st
# 1997 - nsp20

##### Anos que tem o índice de Gini --------------------------------------------

## Baixando tabela com ginis dos países 
load("swiid9_6.rda") # vai retornar uma lista 

## Transformando em objeto o que era lista
gini <- swiid_summary # transforma em tabela

## Copiando objeto original para o que será modificado
gini_mod <- gini

## Trocar o tipo da variável year de double para numeric
gini_mod$year <- as.numeric(gini_mod$year)

# Conferindo se trocou mesmo (tem que ser numeric!)
class(gini_mod$year)

### Modificando gini_mod 
## Já foi tirado Guatemala e Nicaragua porque não tem gini (escolha arbitrária)

gini_mod <- gini_mod %>% # mesmo nome 
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
    country == "Peru" ~ 604,
    country == "Espanha" ~ 724,
    country == "Uruguay" ~ 858,
    country == "Venezuela" ~ 862), 
    ano = case_when( # adicionando variável "ano" só com os anos que tem as perguntas que precisamos
      year == 2020 ~ 2020,
      year == 2018 ~ 2018,
      year == 2017 ~ 2017,
      year == 2016 ~ 2016,
      year == 2015 ~ 2015,
      year == 2013 ~ 2013,
      year == 2011 ~ 2011,
      year == 2010 ~ 2010,
      year == 2009 ~ 2009,
      year == 2007 ~ 2007,
      year == 2002 ~ 2002,
      year == 2001 ~ 2001,
      year == 1997 ~ 1997,
      TRUE ~ NA_real_ # Use NA_real_ para garantir que a coluna seja numérica
    )) %>% 
  select(country, year, gini_disp, pais, ano) %>% # Manter só paises, anos, gini
  na.omit() # todos os valores que tiverem NA, retirar da tabela

# Conferindo

# 1
# conferindo se não tem nenhum NA em outra variável 
# Não é pra ter nenhum NA 
sum(is.na(gini_mod))  

# 2
gini_mod %>% 
  tabyl(country, ano)
# Guatemala e Nicaragua não tem valores de 2015 até 2020, o restante tem valores em tudo
# Por isso os dois paises foram tirados 

# 3
gini_mod %>% 
  tabyl(ano, year) # conferindo se os anos batem nas duas variáveis
# Reparar que os anos de 2015 a 2020 tem só 16 e não 18 por causa da Nicaragua e Guatemala

# Como tabela de gini_mod se mostra 
head(gini_mod, 20)

# Nomes das colunas de Gini original e gini modificado
colnames(gini)
colnames(gini_mod)

#### Importando dados ----------------------------------------------------------

### Baixando dados só dos anos que tem a pergunta que será analisada
## Os nomes dos arquivos estão padronizados, extrair do zip e trocar nome de endereço do dado

# Se quiserem só baixar os anos usados, usar: 2015 - 2016 - 2017 - 2018
# Não usei os demais, apesar de ter como baixá-los também


## Arquivos baixados em sav

# Não precisa baixar esses: 
latino97 <- import("Latinobarometro_1997.sav")
latino01 <- import("Latinobarometro_2001.sav")
latino07 <- import("Latinobarometro_2007.sav")
latino09 <- import("Latinobarometro_2009.sav")
latino11 <- import("Latinobarometro_2011.sav")

# Baixar esse:
latino17 <- import("Latinobarometro_2017.sav")

## Arquivos baixados em dta

# Não precisa baixar esses: 
latino02 <- import("Latinobarometro_2002.dta")
latino10 <- import("Latinobarometro_2010.dta")
latino13 <- import("Latinobarometro_2013.dta")
latino20 <- import("Latinobarometro_2020.dta")

# Baixar esses: 
latino15 <- import("Latinobarometro_2015.dta")
latino16 <- import("Latinobarometro_2016.dta")
latino18 <- import("Latinobarometro_2018.dta")

# Obs: prestar atenção no TIPO do arquivo
# Obs. 2: com o import dá pra baixar todos os arquivos sem problemas

#### Arrumando dados latinobarômetro -------------------------------------------

### Variáveis para se usar:
## - idenpa = pais
## - numinves = ano
## - código (especifico de cada ano) para a pergunta usada

### Como objetos estão organizados:
# Trocando nome para não ter que baixar de novo se quisermos modificar
## latibo_ano = baixado 
## lat_ano = modificado

# Os que não vou usar (1997 - 2013) ----
# 1997

colnames(latino97)

latino97 %>% 
  select(numinves, idenpa, numentre, reg, codigo) %>% 
  count(numinves)

lat97 <- latino97 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = nsp20)

head(lat97, 20)
lat97 %>% 
  tabyl(justo_renda, pais)

# 2001

colnames(latino01)

latino01 %>% 
  select(numinves, idenpa, numentre, reg, codigo) %>% 
  count(numinves) 

lat01 <- latino01 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = p11st)

# 2002

colnames(latino02)

latino02 %>% 
  select(numinves, idenpa, numentre, reg, codigo) %>% 
  count(idenpa)

lat02 <- latino02 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = p16st)

# 2007

colnames(latino07)

lat07 <- latino07 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = p17st)

# 2009

colnames(latino09)

lat09 <- latino09 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = p14st)

# 2010

colnames(latino10)

lat10 <- latino10 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = P12ST)

# 2011

colnames(latino11)

lat11 <- latino11 %>% # Criando novo objeto
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P12ST)

# 2013

colnames(latino13)

lat13 <- latino13 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = P27ST)

# Os que vou usar (2015 - 2018) ----

# 2015

lat15 <- latino15 %>% # Criando novo objeto
  select(numinves, 
         pais = idenpa, 
         justo_renda = P18ST) %>% 
  mutate(ano = case_when( 
    numinves == 18 ~ 2015)) %>% 
  select(-numinves)

# 2016

colnames(latino16)

lat16 <- latino16 %>% # Criando novo objeto
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = P21ST)

# 2017

colnames(latino17)

lat17 <- latino17 %>% # Criando novo objeto
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P20ST)

# 2018

colnames(latino18)

lat18 <- latino18 %>% # Criando novo objeto
  select(ano = NUMINVES, 
         pais = IDENPA, 
         justo_renda = P23ST)

# 2020 - ERRO: NÃO ACHEI A VARIÁVEL

colnames(latino20)
latino20 %>% 
  select(Q19ST.A)

lat20 <- latino20 %>% # Não consegui criar novo objeto porque variável não está certa
  select(ano = numinves, 
         pais = idenpa, 
         justo_renda = P19ST.A)


### Conferindo objetos criados ------------------------------------------------- 

# Não usados 
glimpse(lat97)
glimpse(lat01)
glimpse(lat02)
glimpse(lat07)
glimpse(lat09)
glimpse(lat10)
glimpse(lat11)
glimpse(lat13)

# Usados 
glimpse(lat15)
glimpse(lat16)
glimpse(lat17)
glimpse(lat18)

# Todas as variáveis são double! Não é preciso mudar o tipo da variável! 

## Entendendo se tem todos os anos

# 1997

lat97 %>% 
  tabyl(pais)
# país 600 tem 575 observações 
# país 724 tem 2476 observações
# país 68 tem 796 observações
lat97 %>% 
  tabyl(pais, justo_renda)

# 2001

lat01 %>% 
  tabyl(pais)
# atenção ao país 600 - só tem 604 observações
# País 724 tem só 2496
lat01 %>% 
  tabyl(pais, justo_renda)

# 2002

lat02 %>% 
  tabyl(pais)
# país 600 tem 600 observações 
# país 724 tem 2484 observações 
lat02 %>% 
  tabyl(pais, justo_renda)
# Não tem respostas do ano 724 (todas as respostas estão no -4)
# NÃO DÁ PRA USAR

# 2007

lat07 %>% 
  tabyl(pais)
# País 600 aqui tem 1200 observações
# país 724 tem 2482 observações 
lat07 %>% 
  tabyl(pais, justo_renda)

# 2009

lat09 %>% 
  tabyl(pais)
# País 724 tem 2486 observações
lat09 %>% 
  tabyl(pais, justo_renda)

# 2010

lat10 %>% 
  tabyl(pais)
# País 724 tem 2483 observações
lat10 %>% 
  tabyl(pais, justo_renda)

# 2011

lat11 %>% 
  tabyl(pais)
# País com anos mais regulares 
lat11 %>% 
  tabyl(pais, justo_renda)

# 2013

lat13 %>% 
  tabyl(pais)
# Pais 724 tem 2459 observações 
lat13 %>% 
  tabyl(pais, justo_renda)

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