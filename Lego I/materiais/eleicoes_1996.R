# ---
# Neste exemplo, carregaremos alguns dados que indicam o partido que venceu as
# eleicoes municipais de 1996 em cada municipio. Essa base, no entanto, nao
# contem uma variavel de regiao para nos permitir agrupar os dados e descobrir
# quantas prefetuiras o PT, por exemplo, venceu nesse pleito. Precisaremos,
# portanto, usar join para adicionar essa informacao.
# ---


# Vamos comecar pelos pacotes
library(tidyverse)


# E carregamos os dados diretamente da internet com load (usamos URL para
# fazer a conexao, o que a funcao load nao faz nativamente)
load(url("https://github.com/meirelesff/elections96/raw/master/data/elections96.Rda"))


# Podemos visualizar nossa base:
View(elections96)


# Ha' poucas variaveis nela e, em particular, e' evidente a falta de uma coluna
# indicando a regiao do pais em que cada municipio esta'. Claro, poderiamos
# criar uma usando case_when, atribuindo cada estado a uma regiao. Mas podemos
# usar _join para pegar esses dados de outra tabela. Aqui temos uma:
estados <- read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/estados.csv") %>%
  mutate(uf = ifelse(uf == "RS", "X", uf))
View(estados)


# Agora podemos usar left_join para adicionara informacao que faltava na nossa
# base de eleicoes.

# - Na base elections96, o nome da variavel com a sigla de estado e' 'state'
# - Na base estados, o nome da variavel equivalente e' 'uf'
elections96 <- elections96 %>%
  left_join(estados, by = c("state" = "uf"))


# Pronto, com isso conseguimos saber quantas prefeituras o PT obteve em cada regiao,
# ou a media de votos de suas candidaturas eleitas por regiao:
elections96 %>%
  filter(party == "PT") %>% # Ficamos apenas com prefeituras do PT
  group_by(regiao) %>%
  summarise(prefeituras = n()) # n() serve para contar quantas linhas ha'




