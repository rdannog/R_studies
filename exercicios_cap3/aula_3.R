#Lista de exercícios da aula 3
#Dan Nogueira

# Manipulação de dados

# 3.1.1 - Espalhar e reunir

library(readxl)

homic <- read_excel("homicidios_uf.xls")
View(homic)

# Transformando a base para uma estrutura "tidy"
# Carrega o pacote tidyverse

library(tidyverse)

# Reune as variaveis de ano espalhadas pela base 'homic'
homic2 <- pivot_longer(homic, -c(Sigla, Codigo, Estado), names_to = "Ano", values_to = "Homicidios")

# Verifica as primeiras observacoes do novo banco
View(homic2)

# Reune as variaveis de ano espalhadas pela base 'homic'
homic3 <- pivot_longer(homic, `2000`:`2009`, names_to = "Ano", values_to = "Homicidios")
View(homic3)

# Reune as variaveis de ano espalhadas pela base 'homic'
homic4 <- pivot_longer(homic, -c(Sigla, Codigo, Estado))
View(homic4)

# Espalha as variaveis de ano reunidas pela base 'homic'
homic5 <- pivot_wider(homic2, names_from = Ano, values_from = Homicidios)
head(homic5)


#
#
#
#Operações básicas

load("capitais.Rda")

# Filtra apenas as cinco primeiras observações do banco capitais
slice(capitais, 1:5)

# Filtra apenas a primeira e a quinta observações do banco capitais
slice(capitais, c(1, 5))


# Remove as 10 primeiras observacoes do banco capitais e salva o resultado no objeto 'cap'
cap <- slice(capitais, -c(1:10))
View(cap)

# Filtra observacoes com populacao maior que 2 milhoes habitantes
filter(capitais, populacao > 2000000)

# Filtra observacoes da regiao sul e cria um novo objeto
sul <- filter(capitais, regiao == "Sul")
sul


# Filtra observacoes com populacao maior que 500 mil e menor que 1 milhao
filter(capitais, populacao > 500000 & populacao < 1000000) # ou
filter(capitais, populacao > 500000, populacao < 1000000)


# Filtra as capitais que gastaram mais de R$ 250 milhões em saúde e mais de R$ 300 milhões em educação em 2012
filter(capitais, 
       despesa_saude > 250000000, 
       despesa_educacao > 300000000)

# remover observações com missings
filter(capitais, !is.na(despesa_saude))

# para manter apenas capitais que pertencem às regiões Sul ou Sudeste, podemos usar:

filter(capitais, regiao %in% c("Sul", "Sudeste"))

# Operações de filtragem
filter(capitais, !uf %in% c("RS", "SP", "MG"))
filter(capitais, regiao == "Sul" | regiao == "Sudeste")
filter(capitais, regiao == "Nordeste" & populacao < 1000000)
filter(capitais, !(regiao == "Nordeste" & populacao < 1000000))


#
#
# Selecionar Colunas

# Seleciona apenas as variaveis uf, capital e populacao do banco
cap1 <- select(capitais, uf, capital, populacao)
View(cap1)

# Remove a variavel populacao
cap2 <- select(capitais, -populacao)
head(cap2)

# Mantem apenas a 1a e a 3a colunas
select(capitais, 1, 3)

# Exclui a 1a e a 3a colunas
select(capitais, -1, -3)
select(capitais, -c(1, 3)) # Mesmo resultado

# Mantem as colunas entre uf e despesa_total
select(capitais, uf:despesa_total)

# Mantem as colunas entre uf e populacao e a coluna despesa_saude
select(capitais, uf:populacao, despesa_saude)

# Reordena as colunas do banco capitais
select(capitais, populacao, uf, capital)

# Duplica a variavel populacao
select(capitais, populacao, uf, capital, populacao)

# Inverte a ordem das colunas
select(capitais, 8:1)

# Seleciona apenas variaveis que comecem com 'despesa'
cap1 <- select(capitais, starts_with("despesa"))
head(cap1)

# Seleciona apenas variaveis que contenham 'acao'
cap1 <- select(capitais, contains("acao"))
head(cap1)

# Seleciona apenas variaveis numericas
select(capitais, where(is.numeric))

# Seleciona variaveis numericas e 'capital'
select(capitais, capital, where(is.numeric))


#
#
#
# Criar e modificar variáveis
# Cria a variavel despesa_per_capita
cap1 <- mutate(capitais, despesa_per_capita = despesa_total / populacao)

select(cap1, capital, despesa_total, populacao, despesa_per_capita)
View(cap1)

# Cria tres variaveis de uma so vez
mutate(capitais, despesa_saude_per_capita = despesa_saude / populacao,
       despesa_educacao_per_capita = despesa_educacao / populacao,
       despesa_assistencia_social = despesa_assistencia_social / populacao
)

# Substitui a variavel de populacao
mutate(capitais, populacao = populacao / 1000)

# Cria uma variavel indicando o ano
mutate(capitais, ano = 2012)
