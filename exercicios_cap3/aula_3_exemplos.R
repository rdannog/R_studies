

# Manipula??o de dados

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

# para manter apenas capitais que pertencem ?s regi?es Sul ou Sudeste, podemos usar:

filter(capitais, regiao %in% c("Sul", "Sudeste"))

# Opera??es de filtragem
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


mutate(capitais, 
       ano = 2012, 
       populacao = populacao / 1000, 
       .keep = "none"
)

# posicionar as novas vari?veis antes do nome de alguma vari?vel
mutate(capitais, ano = 2012, .before = regiao)

# Cria uma variavel que indica municipios com mais de 500 mil habitantes
mutate(capitais, capitais_grandes = if_else(populacao > 500000, "Capital de grande porte", "Capital de menor porte"))

# Cria uma variavel que indica o porte das capitais
porte <- mutate(capitais, porte = case_when(
        populacao < 500000 ~ "Capital de menor porte",
        populacao < 1000000 ~ "Capital de porte intermediario",
        populacao < 2000000 ~ "Capital de grande porte",
        populacao < 5000000 ~ "Capital de grande porte II",
        .default = "Capital de grande porte III"
))

select(porte, capital, populacao, porte)

# Calcula a populacao total das capitais
summarise(capitais, populacao_total = sum(populacao))

# Calcula estatisticas da populacao das capitais estaduais em 2012
summarise(capitais, 
          media_populacao = mean(populacao),
          mediana_populacao = median(populacao),
          desvio_populacao = sd(populacao)
)

# Agrupa as observacoes por regiao
capitais_regiao <- group_by(capitais, regiao)

# Calcula a populacao total por regiao
summarise(capitais_regiao, populacao_total = sum(populacao))

capitais_agrupadas_regiao <- group_by(capitais, regiao)
capitais_agrupadas_regiao

# Agrupa o banco capitais por regiao
cap_regiao <- group_by(capitais, regiao)

# Soma a populacao das capitais
cap_regiao <- mutate(cap_regiao, pop_regiao = sum(populacao))

# Resultado (usando select para selecionar algumas variaveis)
select(cap_regiao, regiao, capital, pop_regiao)

# Agrupa o banco capitais por regiao
cap_regiao <- group_by(capitais, regiao)

# Soma a populacao das capitais
cap_regiao <- mutate(cap_regiao, pop_regiao = sum(populacao))

# Desagrupa o banco
cap_regiao <- ungroup(cap_regiao)

# 3.2.5 Modificando múltiplas variáveis com mutate e summarise

# Transforma em logaritmo todas as variaveis numericas
cap <- mutate(capitais, across(where(is.numeric), log))
select(cap, where(is.numeric))

# Calcula a media de todas as variaveis numericas
summarise(capitais, across(where(is.numeric), mean))

# Transforma em logaritmo todas as variaveis que comecem com 'despesa'
mutate(capitais, across(starts_with("despesa"), log))

# Resume apenas variaveis selecionadas pelo nome
summarise(capitais, across(c(populacao, despesa_total), median))

# Calcula valores per capita das variaveis de despesa
mutate(capitais, across(contains("despesa"), \(x) x / populacao))

# 3.2.6 Encadeando operações com pipes

capitais |>
        group_by(regiao) |>
        summarise(populacao_total = sum(populacao))

# Remove as capitais com populacao menor que 500 mil habitantes
cap1 <- filter(capitais, populacao > 500000)

# Transforma a variavel populacao
cap2 <- mutate(cap1, populacao = populacao / 1000)

# Calcula a media da populacao por regiao
cap3 <- group_by(cap2, regiao)
cap4 <- summarise(cap3, media_populacao = mean(populacao))

# Calcula a media da populacao por regiao
capitais |>
        filter(populacao > 500000) |>
        mutate(populacao = populacao / 1000) |>
        group_by(regiao) |>
        summarise(media_populacao = mean(populacao))

# arrange() serve para ordenar as observacoes de um banco
head(arrange(capitais, populacao))

# rename() serve para renomear uma variavel (nome atual vem na frente, seguido do nome antigo)
names(capitais) # nomes atuais

capitais |> 
        rename(populacao_novo = populacao)
names(capitais) # novos nomes

capitais |> 
        rename(populacao = populacao_novo, SAUDE = despesa_saude)
names(capitais) # nomes novos 2

# sample_n() sorteia apenas algumas obsvervacoes de um banco
sample_frac(capitais, 2) # sorteia duas capitais
sample_frac(capitais, 3) # sorteia tres capitais

# 3.3 Cruzar e combinar dados

# Para aprendermos a fazer cruzamentos, usaremos duas bases de dados com informações sobre as cinco regiões do país. Para carregar as duas bases, chamadas de regioes e territorio e que estão no arquivo regioes.Rda10, pode usar a função load:

load("regioes.Rda")
regioes
territorio

# a base territorio tem uma linha a menos, pois não contém a região Sul. Além disso, a grafia da região Centro-Oeste está diferente nas duas bases: na base regioes, usa-se hífen; na territorio, não.

# Para cruzar dados de diferentes bases, usamos as funções _join do pacote dplyr, como left_join, inner_join, full_join e right_join

left_join(regioes, territorio, by = join_by(regiao == regiao))

# E se quisermos manter todas as observações da base territorio e usar as da base regioes para preencher valores de população? Para além da solução mais óbvia (trocar territorio e regioes de lugar), podemos usar right_join:

right_join(regioes, territorio, by = join_by(regiao == regiao))

#  O dplyr também possui outras variantes de _join úteis. Considere essas duas:

# junta apenas as observações correspondentes
inner_join(regioes, territorio, by = join_by(regiao == regiao))

# junta todas as observações, independente da correspondência
full_join(regioes, territorio, by = join_by(regiao == regiao))