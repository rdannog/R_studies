#Lista de exerc?cios do cap?tulo 3
#Dan Nogueira


######## PACOTES #########
#install.packages("rio")


#################################
#                               #
# 1. Filtrando bases de dados I #
#                               #
#################################

# Importando a base de dados
#install.packages("rio")
library(rio)

library(tidyverse)

base_pop_quilombola <- import("pop_quilombola.csv")
View(base_pop_quilombola)

# Filtrando as 100 primeiras observa??es
recorte <- slice(base_pop_quilombola, 1:100)
View(recorte)



###################################
#                                 #
# 2. Filtrando bases de dados II  #
#                                 #
###################################

# quantos munic?pios t?m popula??es quilombolas no pa?s?

pop_municipios <- filter(base_pop_quilombola, pop_quilombola > 0)
View(pop_total)


####################################################
#                                                  #
# 3. Sele??o de vari?veis,filtragem e ordenamento  #
#                                                  #
####################################################

# Carregando a popula??o total dos munic?pios segundo os resultados do universo do Censo de 2022

populacao <- import("pop_total.xlsx")
View(populacao)

# Crie um novo objeto contendo apenas os 50 munic?pios mais populados do pa?s, ordenados de forma decrescente

# Selecionando variaveis a serem usadas
maiores_populacoes <- select(populacao, municipio, pop_total)
View(maiores_populacoes)

# Ordenando por ordem decrescente
populacao_ordenada <- arrange(maiores_populacoes, -pop_total)

# Selecionando os 50 municipios mais populados
populacao_ordenada <- slice(populacao_ordenada, 1:50)

view(populacao_ordenada)


############################
#                          #
# 4. Criação de variáveis  #
#                          #
############################

# Adicione ao objeto populacao duas novas variáveis: 
# uma que contenha a população dos municípios em mil habitantes 

populacao <- mutate(populacao, pop_mil = pop_total/1000)

# outra que tenha como valores Pequeno porte, para municípios com menos de 50 mil habitantes, e Outros para os demais municípios
populacao <- mutate(populacao, classificacao = if_else(pop_total < 500000, "Pequeno porte", "Outros"))

# Selecionando variaveis
#populacao <- select(populacao, municipio, pop_mil, classificacao)

view(populacao)


############################
#                          #
# 5. Resumo de variáveis   #
#                          #
############################

# Calcule o tamanho da população quilombola e a população total do Brasil.

# pop_quilombola_total <- sum(base_pop_quilombola$pop_quilombola)
# pop_brasil_total <- sum(populacao$pop_total)

# ou

summarise(populacao, pop_brasil_total = sum(pop_total)) 

summarise(base_pop_quilombola, pop_quilombola_total = sum(pop_quilombola)) 


################################
#                              #
# 6. Cruzando bases de dados I #
#                              #
################################

# Use a variável cod_ibge para cruzar as duas bases de forma que a base resultante contenha informações sobre a população total e a população quilombola de cada município. Salve o resultado dessa operação no objeto municipios.

municipios <- full_join(populacao, base_pop_quilombola, by = join_by(cod_ibge == cod_ibge))

View(municipios)
