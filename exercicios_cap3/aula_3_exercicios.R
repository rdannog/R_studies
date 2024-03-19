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


####################################################
#                                                  #
# 3. Seleção de variáveis,filtragem e ordenamento  #
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


####################################################
#                                                  #
# 7. Criação de variáveis, filtragem e ordenamento #
#                                                  #
####################################################

# crie uma variável que indique a proporção de pessoas quilombolas em relação ao total da população de cada município

municipios_pop_quilombola <- mutate(municipios, proporcao_quilombola = (pop_quilombola/pop_total)*1000)

# Reporte os 3 municípios com as maiores proporções de pessoas quilombolas nos comentários.

pop_ordenada <- arrange(municipios_pop_quilombola, -proporcao_quilombola)
pop_ordenada <- slice(pop_ordenada, 1:3)

View(pop_ordenada)
# Alcântara (MA) - 84%
# Berilo (MG) - 58%
# Cavalcante (GO) - 57%



######################################
#                                    #
# 8. Cruzamento de bases de dados II #
#                                    #
######################################

# Na pasta de materiais, há um arquivo chamado uf_municipios.csv com três variáveis: regiao, sigla_uf e cod_ibge. Carregue e salve essa base no objeto uf_municipios

uf_municipios <- import("uf_municipios.csv")

# Adicione as 2 variáveis com informações de região e estado à base municipios.

municipios_completo <- right_join(uf_municipios, municipios_pop_quilombola, by = join_by(cod_ibge == cod_ibge))


# ao final, a base municipios deverá ter 6 variáveis: regiao, sigla_uf, municipio, cod_ibge, pop_total e pop_quilombola

municipios_completo <- select(municipios_completo, regiao, sigla_uf, municipio, cod_ibge, pop_total, pop_quilombola, -proporcao_quilombola)



##########################################
#                                        #
# 9. Agrupamento e resumo de variáveis I #
#                                        #
##########################################

# Calcule a população quilombola e a população total de cada uma das regiões do país.

# Agrupando por região
municipios_novo <- group_by(municipios_completo, regiao)

# Calculando a população por região
municipios_resumo <- summarise(municipios, pop_regiao = sum(pop_total), pop_quilombola_regiao = sum(pop_quilombola))

# Calcule a proporção de pessoas quilombolas em relação à população total de cada região

municipios_proporcao <- mutate(municipios_resumo, proporcao = (pop_quilombola_regiao/pop_regiao)*1000)

View(municipios_proporcao)



############################################
#                                          #
# 10. Agrupamento e resumo de variáveis II #
#                                          #
############################################

# Descubra quais são os 5 estados com as maiores proporções de pessoas quilombolas em relação à população total.

# Agrupando por estado

estados <- group_by(municipios, sigla_uf)

# Calculando a população por estado

estados_resumo <- summarise(estados, pop_estado = sum(pop_total), pop_quilombola_estado = sum(pop_quilombola))

# Calculando a proporção de pessoas quilombolas em relação à população total de cada estado

estados_proporcao <- mutate(estados_resumo, proporcao = (pop_quilombola_estado/pop_estado)*1000)

# Reporte os 5 estados com as maiores proporções de pessoas quilombolas nos comentários.

estados_ordenados <- arrange(estados_proporcao, -proporcao)
estados_ordenados <- slice(estados_ordenados, 1:5)

# MA (39%)
# BA (28%)
# AP (17%)
# PA (16%)
# SE (12%)


############################################
#                                          #
#  11. Agrupamento e criação de variáveis  #
#                                          #
############################################

# Ainda com a base municipios, crie uma nova base chamada taxa_pop_quilombola que tenha apenas duas variáveis: sigla_uf, com a sigla de cada estado, e taxa_pop_quilombola, com o número de pessoas quilombolas por 100 mil habitantes. A base final deve ter apenas 27 linhas, uma para cada unidade da federação. Use essa base para descobrir qual é o estado com a maior taxa de pessoas quilombolas por 100 mil habitantes no país.


# Calculando a proporção de pessoas quilombolas em relação à população total de cada estado

estados_proporcao <- mutate(estados_resumo, taxa = (pop_quilombola_estado/pop_estado)*100000)


# Reporte os 5 estados com as maiores proporções de pessoas quilombolas nos comentários.

taxa_pop_quilombola <- arrange(estados_proporcao, -taxa)
taxa_pop_quilombola <- select(taxa_pop_quilombola, sigla_uf, taxa)

# MA (3971 pessoas a cada 100.000 habitantes)

