#Lista de exercícios do capítulo 3
#Dan Nogueira


######## PACOTES #########
#install.packages("rio")



#################################
#                               #
# 1. Filtrando bases de dados I #
#                               #
#################################

# Importando a base de dados

library(rio)

library(tidyverse)

base_pop_quilombola <- import("pop_quilombola.csv")
View(base_pop_quilombola)

# Filtrando os 100 primeiros
recorte <- slice(base_pop_quilombola, 1:100)
View(recorte)



###################################
#                                 #
# 2. Filtrando bases de dados II  #
#                                 #
###################################

# quantos municípios têm populações quilombolas no país?

pop_total <- filter(base_pop_quilombola, pop_quilombola > 0)
View(pop_total)