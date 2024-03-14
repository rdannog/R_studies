#Lista de exercícios do capítulo 3
#Dan Nogueira


######## PACOTES #########
#install.packages("rio")



#################################
#                               #
# 1. Filtrando bases de dados I #
#                               #
#################################

library(rio)

library(tidyverse)

pop_quilombola <- import("pop_quilombola.csv")
View(pop_quilombola)

# Filtrando os 100 primeiros
recorte_pop_quilombola <- slice(pop_quilombola, 1:100)
View(recorte_pop_quilombola)
