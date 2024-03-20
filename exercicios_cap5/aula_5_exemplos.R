#Lista de exercícios do capitulo 5
#Dan Nogueira


######## PACOTES #########

#install.packages("modelsummary")
#install.packages("gt")

######## CARREGANDO PACOTES #########

library(gt)
library(tidyverse)
library(modelsummary)

#####################################

# Como forma de aplicar os conceitos que veremos neste capítulo, usaremos uma pequena base de dados que contém os votos válidos e o percentual de gastos de campanha de candidaturas aos governos estaduais em 2022 no primeiro turno em alguns estados específicos (GO, MG, RJ e PR). 

gov <- read_csv2("governadores.csv")

