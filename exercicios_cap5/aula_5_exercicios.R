#Lista de exercícios do capitulo 5
#Dan Nogueira


######## PACOTES #########

library(gt)
library(tidyverse)
library(modelsummary)
library(rio)

###################################
#                                 #
# 1. Medidas de tendência central #
#                                 #
###################################

# Carregue o arquivo distribuicao_renda_rfb.txt o salve no objeto renda. 

renda <- import("distribuicao_renda_rfb.txt")

# Em seguida, use operações de manipulação de dados para calcular o total (soma), a média e a mediana da renda tributável reportada (variável rendimentos_tributaveis_milhoes) para cada centil de renda.



#  O resultado ser uma base nova com uma linha apenas por centil. A variável que indica os centis de renda chama-se centil.

