
################
# Visualização #
################

# Como usar R gerar visualizações que nos ajudem a explorar dados, documentar e exportar resultados de análise?

# Quando bem feito, um gráfico é capaz mostrar dados de forma precisa, nítida e eficiente suas informações

library(tidyverse)


##########################
# Fundamentos do ggplot2 #
##########################

# O que precisamos para fazer um gráfico no ggplot2? Basicamente, penas determinar quais são os nosso dados e a geometria que vamos usar.


# Carregando pacotes
library(haven)

# Carregando dados
eseb <- read_sav("eseb2022.sav", encoding = "latin1")