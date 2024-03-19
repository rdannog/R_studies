
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

#Para usar os labels, podemos usar a função as_factor, também do pacote haven:

eseb <- as_factor(eseb)

# Com os dados carregados e com labels atribuídos, podemos fazer o seguinte gráfico usando o ggplot:

# Um grafico com ggplot

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar()

# estética (aesthetic) -> aes(x = D01A_FX_ID) ->  primeira camada do gráfico que mapeia as variáveis dos nossos dados para os atributos visuais do gráfico, gerando eixos, cores, formas e tamanhos.

# geometria (geom) -> + geom_bar() -> segunda camada do gráfico, que como o nome sugere, serve para criar barras


###############################
# Camadas de uma visualização #
###############################

#Geometrias

# No exemplo anterior, usamos a função geom_bar() para criar um gráfico de barras. No entanto, o ggplot2 tem várias outras geometrias que podem ser usadas. Podemos usar da base ESEB, a variável D01A_IDADE para criar, por exemplo, um histograma, que nada mais é do que um gráfico de barras que mostra a distribuição de uma variável numérica. Para isso, usamos a função geom_histogram():

ggplot(eseb, aes(x = D01A_IDADE)) + 
  geom_histogram()

# Outra forma de visualizar a mesma informação é usando um gráfico de densidade, que mostra a distribuição de uma variável numérica de forma suave. Para isso, usamos a função geom_density():

ggplot(eseb, aes(x = D01A_IDADE)) + 
  geom_density()

# https://fmeireles.com/livro/04-cap.html#tbl-geoms

