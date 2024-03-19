
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


# Escalas

# podemos usar a função scale_x_discrete (ou scale_y_discrete) para mapear os códigos das faixas de idade para as faixas de idade em si:

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+"))

# A função correspondente para eixos numéricos, isto é, scale_x_continuous (ou scale_y_discrete, se for para o eixo Y), também permite mudar a escala do eixo Y de um gráfico. Um dos seus usos mais comuns é o de alterar os valores máximos e mínimos a serem exibidos no eixo desejado. Um exemplo alterando o valor máximo do eixo Y para 1000:

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) + 
  scale_y_continuous(limits = c(0, 1000))


# Para alterar nomes dos eixos, títulos e subtítulos, podemos usar a função labs() em mais uma camada:

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) +  
  labs(title = "Entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") 

# Coordenadas

