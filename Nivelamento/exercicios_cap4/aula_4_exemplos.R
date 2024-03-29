
################
# Visualiza��o #
################

# Como usar R gerar visualiza��es que nos ajudem a explorar dados, documentar e exportar resultados de an�lise?

# Quando bem feito, um gr�fico � capaz mostrar dados de forma precisa, n�tida e eficiente suas informa��es

library(tidyverse)


##########################
# Fundamentos do ggplot2 #
##########################

# O que precisamos para fazer um gr�fico no ggplot2? Basicamente, penas determinar quais s�o os nosso dados e a geometria que vamos usar.


# Carregando pacotes
library(haven)

# Carregando dados
eseb <- read_sav("eseb2022.sav", encoding = "latin1")

#Para usar os labels, podemos usar a fun��o as_factor, tamb�m do pacote haven:

eseb <- as_factor(eseb)

# Com os dados carregados e com labels atribu�dos, podemos fazer o seguinte gr�fico usando o ggplot:

# Um grafico com ggplot

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar()

# est�tica (aesthetic) -> aes(x = D01A_FX_ID) ->  primeira camada do gr�fico que mapeia as vari�veis dos nossos dados para os atributos visuais do gr�fico, gerando eixos, cores, formas e tamanhos.

# geometria (geom) -> + geom_bar() -> segunda camada do gr�fico, que como o nome sugere, serve para criar barras


###############################
# Camadas de uma visualiza��o #
###############################

#Geometrias

# No exemplo anterior, usamos a fun��o geom_bar() para criar um gr�fico de barras. No entanto, o ggplot2 tem v�rias outras geometrias que podem ser usadas. Podemos usar da base ESEB, a vari�vel D01A_IDADE para criar, por exemplo, um histograma, que nada mais � do que um gr�fico de barras que mostra a distribui��o de uma vari�vel num�rica. Para isso, usamos a fun��o geom_histogram():

ggplot(eseb, aes(x = D01A_IDADE)) + 
  geom_histogram()

# Outra forma de visualizar a mesma informa��o � usando um gr�fico de densidade, que mostra a distribui��o de uma vari�vel num�rica de forma suave. Para isso, usamos a fun��o geom_density():

ggplot(eseb, aes(x = D01A_IDADE)) + 
  geom_density()

# https://fmeireles.com/livro/04-cap.html#tbl-geoms


# Escalas

# podemos usar a fun��o scale_x_discrete (ou scale_y_discrete) para mapear os c�digos das faixas de idade para as faixas de idade em si:

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+"))

# A fun��o correspondente para eixos num�ricos, isto �, scale_x_continuous (ou scale_y_discrete, se for para o eixo Y), tamb�m permite mudar a escala do eixo Y de um gr�fico. Um dos seus usos mais comuns � o de alterar os valores m�ximos e m�nimos a serem exibidos no eixo desejado. Um exemplo alterando o valor m�ximo do eixo Y para 1000:

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


# Para alterar nomes dos eixos, t�tulos e subt�tulos, podemos usar a fun��o labs() em mais uma camada:

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

## Coordenadas


pop_uf <- read_delim("POP2022_Brasil.csv", delim = ";")

# podemos fazer um gr�fico de barras que mostra a popula��o de cada Unidade da Federa��o do Brasil com uma camada nova, coord_flip(), que inverte os eixos X e Y do gr�fico:

ggplot(pop_uf, aes(x = Unidades_Federacao, y=  POPULACAO)) + 
  geom_col() +
  labs(title = "Popula��o por UF",
       subtitle = "Censo 2022 (IBGE)", y = "N") +
  coord_flip() 


## Facetas

# Imagine o seguinte problema: temos os dados de faixa et�ria da popula��o do pa�s em um gr�fico de barras, mas queremos ver essa distribui��o para cada uma das cinco regi�es do pa�s. Como fazer isso sem precisar criar cinco gr�ficos individualmente? Esse � o problema que as facetas resolvem.

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) +  
  facet_wrap(~ REG) +
  labs(title = "Entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") 

# por padr�o, facet_wrap usa a mesma escala dos eixos X e Y para todos os pain�is, mas � poss�vel mudar isso com o argumento scales, que podem assumir os valores de "fixed", "free", "free_x" e "free_y". Para deixar o eixo Y de cada painel variar livremente, por exemplo, podemos fazer o seguinte:

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) + 
  facet_wrap(~ REG, scales = "free_y") +
  labs(title = "Entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") 

# outro argumento �til da fun��o facet_wrap � ncol, que permite especificar o n�mero de colunas que os pain�is devem ter. 

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) +  
  facet_wrap(~ REG, ncol = 2) +
  labs(title = "Entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") 

#  H� outras fun��es facet_ que podem ser usadas para fazer gr�ficos em subgrupos do mesmo conjunto de dados: facet_grid, por exemplo, � usada para fazer gr�ficos em subgrupos de duas vari�veis, uma para o eixo X e outra para o eixo Y. A fun��o facet_grid � especialmente �til para fazer gr�ficos em subgrupos de duas vari�veis categ�ricas, como no exemplo a seguir

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) +  facet_grid(REG ~ D02) +
  labs(title = "Entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") 


## Temas

# temas s�o fun��es que controlam v�rios recursos de exibi��o de uma visualiza��o, como tamanho da fonte e cor do plano de fundo, rota��o e cor de textos, grades e linhas de eixos, entre in�meras outras coisas.

ggplot(eseb, aes(x = D01A_FX_ID)) + 
  geom_bar() +
  scale_x_discrete(labels = c("16 e 17 anos" = "16-17",
                              "18 a 24 anos" = "18-24",
                              "25 a 34 Anos" = "25-34", 
                              "35 a 44 Anos" = "35-44",
                              "45 a 54 Anos" = "45-54",
                              "55 a 64 anos" = "55-64",
                              "65 e mais" = "65+")) +  facet_grid(REG ~ D02) +
  labs(title = "Entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") +
  theme_minimal()  #  cria um gr�fico de barras com um tema minimalista (e, portanto, menos distrativo)

# Suponha agora que, em seu projeto de visualiza��o de dados, as informa��es do eixo Y sejam desnecess�rias. Podemos usar nossa fun��o gen�rica theme para fazer essa edi��o. Como ja estamos usando um tema, a camada com a fun��o gen�rica vai logo ap�s o tema em uso.

ggplot(eseb, aes(as.character(D01A_FX_ID))) + 
  geom_bar() +
  labs(title = "N�mero de entrevistados por faixa de idade",
       subtitle = "ESEB - 2022",
       x = "Faixa de Idade",
       y = "N") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# A fun��o theme � usada para fazer customiza��es em um gr�fico, e a fun��o element_blank() � usada para remover elementos de um gr�fico. 

# A fun��o element_line, � usada para customizar linhas de um gr�fico

ggplot(eseb, aes(as.character(D01A_FX_ID))) + 
  geom_bar() +
  labs(title = "N�mero de entrevistados por faixa de idade",
       subtitle = "ESEB 2022",
       x = "Faixa de Idade",
       y = "N") +
  theme_classic() +
  theme(axis.line.x = element_line(color = "red"))