#Lista de exerc?cios do capitulo 4
#Dan Nogueira


######## PACOTES #########



#########################
#                       #
# 1. Gr?ficos de barras #
#                       #
#########################


##  Usando a vari?vel Q10P2b, crie um gr?fico de barras que contenha o total de votos de cada candidato na amostra do Eseb.


# Carregando pacotes

library(haven)
library(tidyverse)



# Carregando dados

eseb <- read_sav("eseb2022.sav", encoding = "ASCII")
eseb <- as_factor(eseb)


# Criando o gr?fico de barras

ggplot(eseb, aes(x = Q10P2b)) + 
  geom_bar()



#####################################
#                                   #
# 2. Graficos de barras com facetas #
#                                   #
#####################################

# Crie agora um grafico de barras que mostra o total de votos de cada candidato na amostra do Eseb, mas com paineis para cada regiao do pais. Use duas colunas para organizar os paineis.

ggplot(eseb, aes(x = Q10P2b)) + 
  geom_bar() +
  scale_x_discrete(labels = c("Jair Bolsonaro" = "Bolsonaro")) + 
  facet_wrap(~ REG, scales = "free_x") +
  labs(title = "Total de votos por candidato",
       subtitle = "Eleições 2022",
       x = "Candidato",
       y = "Votos") +
  coord_flip()



#############################
#                           #
# 3. Gr?ficos de histograma #
#                           #
#############################

# carregue novamente dados do Censo de 1872
censo <- read_delim("https://raw.githubusercontent.com/izabelflores/Censo_1872/main/Censo_1872_dados_tidy_versao2.csv", delim = ";", locale = locale(encoding = "ISO-8859-1"))

#  crie um gr?fico que mostre a distribui??o das pessoas consideradas de ra?a preta, segundo a denomina??o do Censo, indicada pela vari?vel Ra?as_Preto

ggplot(censo, aes(x = Raças_Preto)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 2000))


###############################################
#                                             #
# 4. Gr?ficos de barras com totais calculados #
#                                             #
###############################################

# crie um gr?fico de barras que mostra o total de pessoas por prov?ncia

pessoas_provincia <- group_by(censo, PrimeiroDeProvincia)
provincia_resumo <- summarise(pessoas_provincia, pop_provincia = sum(Total_Almas))

ggplot(provincia_resumo, aes(x = PrimeiroDeProvincia, y = pop_provincia)) + 
  geom_col() +
  coord_flip()


############################
#                          #
# 5. Temas e customiza??es #
#                          #
############################

# Com o mesmo gr?fico do exerc?cio anterior, aplique um tema de sua escolha e fa?a customiza??es no gr?fico

ggplot(provincia_resumo, aes(x = PrimeiroDeProvincia, y = pop_provincia)) + 
  geom_col() +
  coord_flip()+
  theme(panel.background = element_rect(fill = "lightblue", colour = "lightblue", linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid', colour = "white"))





View(provincia_resumo)
