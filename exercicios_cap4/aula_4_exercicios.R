#Lista de exercícios do capitulo 4
#Dan Nogueira


######## PACOTES #########



#########################
#                       #
# 1. Gráficos de barras #
#                       #
#########################


##  Usando a variável Q10P2b, crie um gráfico de barras que contenha o total de votos de cada candidato na amostra do Eseb.


# Carregando pacotes

library(haven)
library(tidyverse)



# Carregando dados

eseb <- read_sav("eseb2022.sav", encoding = "ASCII")
eseb <- as_factor(eseb)


# Criando o gráfico de barras

ggplot(eseb, aes(x = Q10P2b)) + 
  geom_bar()



#####################################
#                                   #
# 2. Gráficos de barras com facetas #
#                                   #
#####################################

# Crie agora um gráfico de barras que mostra o total de votos de cada candidato na amostra do Eseb, mas com painéis para cada região do país. Use duas colunas para organizar os painéis.

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
# 3. Gráficos de histograma #
#                           #
#############################

# carregue novamente dados do Censo de 1872
censo <- read_delim("https://raw.githubusercontent.com/izabelflores/Censo_1872/main/Censo_1872_dados_tidy_versao2.csv", delim = ";", locale = locale(encoding = "ISO-8859-1"))

#  crie um gráfico que mostre a distribuição das pessoas consideradas de raça preta, segundo a denominação do Censo, indicada pela variável Raças_Preto

ggplot(censo, aes(x = Raças_Preto)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 2000))


###############################################
#                                             #
# 4. Gráficos de barras com totais calculados #
#                                             #
###############################################

# crie um gráfico de barras que mostra o total de pessoas por província

pessoas_provincia <- group_by(censo, PrimeiroDeProvincia)
provincia_resumo <- summarise(pessoas_provincia, pop_provincia = sum(Total_Almas))

ggplot(provincia_resumo, aes(x = PrimeiroDeProvincia, y = pop_provincia)) + 
  geom_col() +
  coord_flip()


View(provincia_resumo)

############################
#                          #
# 5. Temas e customizações #
#                          #
############################

# Com o mesmo gráfico do exercício anterior, aplique um tema de sua escolha e faça customizações no gráfico

