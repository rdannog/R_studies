######################
# Data visualization #
######################

###### BIBLIOTECAS ######

library(tidyverse)
library(palmerpenguins)
library(ggthemes)
library(ggplot2)
#########################


#Para entender como funciona a visualização de dados em R, usaremos o pacote palmerpenguins, que inclui o conjunto de dados penguins contendo medidas corporais de pinguins em três ilhas do Arquipélago Palmer. Além disso, utilizaremos o pacote ggthemes, que oferece uma paleta de cores segura para pessoas com daltonismo.

penguins <- palmerpenguins::penguins

######################################

#Nosso objetivo final é criar uma visualização que exibe a relação entre o comprimento das nadadeiras e a massa corporal desses pinguins, levando em consideração a espécie de cada pinguim


# Com o ggplot2, iniciamos um gráfico com a função `ggplot()`, definindo um objeto de plotagem ao qual posteriormente adiciona camadas. 

# O primeiro argumento de `ggplot()` é o conjunto de dados a ser usado no gráfico. 

#Portanto, `ggplot(data = pinguins)` cria um gráfico vazio que está pronto para exibir os dados de `pinguins`, mas como ainda não dissemos a ele como visualizá-los, por enquanto ele está vazio.


ggplot(data = penguins)


######################################

# Agora, precisamos dizer ao ggplot() como as informações do nosso banco de dados serão representadas visualmente.

# O argumento mapping da função ggplot() define como as variáveis no seu conjunto de dados são mapeadas para propriedades visuais (estéticas) do seu gráfico.

# O argumento mapping é sempre definido na função aes(), e os argumentos x e y de aes() especificam quais variáveis mapear para os eixos x e y.

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

###########################################

# Precisamos definir um geom, que é o objeto geométrico usado pelo gráfico para representar os dados. Muitas vezes descrevemos gráficos pelo tipo de geom que eles utilizam.

## Histogramas usam geoms de barra (geom_bar())
## Gráficos de linha usam geoms de linha (geom_line())
## Boxplots usam geoms de boxplot (geom_boxplot())
## Scatterplots usam geoms de ponto (geom_point()), que é o que usaremos aqui.

###########################################

# A função geom_point() adiciona uma camada de pontos ao gráfico, criando um scatterplot. O ggplot2 oferece várias funções geom, cada uma adicionando um tipo diferente de camada ao seu gráfico.

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()

############################################

# Já definimos o scatterplot com pontos usando geom_point(). Agora, vamos adicionar outra camada ao gráfico usando geom_smooth() para incluir uma linha de tendência. Especificamos method = "lm" para indicar que queremos uma linha baseada em um modelo linear (regressão linear).

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

# Chegamos a um gráfico que se parece muito com o nosso objetivo final, embora ainda não esteja perfeito. Precisamos usar formas diferentes para cada espécie de pinguim e melhorar os rótulos.

# É importante ressaltar que, em geral, não é uma boa ideia representar informações apenas usando cores em um gráfico. Pessoas percebem cores de maneira diferente devido ao daltonismo ou outras variações na visão cromática. Portanto, além da cor, também podemos mapear a espécie para a estética de forma (utilizando símbolos diferentes para cada espécie).

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()