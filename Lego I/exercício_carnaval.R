#mostrar a concentração de blocos por bairro do RJ

# Pacotes necessarios

library(tidyverse)
library(rio)
library(ggplot2)


carnaval <- import("carnaval.csv")

blocos_regiao <- group_by(carnaval, regiao)
total <- mutate(blocos_regiao, classificacao = if_else(publico_estimado > 5000, "Pequeno porte", "Grande Porte"))

  
ggplot(
  data = carnaval,
  mapping = aes(x = regiao, y = ano_primeiro_desfile, color = publico_estimado)
) +
  geom_point()
