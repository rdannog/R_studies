#mostrar a concentração de blocos por bairro do RJ

# Pacotes necessarios

library(tidyverse)
library(rio)
library(ggplot2)


carnaval <- import("carnaval.csv")

blocos_regiao <- carnaval |>
  group_by(regiao) |>
  summarise(x = sum(publico_estimado))


  
