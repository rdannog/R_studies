# Pacotes
library(tidyverse)
library(rio)


# Para iniciar a análise, foi preciso importar a base de dados contendo os dados do censo de 2010.

base <- import("data/belford_roxo.Rda")

# Como resultado, a planilha possui 6 variáveis: id, zona_domicilio, sexo, idade, renda_mensal e cor_raca. Cada uma das 469313 observações dizem respeito a uma pessoa moradora de belford roxo.


# Com a base, é possível visualizar a frequência por sexo e faixa de idade da população entre 13 e 29 anos. Minha dúvida era: "qual o tamanho da população jovem de belford roxo, dividido por sexo"?

base_nova <- base |>
  mutate(faixa = case_when(
    idade <= 17 ~ "13 a 17 anos",
    idade <= 24 ~ "18 a 24 anos",
    idade <= 29 ~ "25 a 29 anos",
    TRUE ~ "Outros"  # catchError
  ))|>
  count(faixa, sexo) |>
  rename(frequencia = n) |>
  filter(faixa != "Outros") |>
  mutate(cumsum= cumsum(frequencia)-frequencia/2)

ggplot(base_nova, aes(x =faixa, y= frequencia, fill= sexo, label =  frequencia)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_label( position = position_stack(vjust = 0.5), fill= "#d2d2d2") +
  labs(title = "Juventude de Belford-Roxo",
       x = "
       Faixa de idade", y = "Número de pessoas") +
  theme_classic() +
  theme(panel.grid.minor = element_blank())+
  scale_fill_viridis_d(name = "Sexo")



### 1. Amostragem aleatória simples
# Com a base de microdados de Belford Roxo carregada, extraia uma amostra aleatória simples de 800 pessoas (sem repetição). 

amostra <- slice_sample(base, n = 800)

# Calcule a média e o desvio padrão da renda mensal da amostra. 
media <- amostra |> 
  mean(amostra$renda_mensal, na.rm = TRUE)

# Compare esses valores com os da população do município.
  # use o slice_sample do tidyverse, set.seed(123).

