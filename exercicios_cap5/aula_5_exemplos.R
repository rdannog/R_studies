#Lista de exercícios do capitulo 5
#Dan Nogueira


######## PACOTES #########

#install.packages("modelsummary")
#install.packages("gt")

######## CARREGANDO PACOTES #########

library(gt)
library(tidyverse)
library(modelsummary)

#####################################

# Usaremos uma pequena base de dados que contém os votos válidos e o percentual de gastos de campanha de candidaturas aos governos estaduais em 2022 no primeiro turno em alguns estados específicos (GO, MG, RJ e PR). 

gov <- read_csv2("governadores.csv")

 
#####################################

## Estatísticas descritivas

# Uma estatística descritiva é um número único que condensa uma propriedade de uma variável. Estatísticas descritivas comuns incluem a média, a mediana, a moda, o desvio padrão, a variância, o valor mínimo, o valor máximo, entre outros. 

mean(gov$pct_gastos) # calcular a média
median(gov$pct_gastos)  # mediana, que é o valor que divide a distribuição em duas partes iguais;
sd(gov$pct_gastos) # desvio padrão, que é a raiz quadrada da variância;
min(gov$pct_gastos) # mínimo;
max(gov$pct_gastos) # máximo;
var(gov$pct_gastos) # variância, que é a média dos quadrados dos desvios em relação à média;


# Com esse conjunto de estatísticas calculadas, já temos uma boa ideia da distribuição da variável pct_gastos da base gov. Para termos uma ideia mais completa, podemos usar a função summary(), que calcula várias estatísticas descritivas de uma variável de uma só vez:

summary(gov$pct_gastos)


## Calculando múltiplas estatísticas descritivas

# Calcular várias estatísticas de uma só vez é algo normal em pesquisas. Por exemplo, suponha que queremos calcular a média e o desvio padrão das variáveis pct_gastos e pct_votos da base gov.

gov2 <- summarise(gov, media_gastos = mean(pct_gastos), desvio_gastos = sd(pct_gastos), media_votos = mean(pct_votos), desvio_votos = sd(pct_votos))

## Estatísticas descritivas por grupo

# Outra tarefa comum é calcular estatísticas descritivas de uma variável para grupos específicos. Imagine, por exemplo, que queremos calcular a média e desvio padrão das variáveis pct_gastos e pct_votos para cada um dos três estados incluídos na base gov. Como fazemos isso? Simples: por meio das funções group_by() e summarise() do pacote dplyr



## Transformando tabelas de estatísticas descritivas

# Quando calculamos estatísticas descritivas para grupos, o resultado é uma tabela com uma linha para cada grupo e uma coluna para cada estatística calculada. Se quisermos alterar essa disposição, podemos usar os princípios tidy. Por exemplo, para obtermos uma tabela com uma linha para cada estatística calculada e uma coluna para cada grupo, podemos usar as funções pivot_longer() e pivot_wider() do pacote tidyr em duas etapas. Primeiro, usamos pivot_longer() para alongar as colunas com estatísticas:

tab_longa <- gov |>
  group_by(uf) |>
  summarise(media_gastos = mean(pct_gastos),
            desvio_gastos = sd(pct_gastos),
            media_votos = mean(pct_votos),
            desvio_votos = sd(pct_votos)) |>
  pivot_longer(cols = -uf, names_to = "estatistica", values_to = "valor")


## 