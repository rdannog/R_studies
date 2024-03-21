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

#  Isso feito, podemos usar pivot_wider() para transformar a coluna uf em múltiplas colunas, uma para cada estado:

tab_final <- tab_longa |>
  pivot_wider(names_from = uf, values_from = valor)

tab_final

# Nem sempre é intuitivo saber quando usar pivot_longer() e pivot_wider(), e em qual ordem, mas, com um pouco de prática, é fácil pegar os padrões mais recorrentes de transformação de acordo com os princípios tidy


## Exportando resultados

# Uma vez calculadas algumas estatísticas descritivas, a sequência natural é exportá-las para um arquivo de texto ou planilha para uso posterior. 

tab_final |>
  write_csv("minha_tabela.csv")

# O ponto negativo dessa abordagem é que, por padrão, a função write_csv() não aplica nenhuma formatação ao resultado. É por essa razão que sugerimos usar o pacote gt – um pacote que facilita a criação modular de tabelas em HTML, LaTeX ou documentos de texto – para salvar estatísticas descritivas.

tab_final |>
  gt() |>
  gtsave("minha_tabela.html")

# A tabela exportada tem uma boa formatação, ainda que falte ajustar detalhes como o excesso de casas decimais e a ausência de título e fonte. Para esses e outros ajustes finos, o pacote gt oferece uma série de funções auxiliares.

tab_final |>
  gt() |>
  fmt_number(decimals = 1) |>
  tab_header(title = "Estatísticas descritivas de gastos e votos por estado") |>
  tab_source_note(source_note = "Fonte: TSE")

# E, usando um pouco de manipulação de dados, conseguimos renomear a coluna de estatisticas e seus valores para algo mais adequado:

tab_formatada <- tab_final |>
  rename(Estatística = estatistica) |>
  mutate(Estatística = case_when(Estatística == "media_gastos" ~ "Média de gastos",
                                 Estatística == "desvio_gastos" ~ "Desvio de gastos",
                                 Estatística == "media_votos" ~ "Média de votos",
                                 Estatística == "desvio_votos" ~ "Desvio de votos")) |>
  gt() |>
  fmt_number(decimals = 1) |>
  tab_header(title = "Estatísticas descritivas de gastos e votos por estado") |>
  tab_source_note(source_note = "Fonte: TSE.") 


tab_formatada |>
  gtsave("minha_tabela.rtf")