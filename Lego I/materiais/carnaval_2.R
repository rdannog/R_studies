# ---
# Este script e' a continuacao do script 'manipulacao_1.R'.
# ---


# Carrega pacotes
library(tidyverse)


# Carrega os dados
carnaval <- read_delim("carnaval.csv", delim = ";")


# Ha' outras perguntas que podemos respoder com os dados, uma delas
# e' a seguinte: quantos blocos saem antes do carnaval? Note que, para alem
# da variavel que indica a data do desfile a coluna 'data_relativa' contém
# indicativos disso. Vejamos alguns valores:
carnaval %>%
  select(data_relativa) %>%
  head(10)


# Veja o padrao: o conteudo indica com a palavra 'antes' o tempo que falta
# ate' o inicio do Carnaval. Que tal contar em quantas linhas essa palavra aparece?
# Se pudermos fazer isso, podemos responder a pergunta acima. No tidyverse, 
# podemos usar a funcao 'str_detect' para isso. Ela funciona assim:
str_detect("4 meses antes do carnaval", "antes") # TRUE
str_detect("10 dias depois do carnaval", "antes") # FALSE


# Basicamente, a funcao 'str_detect' retorna TRUE se a palavra que passamos
# na segunda posicao, como segundo argumento da funcao, estiver presente
# na string que passamos como primeiro argumento. Vamos usar essa funcao
# para contar quantas linhas contem a palavra 'antes' na coluna 'data_relativa':
carnaval |>
  filter(str_detect(data_relativa, "antes"))


# Explicacao do codigo acima: usamos pipe para passar a base de dados
# para a funcao 'filter' e, dentro de 'filter', usamos 'str_detect' para
# selecionar apenas as linhas que contem a palavra 'antes' na coluna
# 'data_relativa'. O resultado? Agora temos uma base menor, com apenas
# as linhas que contem a palavra 'antes'.


# Podemos ir alem e criar uma variavel que indica se o bloco sai antes
# do carnaval ou nao. Para isso, vamos usar a funcao 'mutate' e a funcao
# 'str_detect' dentro dela:
carnaval |>
  mutate(sai_antes = ifelse(str_detect(data_relativa, "antes"), "Antes", "Depois")) |>
  select(data_relativa, sai_antes) |>
  slice(1:5)


# O que fizemos acima foi apenas passar o resultado de 'str_detect' para
# a funcao 'ifelse'; caso uma linha retorne TRUE (isto e', a palavra 'antes'
# esteja presente na coluna 'data_relativa'), a variavel 'sai_antes' recebe
# o valor 'Antes', caso contrario, recebe o valor 'Depois'. 


# Como contar quantos blocos saem antes do carnaval? Basta contar quantas
# linhas tem o valor 'Antes' na variavel 'sai_antes' usando group_by e summarise:
carnaval |>
  mutate(sai_antes = ifelse(str_detect(data_relativa, "antes"), "Antes", "Depois")) |>
  group_by(sai_antes) |>
  summarise(n = n())


# Note que usamos `n()`, que serve para contar o numero de linhas em cada grupo.
# O que aprendemos com o exercicio acima: ha' 199 blocos que sairam antes do carnaval
# no Rio em 2018, e 396 depois (houve um missing tambem).

# O mais importante e' que essa logica pode ser aplicada a outras perguntas --
# de forma geral, em qualquer variavel de texto, string.