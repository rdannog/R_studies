# ---
# Este script faz serve para carregar e manipular a base de dados
# salva no arquivo 'carnaval.csv', com informacoes sobre blocos de
# carnaval no Rio de Janeiro em 2018.
# ---

# Carrega pacotes
library(tidyverse)
library(rio)


# Carrega os dados
carnaval <- import("carnaval.csv")


# Aqui, podemos explorar os dados de varias maneiras. Que tal, por exemplo,
# ver a distribuicao da variavel de regiao ('regiao')?
carnaval |>
    ggplot(aes(x = regiao)) +
    geom_bar()


# Podemos notar algumas coisas: 1) ha muitas barras, o que dificulta a visualizacao
# dos dados; 2) algumas barras aparecem erradas. Resolver problemas como o 2 geralmente envolve
# manipular a base original usando 'mutate'. No caso, podemos usar a funcao 'mutate'
# junto de 'ifelse' ou 'case_when' para fazer os ajustes. Antes, vamos ver todas as 
# categorias que temos na variavel 'regiao':

carnaval |>
    count(regiao) 

# Count eh uma funcao que conta as ocorrencias de cada categoria


# Valores com problemas: "centro", "zone oeste", "Zona norte 1". 
# Além disso, os valores de público estimado estão como texto, quando deveriam ser valores numéricos.
# Vamos muda-los com:

carnaval <- carnaval |>
    mutate(regiao = case_when(
        regiao == "centro" ~ "Centro",
        regiao == "Zona oeste" ~ "Zona Oeste",
        regiao == "zona oeste" ~ "Zona Oeste",
        str_detect(regiao, "Norte|norte|Tijuca") ~ "Zona Norte",
        str_detect(regiao, "Oeste|oeste|Barra") ~ "Zona Oeste",
        TRUE ~ regiao)) |>
    mutate(publico_estimado = str_replace(publico_estimado, "\\.", "")) |>
    mutate(publico_estimado = as.numeric(publico_estimado))


# Com os dados arrumados, agora podemos, por exemplo, agrupar os dados por região e criar uma variável com o resumo da soma do público total estimado por região, no carnaval de 2018

resumo_carnaval <- carnaval |> 
    group_by(regiao) |>
    summarise(publico_total = sum(publico_estimado))

# Gerando uma visualização da distribuição total de público por região com ggplot2

ggplot(resumo_carnaval, aes(x = regiao, y = publico_total)) + 
  geom_col(width = 0.7, fill= "steelblue")+
  coord_flip()+
  theme(panel.grid = element_blank())


# E se quisermos ordenar as barras? 
# Com ggplot2, podemos usar a funcao 'reorder', que reordena os niveis de uma variavel de acordo com outra. Por exemplo, podemos reordenar 'regiao' de acordo com a quantidade de blocos que sairam em cada regiao. 
# Para tanto, precisamos contar efetivamente a quantidade de blocos por regiao, algo que podemos fazer agrupando a base por 'regiao' e, depois, contando as ocorrencias de cada uma:

carnaval |>
    group_by(regiao) |>
    summarise(n = n()) |>
    ggplot(aes(x = reorder(regiao, n), y = n)) +
    geom_col(width = 0.7, fill= "steelblue") +
    coord_flip()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank())


# Coisas a notar: como passamos um eixo Y para aes(), tivemos que usar 'geom_col' ao inves de 'geom_bar'.
# Alem disso, usamos 'reorder' para reordenar as barras de acordo com a quantidade de blocos por regiao, o que e' indicado pela variavel 'n'. 
# Podemos fazer a mesma coisa usando 'count':

carnaval |>
    count(regiao) |>
    ggplot(aes(x = reorder(regiao, n), y = n)) +
    geom_col() +
    coord_flip()


# Se quisermos fazer o mesmo exercicio usando a variavel 'bairro', basta trocar 'regiao' por 'bairro':

carnaval |>
    count(bairro) |>
    ggplot(aes(x = reorder(bairro, n), y = n)) +
    geom_col() +
    coord_flip()
