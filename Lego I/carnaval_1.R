# ---
# Este script faz serve para carregar e manipular a base de dados
# salva no arquivo 'carnaval.csv', com informacoes sobre blocos de
# carnaval no Rio de Janeiro em 2018.
#
# Lembre-se de instalar pacotes caso necessario.
# ---

# Carrega pacotes
library(tidyverse)
library(rio)


# Carrega os dados
carnaval <- import("carnaval.csv")


# Podemos ver parte dos dados com a funcao 'head'
head(carnaval)


# Podemos notar varias coisas interessantes: a base contem informacoes como o
# nome de um dado bloco, mas tambem algumas outras como a data
# relativa ('data_relativa'), que indica a quantos dias do carnaval o bloco saiu;
# a data atual de saida do bloco ('data'); a quantidade de pessoas estimada
# ('publico_estimado'); as horas de desfile e finalizacao ('desfile' e 'Final');
# alem do ano de primeiro desfile ('ano_primeiro_desfile')

# Aqui, podemos explorar os dados de varias maneiras. Que tal, por exemplo,
# ver a distribuicao da variavel de regiao ('regiao')?
carnaval %>%
    ggplot(aes(x = regiao)) +
    geom_bar()


# Podemos notar algumas coisas: 1) ha muitas barras, o que dificulta a visualizacao
# dos dados; 2) algumas barras aparecem erradas, como as que indicam "Centro" e "centro",
# que deveriam ser a mesma coisa. Resolver problemas como o 2 geralmente envolve
# manipular a base original usando 'mutate'. No caso, podemos usar a funcao 'mutate'
# junto de 'ifelse' ou 'case_when' para fazer os ajustes. Antes, vamos ver todas as 
# categorias que temos na variavel 'regiao':
carnaval %>%
    count(regiao) # Count eh uma funcao que conta as ocorrencias de cada categoria


# Valores com problemas: "centro", "zone oeste", "Zona norte 1". Vamos mudalos com:
carnaval <- carnaval |>
    mutate(regiao = case_when(
        regiao == "centro" ~ "Centro",
        regiao == "Zona oeste" ~ "Zona Oeste",
        regiao == "zona oeste" ~ "Zona Oeste",
        str_detect(regiao, "Norte|norte|Tijuca") ~ "Zona Norte",
        str_detect(regiao, "Oeste|oeste|Barra") ~ "Zona Oeste",
        TRUE ~ regiao
    )) |>
    mutate(publico_estimado = str_replace(publico_estimado, "\\.", "")) |>
    mutate(publico_estimado = as.numeric(publico_estimado))


resumo <- carnaval |> 
    group_by(regiao) |>
    summarise(pop_total = sum(publico_estimado))

ggplot(resumo, aes(x = regiao, y = pop_total)) + 
  geom_col() 



# O que o codigo acima faz eh simples: ele cria uma nova variavel 'regiao' (sobreescreve a original)
# trocando cada valor antes de ~ pelo valor especificado depois. O 'TRUE ~ regiao' eh um fallback,
# isto e', ele mantem o valor original caso nao haja nenhuma condicao anterior que seja verdadeira.

# Com a base arrumada, podemos agora fazer o grafico de barras novamente usando, tambem,
# a funcao 'coord_flip' para inverter os eixos ('coord_' e' uma camada de ggplot2):
carnaval %>%
    ggplot(aes(x = regiao)) +
    geom_bar(width = 0.5, fill= "steelblue") +
    coord_flip()+
    theme(panel.grid = element_blank())


###### str_replace

# E se quisermos ordenar as barras? Com ggplot2, podemos usar a funcao 'reorder', que e' uma
# funcao auxiliar que reordena os niveis de uma variavel de acordo com outra. Por exemplo, podemos
# reordenar 'regiao' de acordo com a quantidade de blocos que sairam em cada regiao. Para tanto,
# precisamos anter contar efetivamente a quantidade de blocos por regiao, algo que podemos fazer
# agrupando a base por 'regiao' e, depois, contando as ocorrencias de cada uma:
carnaval %>%
    group_by(regiao) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = reorder(regiao, n), y = n)) +
    geom_col(width = 0.5, fill= "steelblue") +
    coord_flip()+
    theme(panel.grid = element_blank())


# Coisas a notar: como passamos um eixo Y para aes(), tivemos que usar 'geom_col' ao inves de 'geom_bar'.
# Alem disso, usamos 'reorder' para reordenar as barras de acordo com a quantidade de blocos por regiao,
# o que e' indicado pela variavel 'n'. Podemos fazer a mesma coisa usando 'count':
carnaval %>%
    count(regiao) %>%
    ggplot(aes(x = reorder(regiao, n), y = n)) +
    geom_col() +
    coord_flip()


# Se quisermos fazer o mesmo exercicio usando a variavel 'bairro', basta trocar 'regiao' por 'bairro':

