# ---
# Este script contem um exemplo de como fazer mapas de municipios
# e estados brasileiros sem precisar de shapefiles. Para isso,
# usaremos o pacote 'geobr', que obtem os mapas diretamente do IBGE.
#
# Detalhe: certifique-se de instalar o pacote antes de prosseguir.
# ---


# Pacotes necessarios

library(tidyverse)
library(geobr)


# Com o pacote 'geobr' carregado, podemos obter os mapas de municipios
# com a funcao 'read_municipality'. O argumento que passamos define o
# estado que queremos usar para delimitar o mapa (se nao passarmos nada
# o pacote carrega o mapa de todos os municipios do Brasil)

mapa_rio <- read_municipality("RJ")


# O objeto 'mapa_rio' eh um 'sf', uma especie de data.frame espacial.
# Podemos ver as primeiras linhas do objeto com a funcao 'head'
head(mapa_rio)


# A base inclui uma variavel chamada 'geom' que contem as coordenadas
# dos poligonos, isto eh, dos limites que formam os municipios.
# Podemos fazer alguns mapas rapidos com essa base usando ggplot:

ggplot(mapa_rio) +
    geom_sf()


# O mapa acima nao contem nenhuma informacao, mas podemos fazer isso combinando
# dados a partir de outras fontes. Por exemplo, podemos puxar dados de casos
# de Covid-19, que inclui uma metrica de obitos por 100 mil habitantes, daqui:
covid <- read_delim("https://raw.githubusercontent.com/Mjrovai/Python4DS/59a6a6651856e6f70fc849410994f1f44278842b/20_Mapping_Covid19_Brazil/data/cases-brazil-cities-5-5-2020.csv", delim = ",")
head(covid)


# Para adicionar os dados de Covid ao mapa, precisamos de variaveis-chave.
# Neste exemplo, vamos usar o codigo do IBGE para fazer o join (a variavel
# com o codigo dos municipios do IBGE na base de dados sobre Covid eh 'ibgeID'):
mapa_rio <- mapa_rio %>%
    left_join(covid, by = c("code_muni" = "ibgeID"))


# Como resultado, agora temos as variaveis de Covid no data.frame espacial.
# Com isso, podemso refazer o nosso mapa, agora com essa informacao
# passada para o argumento 'fill', que serve para colorir os poligonos:
ggplot(mapa_rio) +
    geom_sf(aes(fill = deaths_per_100k_inhabitants)) 


# Por padrao, o ggplot usa uma paleta sequencial, em azuis, para preencher
# os poligonos. Podemos mudar isso passando uma paleta de cores diferente,
# o que eh feito geralmente por meio de alguma funcao `scale_fill_...`.
# A titulo de exemplo, vamos usar a paleta 'viridis' para preencher os
# poligonos:
ggplot(mapa_rio) +
    geom_sf(aes(fill = deaths_per_100k_inhabitants)) +
    scale_fill_viridis_c()


# O mapa ainda esta' aparecendo com eixos e legendas, que nao sao necessarios.
# Podemos remover isso com `theme_void`:
ggplot(mapa_rio) +
    geom_sf(aes(fill = deaths_per_100k_inhabitants)) +
    scale_fill_viridis_c() +
    theme_void()


# Cortonos no mapa estão em cinza, mas branco torna o mapa mais limpo:
ggplot(mapa_rio) +
    geom_sf(aes(fill = deaths_per_100k_inhabitants), color = "white") +
    scale_fill_viridis_c() +
    theme_void()


# Finalmente, vamos mudar a posicao da legenda usando a funcao
# `theme` e o argumento `legend.position`:
ggplot(mapa_rio) +
    geom_sf(aes(fill = deaths_per_100k_inhabitants), color = "white") +
    scale_fill_viridis_c() +
    theme_void() +
    theme(legend.position = "bottom")


# E trocar o titulo da legenda:
ggplot(mapa_rio) +
    geom_sf(aes(fill = deaths_per_100k_inhabitants), color = "white") +
    scale_fill_viridis_c(name = "Óbitos por 100 mil habitantes") +
    theme_void(base_size = 14) + # Aumenta o tamanho das fontes
    theme(legend.position = "bottom")


