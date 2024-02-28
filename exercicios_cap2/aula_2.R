# Script de exercícios da aula 2
# Dan Nogueira

# Vetores
meu_vetor <- c(24, 23, 26, 30)
nomes <- c("Fernando", "André", "Carlos", "Shamira")
curso <- c("CP","CP","CP","CP")

# data.frame
banco <- data.frame ( Nome = nomes, Idade = meu_vetor, Curso = curso)

print(banco)

# Acessar o primeiro elemento do vetor
meu_vetor[1]

# Acessar elementos de um data.frame
banco[,2]


# Importando uma biblioteca

library(tidyverse)

# 1. Carregando arquivos simples I
pessoas <- read_csv("pessoas.csv") #guardei no objeto "pessoas" os dados do arquivo em csv
head(pessoas)

# 2. Carregando arquivos simples II
satisfação <- read_csv("pesquisa_satisfacao.txt")
head(satisfação)

# 3. Carregando arquivos simples III
# inventario <- read_table("inventario.tab")
# View(inventario)

# 4. Carregando arquivos simples IV
casos <- read_csv("casos_registrados.csv")
head(casos)
filtro <- data.frame(casos[casos > 1])
