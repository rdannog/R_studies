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

#install.packages("readODS")
#install.packages("readxl")
#install.packages("haven")
#install.packages("rio")


#SPSS
# Carrega o pacote haven
library(haven)

# Carrega o arquivo 
dados <- read_sav("sleep.sav")
dados <- as_factor(dados)
View(dados)


#JSON
# Carrega o pacote rio
library(rio)

# Carrega o banco de dados do CPI 2015
cpi <- import("cpi-data.json")
glimpse(cpi)

# Carrega os mesmos dados da CPI 2015, agora em formato .Rda
load("cpi2015.Rda")


# rio
# Carrega o pacote rio
library(rio)

# Importa alguns dados
dados1 <- import("exemplo.csv")
dados2 <- import("sleep.sav")

glimpse(dados2)



#Exportando dados - criando arquivos
# Carrega o pacote readr
library(readr)

# Cria um data.frame com duas variaveis
vetor <- data.frame(x = 1:10, y = 1:10)

# Exporta ele para um arquivo .txt
write_delim(vetor, "banco.txt")
View(vetor)

# Outros pacotes
library(haven)
library(rio)

# Exporta para .sav
write_sav(vetor, "banco.sav")

# Exporta para .dta
write_dta(vetor, "banco.dta")

# Exporta para .json (e' preciso declarar 'file =')
export(vetor, "banco.json")

# Converte o arquivo 'exemplo.csv' para .sav
convert("exemplo.csv", "exemplo.sav")






#
#
#
#EXERCÍCIOS DO LIVRO
# Carrega o tidyverse
library(tidyverse) 

# 1. Carregando arquivos simples I
# pessoas <- read_delim("pessoas.csv", delim = ",", skip = 3) 

pessoas <- read_delim("pessoas.csv", delim = ",")
View(pessoas)

# 2. Carregando arquivos simples II
#satisfação <- read_csv("pesquisa_satisfacao.txt")
satisfação <- read_delim("pesquisa_satisfacao.txt", delim = ";")
head(satisfação)


# 3. Carregando arquivos simples III
# inventario <- read_table("inventario.tab")
# View(inventario)

# 4. Carregando arquivos simples IV

