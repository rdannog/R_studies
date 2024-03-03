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
head(pessoas)


# 2. Carregando arquivos simples II

#satisfação <- read_csv("pesquisa_satisfacao.txt")

satisfação <- read_delim("pesquisa_satisfacao.txt", delim = ";")
head(satisfação)



# 3. Carregando arquivos simples III
# inventario <- read_table("inventario.tab")
# View(inventario)

casos <- read_delim("casos_registrados.csv", delim = ",", skip = 2)
view(casos)



# 4. Carregando arquivos delimitados

library(readr)

censo <- read_delim("https://raw.githubusercontent.com/izabelflores/Censo_1872/main/Censo_1872_dados_tidy_versao2.csv", delim = ";", locale = locale(encoding = "ISO-8859-1"))

head(censo)



# 5. Carregando arquivos de outros formatos I

library(rio)
library(haven)

wvs_spss <- import("wvs.sav")
wvs_stata <- import("wvs.dta")
View(wvs_spss)
View(wvs_stata)



# 6. Carregando arquivos de outros formatos II

airbnb <- import("2019_Alto_Paraiso_do_Goias_STATE_OF_GOIAS_ Airbnb_listings.xlsx")
View(airbnb)



# 7. Carregando microdados administrativos

alunos <- import("MICRODADOS_ED_SUP_IES_2022.CSV")

names(alunos)
ncol(alunos)
nrow(alunos)



