# Script de exerc�cios da aula 1
# Dan Nogueira

# 1. Primeiros passos com R

meu_ano_nascimento <- 1998
ano_atual <- 2024

minha_idade <- ano_atual - meu_ano_nascimento

# 2. Trabalhando com textos
meu_nome <- "Dan Nogueira"
paste("Meu nome é", meu_nome)

# 3. Usando funções básicas
raiz_nome <- sqrt(nchar(meu_nome))

# 4. Criando e usando vetores
notas <- c(7, 10, 5, 9)
media_notas <- mean(notas)


# 5. Usando lógica condicional

aprovado <- notas[notas > 8]  
#aqui estou pedindo para atribuir ao objeto "aprovado" 
#uma comparação de todos os valores de "notas" que são 
#maiores que 8


# 6. Trabalhando com textos (strings)

nome_abreviado <- abbreviate(meu_nome)
# Abreviado como "DnNg". Acho que ele aproveitou
# que o primeiro nome possui 3 letras, pegou as 3
# primeiras de cada nome e excluiu a vogal. Gostei.

# 7. Operações com vetores I
anos <- c(2024, 2023, 2022, 2021, 2020)
minhas_idades <- c(anos - meu_ano_nascimento)
media_idades <- mean(minhas_idades)
x <- media_idades - minha_idade

# 8. Operações com vetores II
notas_abaixo_media <- notas[notas < media_notas]

# 9. Explorando data.frames
dados_pessoais <- data.frame(anos, minhas_idades) #criando tabela
names(dados_pessoais) <- c("Anos", "Idade") #Nomeia as colunas
nrow(dados_pessoais) #Verifica o número de linhas da tabela
ncol(dados_pessoais) #Verifica o número de colunas da tabela
View(dados_pessoais) #Abre uma visualização da tabela


# 10. Manipulando data.frames I

capitais_sudeste <- data.frame(
  capital = c("Belo Horizonte", "São Paulo", "Rio de Janeiro", "Vitória"),
  estado = c("MG", "SP", "RJ", "ES"),
  populacao_por_mil = c(2315, 11451, 6211, 322)
)

grandes_capitais_sudeste <- capitais_sudeste[capitais_sudeste$populacao_por_mil > 5000,]
#Peguei as informações de "capitais_sudeste", 
#com o [] acessei nesse objeto a coluna "populacao_por_mil"
#e pedi para filtrar dentro desse item apenas as condições maiores que 5000
#Atribuí o resultado ao objeto "grandes_capitais_sudeste"
View(grandes_capitais_sudeste)

names(grandes_capitais_sudeste) <- c("Capital", "Estado", "População")


#11. Manipulando data.frames II

#names(capitais_sudeste) <- c("Capital", "Estado", "População por mil")
#estados <- list("Minas Gerais", "São Paulo", "Rio de Janeiro", "Espírito Santo")
#print(capitais_sudeste)


# 12. 
library(ggplot2)
ggplot(data = dados_pessoais, aes(x = anos, y = minhas_idades)) + 
  geom_line() + 
  geom_point()
