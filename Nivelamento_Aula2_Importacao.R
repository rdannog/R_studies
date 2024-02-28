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


