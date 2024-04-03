# ---
# Este script usa dados do Censo de 1872 para mostrar
# como transformar uma base de dados de formato largo para longo
# usando funcoes pivot_ do pacote tidyverse.
# ---


# Comecamos carregando o pacote tidyverse
library(tidyverse)


# Os dados do Censo de 1872 em formato longo foi compilado por Izabel Flores
# (https://github.com/izabelflores), que o organizou a partir de dados compilados
# pelo Cedeplar (UFMG), e esta aberto na internet. Por isso, podemos carregar
# os dados diretamente do R com:
censo <- read_csv2("https://raw.githubusercontent.com/izabelflores/Censo_1872/main/Censo_1872_dados_tidy_versao2.csv",
                   locale = locale(encoding = "windows-1252"))


# Note que usamos 'locale = locale(encoding = "windows-1252")' porque os dados originais
# contem alguns problemas de encodificacao (i.e., quando acentos aparecem
# desconfigurados). Podemos olhar a base para ter uma ideia geral da sua
# estrutura com:
View(censo)


# Como da' para ver, a base tem informacoes de `Total_Almas` (populacao) por
# sexo e condicao, informacao essa em formato longo. No entanto, outras informa
# coes, como raca, esta ainda em formato largo.

# Como exercicio, vamos fazer o seguinte: calcular o total de pessoas por estado
# (provincia) e por raca -- conforme definidas no Censo de 1872 -- e, depois, 
# alongar a base por raca.

# Vamos comecar por contar as pessoas por cor/raca, o que podemos fazer -- como
# voces ja' sabem -- com group_by e summarise.
censo_raca <- censo %>%
  filter(Sexo_Condicao != "Total") %>%
  group_by(PrimeiroDeProvincia) %>%
  summarise(Branca = sum(Raças_Branco),
            Preta = sum(Raças_Preto),
            Parda = sum(Raças_Pardo),
            Cabocla = sum(Raças_Caboclo)
            )
  

# Vamos olhar os dados. Agora, temos uma base larga com colunas indicando o total
# de pessoas em 1872 por provincia e por raca. Essa base, no entanto, nao esta'
# em formato longo.
View(censo_raca)


# Como transforma-la em tidy? Com pivot_longer.
censo_raca <- censo_raca %>%
  pivot_longer(c(Branca, Preta, Parda, Cabocla), 
               names_to = "raca", values_to = "pessoas")

View(censo_raca)


# Com isso, agora podemos fazer um grafico usando ggplot2 para visualizar os dados
censo_raca %>%
  ggplot(aes(x = PrimeiroDeProvincia, y = pessoas, fill = raca)) +
  geom_col() +
  theme_minimal() + # Outro tema mais simples
  coord_flip() # Inverte os eixos (queremos as provincias na vertical)


# Ou podemos fazer um grafico um pouco mais legal
censo_raca %>%
  ggplot(aes(x = reorder(PrimeiroDeProvincia, pessoas), y = pessoas / 1000000, fill = raca)) +
  geom_col(width = 0.8, alpha = 0.9) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, by = 0.5)) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(face = "bold", size = 42),
        plot.subtitle = element_text(size = 26),
        legend.position = "top",
        panel.grid.major.y = element_blank()) +
  coord_flip() +
  labs(title = "População das províncias, 1872",
       subtitle = "Barras indicam o total de pessoas registradas pelo Censo, por cor/raça\n(por milhão de habitantes)",
       x = NULL, y = NULL, fill = NULL)


# Ou que permita comparar as provincias por cor/raca
censo_raca %>%
  ggplot(aes(x = reorder(PrimeiroDeProvincia, pessoas), y = pessoas / 1000000, fill = raca)) +
  geom_col(width = 0.8, alpha = 0.9) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, by = 0.5)) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(face = "bold", size = 42),
        plot.subtitle = element_text(size = 26),
        legend.position = "top",
        panel.grid.major.y = element_blank()) +
  coord_flip() +
  labs(title = "População das províncias, 1872",
       subtitle = "Barras indicam o total de pessoas registradas pelo Censo, por cor/raça\n(por milhão de habitantes)",
       x = NULL, y = NULL, fill = NULL) +
  facet_grid(~ raca)









