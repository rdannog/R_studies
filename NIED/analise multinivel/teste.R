library(rio)
library(tidyverse)
library(knitr)
library(sjPlot)
library(lme4)


### SOBRE OS GRÁFICOS DO NIED: rodar regressão e intervalo de confiança. Inserir nos gráficos.
# World value survey, International social survey.
# SURVEYS NO REPOSITÓRIO DA UNICAMP: https://www.cesop.unicamp.br/por (só pro Brasil)



# https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html

# SOBRE A BASE ----
# Cada linha é a resposta de uma pessoa aos surveys de cada país no seu
# respectivo ano.


# Modelos TESTE ----
aaa <-lm(age ~ country + year, data = elite)

lmer(redistrib_free_education~country+year+question_simil_redistrib_free_education+ response_category_redistrib_free_education+(1|country))

 

# Plota gráfico com as informações que precisamos (a predição) !!!! 
plot_model(aaa, type = "pred", terms = c("country", "year[2005]"))



# 1. Importando a base ----
elite <- import("Elite harmonization - harmonized data.csv")


# RAUL ----

# Carregar pacotes necessários
library(lme4)
library(dplyr)

# Criar unique_combinations com os níveis corretos
unique_combinations <- Elite_data %>%
  distinct(country,
           year,
           elite_type,
           sampling_nom,
           question_simil_redistrib_free_education,
           survey) %>%
  mutate(country = factor(
    country,
    levels = unique(Elite_data$country)),
    year = factor(year, levels = unique(Elite_data$year)),
    elite_type = factor(elite_type, levels = unique(Elite_data$elite_type)),
    survey = factor(survey, levels = unique(Elite_data$survey)) )

# Verificar níveis das variáveis
print(levels(unique_combinations$country))

print(levels(unique_combinations$year))

print(levels(unique_combinations$elite_type))

print(levels(unique_combinations$survey))

# Criar a matriz de design
design_matrix <- model.matrix(terms(modelofree_education), unique_combinations)

# Verificar a matriz de design
head(design_matrix) 

# Obter as predições do modelo para as combinações únicas
unique_combinations$predictions <- predict(modelofree_education,
                                           newdata = unique_combinations,
                                           allow.new.levels = TRUE) 

# Visualizar as primeiras linhas das predições head(unique_combinations)
tab_model(aaa, show.se=T)


# 2. IGNORAR POR ENQUANTO ----

teste <- elite %>%
  select(country, year) %>% 
  group_by(country) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  unique() %>% 
  arrange(country)

#


elite2 <- elite %>% 
  select(question_simil_redistrib_free_education, response_category_redistrib_free_education, country, year)