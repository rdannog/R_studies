

library(tidyverse)
library(here)


governadores <- readRDS(here("governador_18_22.Rds")) %>%
  filter(ANO_ELEICAO == 2022) %>%
  filter(NR_TURNO == 1) %>%
  filter(SG_UF %in% c("RJ", "MG", "GO", "PR")) %>%
  as_tibble() %>% 
  select(SG_UF, NM_CANDIDATO, SG_PARTIDO, percent_gastos, percent_votos) %>%
  set_names("uf", "candidatura", "partido", "pct_gastos", "pct_votos") %>%
  filter(pct_votos > 1)

write_csv2(governadores, file = here("governadores.csv"))
   
governadores %>%
  ggplot(aes(x = pct_gastos, y = pct_votos)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  ylim(0, 100) 



