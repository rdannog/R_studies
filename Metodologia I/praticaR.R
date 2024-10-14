# Iniciando

rm(list = ls())

# retirando notacao cientifica

options(scipen = 999)

# Carregando pacotes

pacman::p_load(tidyverse, janitor, car, rio, survey, bit64, descr,dnads, data.table, R.utils, readr, dplyr,gt, pollster,PNADcIBGE, svry)

# Carregando PNAD 2019 #

pnad2021 <- get_pnadc(year= 2021, quarter =  1, 
                      labels=TRUE, design=F)



pnad21 <- pnad2021 |> 
  select()

### O que se pretende investigar ###

### Quais var precisam ser modificadas ###