options(scipen = 999999)
gc()


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)

# Import data -------------------------------------------------------------

anos <- c("1970", "1980", "1991", "2000", "2010")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_sample_all.RData")
  load(dir)
}

# Handling data -----------------------------------------------------------

# 1970

df_1970_mod <- df_1970 |> 
  na.omit() |>
  mutate(grupo_etario = as.integer(grupo_etario)) |>  
  filter(grupo_etario >= 10 & grupo_etario <= 80) |>
  filter(status_marital != "NA/NR") |> 
  filter(educ_atingida != "NA/NR") |>
  filter(status_ocupacional != "NA/NR")

df_1970_mod <- df_1970_mod |> 
  mutate(
    # Quintil de renda per capita domiciliar
    inc_decil = ntile(inc_dom_pc, 5),
    inc_decil = factor(inc_decil,
                       levels = c(1,2,3,4,5),
                       labels = c("P20","P40","P60","P80", "P100")),
    # Status ocupacional (bin)
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Empregado" |
        status_ocupacional == "Desempregado" ~ "Ativo",
      TRUE ~ "Inativo")),
    ) |> 
  select(-c(PERNUM, INCTOT, PERSONS, inc_dom, inc_dom_pc, inc_dom_prop, tipo_dom)) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

rm(df_1970)
gc()

###
### 1980

df_1980_mod <- df_1980 |> 
  na.omit() |>
  mutate(grupo_etario = as.integer(grupo_etario)) |>  
  filter(grupo_etario >= 10 & grupo_etario <= 80) |>
  filter(status_marital != "NA/NR") |> 
  filter(educ_atingida != "NA/NR") |>
  filter(status_ocupacional != "NA/NR")

df_1980_mod <- df_1980_mod |> 
  mutate(
    # Quintil de renda per capita domiciliar
    inc_decil = ntile(inc_dom_pc, 5),
    inc_decil = factor(inc_decil,
                       levels = c(1,2,3,4,5),
                       labels = c("P20","P40","P60","P80", "P100")),
    # Status ocupacional (bin)
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Empregado" |
        status_ocupacional == "Desempregado" ~ "Ativo",
      TRUE ~ "Inativo")),
  ) |> 
  select(-c(PERNUM, INCTOT, PERSONS, inc_dom, inc_dom_pc, inc_dom_prop, tipo_dom)) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

rm(df_1980)
gc()

###
### 1991

df_1991_mod <- df_1991 |> 
  na.omit() |>
  mutate(grupo_etario = as.integer(grupo_etario)) |>  
  filter(grupo_etario >= 10 & grupo_etario <= 80) |>
  filter(status_marital != "NA/NR") |> 
  filter(educ_atingida != "NA/NR") |>
  filter(status_ocupacional != "NA/NR")

df_1991_mod <- df_1991_mod |> 
  mutate(
    # Quintil de renda per capita domiciliar
    inc_decil = ntile(inc_dom_pc, 5),
    inc_decil = factor(inc_decil,
                       levels = c(1,2,3,4,5),
                       labels = c("P20","P40","P60","P80", "P100")),
    # Status ocupacional (bin)
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Empregado" |
        status_ocupacional == "Desempregado" ~ "Ativo",
      TRUE ~ "Inativo")),
  ) |> 
  select(-c(PERNUM, INCTOT, PERSONS, inc_dom, inc_dom_pc, inc_dom_prop, tipo_dom)) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

rm(df_1991)
gc()

###
### 2000


df_2000_mod <- df_2000|> 
  na.omit() |>
  mutate(grupo_etario = as.integer(grupo_etario)) |>  
  filter(grupo_etario >= 10 & grupo_etario <= 80) |>
  filter(status_marital != "NA/NR") |> 
  filter(educ_atingida != "NA/NR") |>
  filter(status_ocupacional != "NA/NR")

df_2000_mod <- df_2000_mod |> 
  mutate(
    # Quintil de renda per capita domiciliar
    inc_decil = ntile(inc_dom_pc, 5),
    inc_decil = factor(inc_decil,
                       levels = c(1,2,3,4,5),
                       labels = c("P20","P40","P60","P80", "P100")),
    # Status ocupacional (bin)
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Empregado" |
        status_ocupacional == "Desempregado" ~ "Ativo",
      TRUE ~ "Inativo")),
  ) |> 
  select(-c(PERNUM, INCTOT, PERSONS, inc_dom, inc_dom_pc, inc_dom_prop, tipo_dom)) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

rm(df_2000)
gc()

###
### 2010


df_2010_mod <- df_2010 |> 
  na.omit() |>
  mutate(grupo_etario = as.integer(grupo_etario)) |>  
  filter(grupo_etario >= 10 & grupo_etario <= 80) |>
  filter(status_marital != "NA/NR") |> 
  filter(educ_atingida != "NA/NR") |>
  filter(status_ocupacional != "NA/NR")

df_2010_mod <- df_2010_mod |> 
  mutate(
    # Quintil de renda per capita domiciliar
    inc_decil = ntile(inc_dom_pc, 5),
    inc_decil = factor(inc_decil,
                       levels = c(1,2,3,4,5),
                       labels = c("P20","P40","P60","P80", "P100")),
    # Status ocupacional (bin)
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Empregado" |
        status_ocupacional == "Desempregado" ~ "Ativo",
      TRUE ~ "Inativo")),
  ) |> 
  select(-c(PERNUM, INCTOT, PERSONS, inc_dom, inc_dom_pc, inc_dom_prop, tipo_dom)) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

rm(df_2010)
gc()


# Saving data -------------------------------------------------------------

save(df_1970_mod, file = "./dados/sample/df_1970_mod.RData")
save(df_1980_mod, file = "./dados/sample/df_1980_mod.RData")
save(df_1991_mod, file = "./dados/sample/df_1991_mod.RData")
save(df_2000_mod, file = "./dados/sample/df_2000_mod.RData")
save(df_2010_mod, file = "./dados/sample/df_2010_mod.RData")
