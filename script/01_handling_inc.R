options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(data.table)

# Selected variables ------------------------------------------------------

vars_inc <- c("id_dom","id_pes", "INCTOT", "PERSONS")
vars <- c("id_pes", "id_dom", "YEAR", "PERWT", "SEX", "MARSTD", "PERNUM", "RELATED", "EDATTAIND", 
          "INCTOT","EMPSTAT", "grupo_etario", "HHTYPE", "PERSONS")

# 1970 -----------------------------------------------------------

# Import data

load("./dados/raw/censo_1970.RData")

# Selecao de de variaveis

df_1970 <- df_1970 |> 
  select(all_of(vars))

df_1970_inc <- df_1970 |> 
  select(all_of(vars_inc)) |> 
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT))

# Criando variaveis derivadas de renda
gc()
df_1970_inc <- df_1970_inc |> 
  group_by(id_dom) |>
  mutate(inc_dom = sum(INCTOT)) |>  # agrupando renda total domiciliar
  ungroup() |> 
  select(id_pes, id_dom, inc_dom)

# gc()
# df_1970_inc <- df_1970_inc |> 
#   mutate(
#     # Renda domiciliar per capita
#     inc_dom_pc = inc_dom/PERSONS,
#     # Parcela da renda atribuida a cada corresidente
#     inc_dom_prop = fcase(INCTOT == 0, 0,
#                          TRUE, INCTOT/inc_dom)) |> 
#   select(-c(id_dom, INCTOT, PERSONS))

# Juntar novas variaveis
gc()
df_1970 <- df_1970 |> 
  left_join(df_1970_inc,
            by = c("id_dom" = "id_dom", "id_pes" = "id_pes"),
            keep = FALSE,
            multiple = "first")

save(df_1970, file = "./dados/df_1970.RData")

# removing dataset

rm(df_1970, df_1970_inc)
gc()

# 1980 -----------------------------------------------------------

# Import data

load("./dados/raw/censo_1980.RData")

# Selecao de de variaveis

df_1980 <- df_1980 |> 
  select(all_of(vars))

df_1980_inc <- df_1980 |> 
  select(all_of(vars_inc)) |> 
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT))

# Criando variaveis derivadas de renda
gc()
df_1980_inc <- df_1980_inc |> 
  group_by(id_dom) |>
  mutate(inc_dom = sum(INCTOT)) |>  # agrupando renda total domiciliar
  ungroup() |> 
  select(id_pes, id_dom, inc_dom)

# gc()
# df_1980_inc <- df_1980_inc |> 
#   mutate(
#     # Renda domiciliar per capita
#     inc_dom_pc = inc_dom/PERSONS,
#     # Parcela da renda atribuida a cada corresidente
#     inc_dom_prop = fcase(INCTOT == 0, 0,
#                          TRUE, INCTOT/inc_dom)) |> 
#   select(-c(id_dom, INCTOT, PERSONS))

# Juntar novas variaveis
gc()
df_1980 <- df_1980 |> 
  left_join(df_1980_inc,
            by = c("id_dom" = "id_dom", "id_pes" = "id_pes"),
            keep = FALSE,
            multiple = "first")

save(df_1980, file = "./dados/df_1980.RData")

# removing dataset

rm(df_1980, df_1980_inc)
gc()

# 1991 -----------------------------------------------------------

# Import data

load("./dados/raw/censo_1991.RData")

# Selecao de de variaveis

df_1991 <- df_1991 |> 
  select(all_of(vars))

df_1991_inc <- df_1991 |> 
  select(all_of(vars_inc)) |> 
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT))

# Criando variaveis derivadas de renda
gc()
df_1991_inc <- df_1991_inc |> 
  group_by(id_dom) |>
  mutate(inc_dom = sum(INCTOT)) |>  # agrupando renda total domiciliar
  ungroup() |> 
  select(id_pes, id_dom, inc_dom)

# gc()
# df_1991_inc <- df_1991_inc |> 
#   mutate(
#     # Renda domiciliar per capita
#     inc_dom_pc = inc_dom/PERSONS,
#     # Parcela da renda atribuida a cada corresidente
#     inc_dom_prop = fcase(INCTOT == 0, 0,
#                          TRUE, INCTOT/inc_dom)) |> 
#   select(-c(id_dom, INCTOT, PERSONS))

# Juntar novas variaveis
gc()
df_1991 <- df_1991 |> 
  left_join(df_1991_inc,
            by = c("id_dom" = "id_dom", "id_pes" = "id_pes"),
            keep = FALSE,
            multiple = "first")

save(df_1991, file = "./dados/df_1991.RData")

# removing dataset

rm(df_1991, df_1991_inc)
gc()

# 2000 -----------------------------------------------------------

# Import data

load("./dados/raw/censo_2000.RData")

# Selecao de de variaveis

df_2000 <- df_2000 |> 
  select(all_of(vars))

df_2000_inc <- df_2000 |> 
  select(all_of(vars_inc)) |> 
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT))

# Criando variaveis derivadas de renda
gc()
df_2000_inc <- df_2000_inc |> 
  group_by(id_dom) |>
  mutate(inc_dom = sum(INCTOT)) |>  # agrupando renda total domiciliar
  ungroup() |> 
  select(id_pes, id_dom, inc_dom)

# gc()
# df_2000_inc <- df_2000_inc |> 
#   mutate(
#     # Renda domiciliar per capita
#     inc_dom_pc = inc_dom/PERSONS,
#     # Parcela da renda atribuida a cada corresidente
#     inc_dom_prop = fcase(INCTOT == 0, 0,
#                          TRUE, INCTOT/inc_dom)) |> 
#   select(-c(id_dom, INCTOT, PERSONS))

# Juntar novas variaveis
gc()
df_2000 <- df_2000 |> 
  left_join(df_2000_inc,
            by = c("id_dom" = "id_dom", "id_pes" = "id_pes"),
            keep = FALSE,
            multiple = "first")

save(df_2000, file = "./dados/df_2000.RData")

# removing dataset

rm(df_2000, df_2000_inc)
gc()

# 2010 -----------------------------------------------------------

# Import data

load("./dados/raw/censo_2010.RData")

# Selecao de de variaveis

df_2010 <- df_2010 |> 
  select(all_of(vars))

df_2010_inc <- df_2010 |> 
  select(all_of(vars_inc)) |> 
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT))

# Criando variaveis derivadas de renda
gc()
df_2010_inc <- df_2010_inc |> 
  group_by(id_dom) |>
  mutate(inc_dom = sum(INCTOT)) |>  # agrupando renda total domiciliar
  ungroup() |> 
  select(id_pes, id_dom, inc_dom)

# gc()
# df_2010_inc <- df_2010_inc |> 
#   mutate(
#     # Renda domiciliar per capita
#     inc_dom_pc = inc_dom/PERSONS,
#     # Parcela da renda atribuida a cada corresidente
#     inc_dom_prop = fcase(INCTOT == 0, 0,
#                          TRUE, INCTOT/inc_dom)) |> 
#   select(-c(id_dom, INCTOT, PERSONS))

# Juntar novas variaveis
gc()
df_2010 <- df_2010 |> 
  left_join(df_2010_inc,
            by = c("id_dom" = "id_dom", "id_pes" = "id_pes"),
            keep = FALSE,
            multiple = "first")

save(df_2010, file = "./dados/df_2010.RData")

# removing dataset

rm(df_2010, df_2010_inc)
gc()
