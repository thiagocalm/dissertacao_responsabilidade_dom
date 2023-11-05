options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(srvyr)

# Import data -------------------------------------------------------------

anos <- c("1970", "1980", "1991", "2000", "2010")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_sample.RData")
  load(dir)
}

# Handling data --------------------------

# 1970

df_1970 <- df_1970_sample |>
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT)) |> 
  mutate(
    # Renda domiciliar per capita
    inc_dom_pc = inc_dom/PERSONS,
    inc_dom_prop = case_when(
      INCTOT == 0 ~ 0,
      TRUE ~ INCTOT/inc_dom),
    inc_dom_prop_cat = case_when(
      inc_dom_prop > .5 ~ 1,
      inc_dom_prop > 0 ~ 2,
      TRUE ~ 3),
    inc_dom_prop_cat = factor(
      inc_dom_prop_cat,
      levels = c(1,2,3),
      labels = c("Maior", "Intermediaria", "Zero")),
    # responsavel (bin)
    responsavel = case_when(RELATED == 1000 ~ 1, TRUE ~ 0),
    # Sexo (bin para fem)
    female = case_when(SEX == 2 ~ 1, TRUE ~ 0),
    # Status Marital (factor)
    status_marital = case_when(
      MARSTD == 100 ~ 1,
      MARSTD == 110 ~ 1,
      MARSTD == 111 ~ 1,
      MARSTD == 200 ~ 2,
      MARSTD == 210 ~ 2,
      MARSTD == 211 ~ 2,
      MARSTD == 212 ~ 2,
      MARSTD == 213 ~ 2,
      MARSTD == 214 ~ 2,
      MARSTD == 215 ~ 2,
      MARSTD == 216 ~ 2,
      MARSTD == 217 ~ 2,
      MARSTD == 219 ~ 2,
      MARSTD == 220 ~ 3,
      MARSTD == 300 ~ 4,
      MARSTD == 310 ~ 4,
      MARSTD == 320 ~ 4,
      MARSTD == 330 ~ 4,
      MARSTD == 331 ~ 4,
      MARSTD == 332 ~ 4,
      MARSTD == 333 ~ 4,
      MARSTD == 334 ~ 4,
      MARSTD == 335 ~ 4,
      MARSTD == 340 ~ 4,
      MARSTD == 350 ~ 4,
      MARSTD == 400 ~ 5,
      MARSTD == 411 ~ 5,
      MARSTD == 412 ~ 5,
      MARSTD == 413 ~ 5,
      MARSTD == 420 ~ 5,
      TRUE ~ 999),
    status_marital = 
      factor(status_marital,
             levels = c(1,2,3,4,5,999),
             labels = c("Solteiras/Nunca casadas",
                        "Casadas",
                        "União consensual",
                        "Separadas/Divorciadas",
                        "Viúvas",
                        "NA/NR")),
    # Educação atingida (factor)
    educ_atingida = case_when(
      EDATTAIND == 100 ~ 2,
      EDATTAIND == 110 ~ 1,
      EDATTAIND == 120 ~ 2,
      EDATTAIND == 130 ~ 2,
      EDATTAIND == 211 ~ 2,
      EDATTAIND == 212 ~ 2,
      EDATTAIND == 221 ~ 3,
      EDATTAIND == 222 ~ 3,
      EDATTAIND == 311 ~ 4,
      EDATTAIND == 312 ~ 4,
      EDATTAIND == 400 ~ 5,
      TRUE ~ 999),
    educ_atingida = 
      factor(educ_atingida,
             levels = c(1,2,3,4,5,999),
             labels = c("Sem escolaridade",
                        "Fundamental incompleto",
                        "Fundamental completo",
                        "Médio completo",
                        "Superior completo",
                        "NA/NR")),
    # Status ocupacional (factor)
    status_ocupacional = as.factor(case_when(
      EMPSTAT == 1 ~ "Empregado",
      EMPSTAT == 2 ~ "Desempregado",
      EMPSTAT == 3 ~ "Inativo",
      TRUE ~ "NA/NR")),
    # Tipo de domicilio (factor)
    tipo_dom = case_when(
      HHTYPE == 1 ~ 1,
      HHTYPE == 2 ~ 2,
      HHTYPE == 3 ~ 3,
      HHTYPE == 4 ~ 4,
      HHTYPE == 5 ~ 6,
      HHTYPE == 6 ~ 5,
      HHTYPE == 7 ~ 6,
      HHTYPE == 8 ~ 6,
      HHTYPE == 9 ~ 6,
      HHTYPE == 10 ~ 6,
      HHTYPE == 11 ~ 6,
      TRUE ~ 99),
    tipo_dom = factor(tipo_dom,
                      levels = c(1, 2, 3, 4, 5, 6, 99),
                      labels = c("Unipessoal", "Casal sem filhos",
                                 "Casal com filhos","Monoparental",
                                 "Estendido", "Outros", "NA/NR"))
  ) |> 
  select(-c("SEX", "MARSTD", "RELATED", "EDATTAIND", "EMPSTAT", "HHTYPE")) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

# 1980

df_1980 <- df_1980_sample |>
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT)) |> 
  mutate(
    # Renda domiciliar per capita
    inc_dom_pc = inc_dom/PERSONS,
    inc_dom_prop = case_when(
      INCTOT == 0 ~ 0,
      TRUE ~ INCTOT/inc_dom),
    inc_dom_prop_cat = case_when(
      inc_dom_prop > .5 ~ 1,
      inc_dom_prop > 0 ~ 2,
      TRUE ~ 3),
    inc_dom_prop_cat = factor(
      inc_dom_prop_cat,
      levels = c(1,2,3),
      labels = c("Maior", "Intermediaria", "Zero")),
    # responsavel (bin)
    responsavel = case_when(RELATED == 1000 ~ 1, TRUE ~ 0),
    # Sexo (bin para fem)
    female = case_when(SEX == 2 ~ 1, TRUE ~ 0),
    # Status Marital (factor)
    status_marital = case_when(
      MARSTD == 100 ~ 1,
      MARSTD == 110 ~ 1,
      MARSTD == 111 ~ 1,
      MARSTD == 200 ~ 2,
      MARSTD == 210 ~ 2,
      MARSTD == 211 ~ 2,
      MARSTD == 212 ~ 2,
      MARSTD == 213 ~ 2,
      MARSTD == 214 ~ 2,
      MARSTD == 215 ~ 2,
      MARSTD == 216 ~ 2,
      MARSTD == 217 ~ 2,
      MARSTD == 219 ~ 2,
      MARSTD == 220 ~ 3,
      MARSTD == 300 ~ 4,
      MARSTD == 310 ~ 4,
      MARSTD == 320 ~ 4,
      MARSTD == 330 ~ 4,
      MARSTD == 331 ~ 4,
      MARSTD == 332 ~ 4,
      MARSTD == 333 ~ 4,
      MARSTD == 334 ~ 4,
      MARSTD == 335 ~ 4,
      MARSTD == 340 ~ 4,
      MARSTD == 350 ~ 4,
      MARSTD == 400 ~ 5,
      MARSTD == 411 ~ 5,
      MARSTD == 412 ~ 5,
      MARSTD == 413 ~ 5,
      MARSTD == 420 ~ 5,
      TRUE ~ 999),
    status_marital = 
      factor(status_marital,
             levels = c(1,2,3,4,5,999),
             labels = c("Solteiras/Nunca casadas",
                        "Casadas",
                        "União consensual",
                        "Separadas/Divorciadas",
                        "Viúvas",
                        "NA/NR")),
    # Educação atingida (factor)
    educ_atingida = case_when(
      EDATTAIND == 100 ~ 2,
      EDATTAIND == 110 ~ 1,
      EDATTAIND == 120 ~ 2,
      EDATTAIND == 130 ~ 2,
      EDATTAIND == 211 ~ 2,
      EDATTAIND == 212 ~ 2,
      EDATTAIND == 221 ~ 3,
      EDATTAIND == 222 ~ 3,
      EDATTAIND == 311 ~ 4,
      EDATTAIND == 312 ~ 4,
      EDATTAIND == 400 ~ 5,
      TRUE ~ 999),
    educ_atingida = 
      factor(educ_atingida,
             levels = c(1,2,3,4,5,999),
             labels = c("Sem escolaridade",
                        "Fundamental incompleto",
                        "Fundamental completo",
                        "Médio completo",
                        "Superior completo",
                        "NA/NR")),
    # Status ocupacional (factor)
    status_ocupacional = as.factor(case_when(
      EMPSTAT == 1 ~ "Empregado",
      EMPSTAT == 2 ~ "Desempregado",
      EMPSTAT == 3 ~ "Inativo",
      TRUE ~ "NA/NR")),
    # Tipo de domicilio (factor)
    tipo_dom = case_when(
      HHTYPE == 1 ~ 1,
      HHTYPE == 2 ~ 2,
      HHTYPE == 3 ~ 3,
      HHTYPE == 4 ~ 4,
      HHTYPE == 5 ~ 6,
      HHTYPE == 6 ~ 5,
      HHTYPE == 7 ~ 6,
      HHTYPE == 8 ~ 6,
      HHTYPE == 9 ~ 6,
      HHTYPE == 10 ~ 6,
      HHTYPE == 11 ~ 6,
      TRUE ~ 99),
    tipo_dom = factor(tipo_dom,
                      levels = c(1, 2, 3, 4, 5, 6, 99),
                      labels = c("Unipessoal", "Casal sem filhos",
                                 "Casal com filhos","Monoparental",
                                 "Estendido", "Outros", "NA/NR"))
  ) |> 
  select(-c("SEX", "MARSTD", "RELATED", "EDATTAIND", "EMPSTAT", "HHTYPE")) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

# 1991

df_1991 <- df_1991_sample |>
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT)) |> 
  mutate(
    # Renda domiciliar per capita
    inc_dom_pc = inc_dom/PERSONS,
    inc_dom_prop = case_when(
      INCTOT == 0 ~ 0,
      TRUE ~ INCTOT/inc_dom),
    inc_dom_prop_cat = case_when(
      inc_dom_prop > .5 ~ 1,
      inc_dom_prop > 0 ~ 2,
      TRUE ~ 3),
    inc_dom_prop_cat = factor(
      inc_dom_prop_cat,
      levels = c(1,2,3),
      labels = c("Maior", "Intermediaria", "Zero")),
    # responsavel (bin)
    responsavel = case_when(RELATED == 1000 ~ 1, TRUE ~ 0),
    # Sexo (bin para fem)
    female = case_when(SEX == 2 ~ 1, TRUE ~ 0),
    # Status Marital (factor)
    status_marital = case_when(
      MARSTD == 100 ~ 1,
      MARSTD == 110 ~ 1,
      MARSTD == 111 ~ 1,
      MARSTD == 200 ~ 2,
      MARSTD == 210 ~ 2,
      MARSTD == 211 ~ 2,
      MARSTD == 212 ~ 2,
      MARSTD == 213 ~ 2,
      MARSTD == 214 ~ 2,
      MARSTD == 215 ~ 2,
      MARSTD == 216 ~ 2,
      MARSTD == 217 ~ 2,
      MARSTD == 219 ~ 2,
      MARSTD == 220 ~ 3,
      MARSTD == 300 ~ 4,
      MARSTD == 310 ~ 4,
      MARSTD == 320 ~ 4,
      MARSTD == 330 ~ 4,
      MARSTD == 331 ~ 4,
      MARSTD == 332 ~ 4,
      MARSTD == 333 ~ 4,
      MARSTD == 334 ~ 4,
      MARSTD == 335 ~ 4,
      MARSTD == 340 ~ 4,
      MARSTD == 350 ~ 4,
      MARSTD == 400 ~ 5,
      MARSTD == 411 ~ 5,
      MARSTD == 412 ~ 5,
      MARSTD == 413 ~ 5,
      MARSTD == 420 ~ 5,
      TRUE ~ 999),
    status_marital = 
      factor(status_marital,
             levels = c(1,2,3,4,5,999),
             labels = c("Solteiras/Nunca casadas",
                        "Casadas",
                        "União consensual",
                        "Separadas/Divorciadas",
                        "Viúvas",
                        "NA/NR")),
    # Educação atingida (factor)
    educ_atingida = case_when(
      EDATTAIND == 100 ~ 2,
      EDATTAIND == 110 ~ 1,
      EDATTAIND == 120 ~ 2,
      EDATTAIND == 130 ~ 2,
      EDATTAIND == 211 ~ 2,
      EDATTAIND == 212 ~ 2,
      EDATTAIND == 221 ~ 3,
      EDATTAIND == 222 ~ 3,
      EDATTAIND == 311 ~ 4,
      EDATTAIND == 312 ~ 4,
      EDATTAIND == 400 ~ 5,
      TRUE ~ 999),
    educ_atingida = 
      factor(educ_atingida,
             levels = c(1,2,3,4,5,999),
             labels = c("Sem escolaridade",
                        "Fundamental incompleto",
                        "Fundamental completo",
                        "Médio completo",
                        "Superior completo",
                        "NA/NR")),
    # Status ocupacional (factor)
    status_ocupacional = as.factor(case_when(
      EMPSTAT == 1 ~ "Empregado",
      EMPSTAT == 2 ~ "Desempregado",
      EMPSTAT == 3 ~ "Inativo",
      TRUE ~ "NA/NR")),
    # Tipo de domicilio (factor)
    tipo_dom = case_when(
      HHTYPE == 1 ~ 1,
      HHTYPE == 2 ~ 2,
      HHTYPE == 3 ~ 3,
      HHTYPE == 4 ~ 4,
      HHTYPE == 5 ~ 6,
      HHTYPE == 6 ~ 5,
      HHTYPE == 7 ~ 6,
      HHTYPE == 8 ~ 6,
      HHTYPE == 9 ~ 6,
      HHTYPE == 10 ~ 6,
      HHTYPE == 11 ~ 6,
      TRUE ~ 99),
    tipo_dom = factor(tipo_dom,
                      levels = c(1, 2, 3, 4, 5, 6, 99),
                      labels = c("Unipessoal", "Casal sem filhos",
                                 "Casal com filhos","Monoparental",
                                 "Estendido", "Outros", "NA/NR"))
  ) |> 
  select(-c("SEX", "MARSTD", "RELATED", "EDATTAIND", "EMPSTAT", "HHTYPE")) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

# 2000

df_2000 <- df_2000_sample |>
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT)) |> 
  mutate(
    # Renda domiciliar per capita
    inc_dom_pc = inc_dom/PERSONS,
    inc_dom_prop = case_when(
      INCTOT == 0 ~ 0,
      TRUE ~ INCTOT/inc_dom),
    inc_dom_prop_cat = case_when(
      inc_dom_prop > .5 ~ 1,
      inc_dom_prop > 0 ~ 2,
      TRUE ~ 3),
    inc_dom_prop_cat = factor(
      inc_dom_prop_cat,
      levels = c(1,2,3),
      labels = c("Maior", "Intermediaria", "Zero")),
    # responsavel (bin)
    responsavel = case_when(RELATED == 1000 ~ 1, TRUE ~ 0),
    # Sexo (bin para fem)
    female = case_when(SEX == 2 ~ 1, TRUE ~ 0),
    # Status Marital (factor)
    status_marital = case_when(
      MARSTD == 100 ~ 1,
      MARSTD == 110 ~ 1,
      MARSTD == 111 ~ 1,
      MARSTD == 200 ~ 2,
      MARSTD == 210 ~ 2,
      MARSTD == 211 ~ 2,
      MARSTD == 212 ~ 2,
      MARSTD == 213 ~ 2,
      MARSTD == 214 ~ 2,
      MARSTD == 215 ~ 2,
      MARSTD == 216 ~ 2,
      MARSTD == 217 ~ 2,
      MARSTD == 219 ~ 2,
      MARSTD == 220 ~ 3,
      MARSTD == 300 ~ 4,
      MARSTD == 310 ~ 4,
      MARSTD == 320 ~ 4,
      MARSTD == 330 ~ 4,
      MARSTD == 331 ~ 4,
      MARSTD == 332 ~ 4,
      MARSTD == 333 ~ 4,
      MARSTD == 334 ~ 4,
      MARSTD == 335 ~ 4,
      MARSTD == 340 ~ 4,
      MARSTD == 350 ~ 4,
      MARSTD == 400 ~ 5,
      MARSTD == 411 ~ 5,
      MARSTD == 412 ~ 5,
      MARSTD == 413 ~ 5,
      MARSTD == 420 ~ 5,
      TRUE ~ 999),
    status_marital = 
      factor(status_marital,
             levels = c(1,2,3,4,5,999),
             labels = c("Solteiras/Nunca casadas",
                        "Casadas",
                        "União consensual",
                        "Separadas/Divorciadas",
                        "Viúvas",
                        "NA/NR")),
    # Educação atingida (factor)
    educ_atingida = case_when(
      EDATTAIND == 100 ~ 2,
      EDATTAIND == 110 ~ 1,
      EDATTAIND == 120 ~ 2,
      EDATTAIND == 130 ~ 2,
      EDATTAIND == 211 ~ 2,
      EDATTAIND == 212 ~ 2,
      EDATTAIND == 221 ~ 3,
      EDATTAIND == 222 ~ 3,
      EDATTAIND == 311 ~ 4,
      EDATTAIND == 312 ~ 4,
      EDATTAIND == 400 ~ 5,
      TRUE ~ 999),
    educ_atingida = 
      factor(educ_atingida,
             levels = c(1,2,3,4,5,999),
             labels = c("Sem escolaridade",
                        "Fundamental incompleto",
                        "Fundamental completo",
                        "Médio completo",
                        "Superior completo",
                        "NA/NR")),
    # Status ocupacional (factor)
    status_ocupacional = as.factor(case_when(
      EMPSTAT == 1 ~ "Empregado",
      EMPSTAT == 2 ~ "Desempregado",
      EMPSTAT == 3 ~ "Inativo",
      TRUE ~ "NA/NR")),
    # Tipo de domicilio (factor)
    tipo_dom = case_when(
      HHTYPE == 1 ~ 1,
      HHTYPE == 2 ~ 2,
      HHTYPE == 3 ~ 3,
      HHTYPE == 4 ~ 4,
      HHTYPE == 5 ~ 6,
      HHTYPE == 6 ~ 5,
      HHTYPE == 7 ~ 6,
      HHTYPE == 8 ~ 6,
      HHTYPE == 9 ~ 6,
      HHTYPE == 10 ~ 6,
      HHTYPE == 11 ~ 6,
      TRUE ~ 99),
    tipo_dom = factor(tipo_dom,
                      levels = c(1, 2, 3, 4, 5, 6, 99),
                      labels = c("Unipessoal", "Casal sem filhos",
                                 "Casal com filhos","Monoparental",
                                 "Estendido", "Outros", "NA/NR"))
  ) |> 
  select(-c("SEX", "MARSTD", "RELATED", "EDATTAIND", "EMPSTAT", "HHTYPE")) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

# 2010

df_2010 <- df_2010_sample |>
  # Renda total: atribuindo aqueles que não estão no universo == 0
  mutate(INCTOT = case_when(
    INCTOT == 9999999 ~ 0,
    INCTOT == 9999998 ~ 0,
    TRUE ~ INCTOT)) |> 
  mutate(
    # Renda domiciliar per capita
    inc_dom_pc = inc_dom/PERSONS,
    inc_dom_prop = case_when(
      INCTOT == 0 ~ 0,
      TRUE ~ INCTOT/inc_dom),
    inc_dom_prop_cat = case_when(
      inc_dom_prop > .5 ~ 1,
      inc_dom_prop > 0 ~ 2,
      TRUE ~ 3),
    inc_dom_prop_cat = factor(
      inc_dom_prop_cat,
      levels = c(1,2,3),
      labels = c("Maior", "Intermediaria", "Zero")),
    # responsavel (bin)
    responsavel = case_when(RELATED == 1000 ~ 1, TRUE ~ 0),
    # Sexo (bin para fem)
    female = case_when(SEX == 2 ~ 1, TRUE ~ 0),
    # Status Marital (factor)
    status_marital = case_when(
      MARSTD == 100 ~ 1,
      MARSTD == 110 ~ 1,
      MARSTD == 111 ~ 1,
      MARSTD == 200 ~ 2,
      MARSTD == 210 ~ 2,
      MARSTD == 211 ~ 2,
      MARSTD == 212 ~ 2,
      MARSTD == 213 ~ 2,
      MARSTD == 214 ~ 2,
      MARSTD == 215 ~ 2,
      MARSTD == 216 ~ 2,
      MARSTD == 217 ~ 2,
      MARSTD == 219 ~ 2,
      MARSTD == 220 ~ 3,
      MARSTD == 300 ~ 4,
      MARSTD == 310 ~ 4,
      MARSTD == 320 ~ 4,
      MARSTD == 330 ~ 4,
      MARSTD == 331 ~ 4,
      MARSTD == 332 ~ 4,
      MARSTD == 333 ~ 4,
      MARSTD == 334 ~ 4,
      MARSTD == 335 ~ 4,
      MARSTD == 340 ~ 4,
      MARSTD == 350 ~ 4,
      MARSTD == 400 ~ 5,
      MARSTD == 411 ~ 5,
      MARSTD == 412 ~ 5,
      MARSTD == 413 ~ 5,
      MARSTD == 420 ~ 5,
      TRUE ~ 999),
    status_marital = 
      factor(status_marital,
             levels = c(1,2,3,4,5,999),
             labels = c("Solteiras/Nunca casadas",
                        "Casadas",
                        "União consensual",
                        "Separadas/Divorciadas",
                        "Viúvas",
                        "NA/NR")),
    # Educação atingida (factor)
    educ_atingida = case_when(
      EDATTAIND == 100 ~ 2,
      EDATTAIND == 110 ~ 1,
      EDATTAIND == 120 ~ 2,
      EDATTAIND == 130 ~ 2,
      EDATTAIND == 211 ~ 2,
      EDATTAIND == 212 ~ 2,
      EDATTAIND == 221 ~ 3,
      EDATTAIND == 222 ~ 3,
      EDATTAIND == 311 ~ 4,
      EDATTAIND == 312 ~ 4,
      EDATTAIND == 400 ~ 5,
      TRUE ~ 999),
    educ_atingida = 
      factor(educ_atingida,
             levels = c(1,2,3,4,5,999),
             labels = c("Sem escolaridade",
                        "Fundamental incompleto",
                        "Fundamental completo",
                        "Médio completo",
                        "Superior completo",
                        "NA/NR")),
    # Status ocupacional (factor)
    status_ocupacional = as.factor(case_when(
      EMPSTAT == 1 ~ "Empregado",
      EMPSTAT == 2 ~ "Desempregado",
      EMPSTAT == 3 ~ "Inativo",
      TRUE ~ "NA/NR")),
    # Tipo de domicilio (factor)
    tipo_dom = case_when(
      HHTYPE == 1 ~ 1,
      HHTYPE == 2 ~ 2,
      HHTYPE == 3 ~ 3,
      HHTYPE == 4 ~ 4,
      HHTYPE == 5 ~ 6,
      HHTYPE == 6 ~ 5,
      HHTYPE == 7 ~ 6,
      HHTYPE == 8 ~ 6,
      HHTYPE == 9 ~ 6,
      HHTYPE == 10 ~ 6,
      HHTYPE == 11 ~ 6,
      TRUE ~ 99),
    tipo_dom = factor(tipo_dom,
                      levels = c(1, 2, 3, 4, 5, 6, 99),
                      labels = c("Unipessoal", "Casal sem filhos",
                                 "Casal com filhos","Monoparental",
                                 "Estendido", "Outros", "NA/NR"))
  ) |> 
  select(-c("SEX", "MARSTD", "RELATED", "EDATTAIND", "EMPSTAT", "HHTYPE")) |> 
  select(id_pes, PERWT, YEAR, grupo_etario, everything())

# Saving

save(df_1970, file = "./dados/sample/df_1970_sample_all.RData")

save(df_1980, file = "./dados/sample/df_1980_sample_all.RData")

save(df_1991, file = "./dados/sample/df_1991_sample_all.RData")

save(df_2000, file = "./dados/sample/df_2000_sample_all.RData")

save(df_2010, file = "./dados/sample/df_2010_sample_all.RData")

