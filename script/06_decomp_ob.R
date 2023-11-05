options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(lmtest)
source("./script/06_decomp_function.R")

# 1980 - 1970 -------------------------------------------------------------

# Import data
anos <- c("1970", "1980")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

df_80_70 <- df_1970_mod |> 
  bind_rows(df_1980_mod)

df_80_70 <- df_80_70 |> 
  mutate(ano_1980 = case_when(YEAR == 1980 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_80_70,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

###
## Decomposition
###

# Preparando base de dados para decomposição
df_80_70_dec <- df_80_70 |> 
  as.data.frame()

formula <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

gc()
dec_80_70 <- GLM_OaxacaBlinder(formula,
                               family = binomial,
                               data = df_80_70_dec,
                               groupRef = "2",
                               groupInd = "ano_1980")

dec_80_70 <- dec_80_70[-c(3)]
dec_80_70_tf <- dec_80_70[-c(1)]

save(dec_80_70_tf, file = "./output/pt4/dec_80_70_twofold.RData")

rm(df_1970_mod, df_1980_mod, df_80_70, df_80_70_dec, dec_80_70, dec_80_70_tf)

# 1991 - 1980 -------------------------------------------------------------

# Import data
anos <- c("1980", "1991")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

df_91_80 <- df_1980_mod |> 
  bind_rows(df_1991_mod)

df_91_80 <- df_91_80 |> 
  mutate(ano_1991 = case_when(YEAR == 1991 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_91_80,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

###
## Decomposition
###

# Preparando base de dados para decomposição
df_91_80_dec <- df_91_80 |> 
  as.data.frame()

formula <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

gc()
dec_91_80 <- GLM_OaxacaBlinder(formula,
                               family = binomial,
                               data = df_91_80_dec,
                               groupRef = "2",
                               groupInd = "ano_1991")
dec_91_80 <- dec_91_80[-c(3)]
dec_91_80_tf <- dec_91_80[-c(1)]

save(dec_91_80_tf, file = "./output/pt4/dec_91_80_twofold.RData")

rm(df_1980_mod, df_1991_mod, df_91_80, df_91_80_dec, dec_91_80, dec_91_80_tf)

# 2000 - 1991 -------------------------------------------------------------

# Import data
anos <- c("1991", "2000")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

df_00_91 <- df_1991_mod |> 
  bind_rows(df_2000_mod)

df_00_91 <- df_00_91 |> 
  mutate(ano_2000 = case_when(YEAR == 2000 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_00_91,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

###
## Decomposition
###

# Preparando base de dados para decomposição
df_00_91_dec <- df_00_91 |> 
  as.data.frame()

formula <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

gc()
dec_00_91 <- GLM_OaxacaBlinder(formula,
                               family = binomial,
                               data = df_00_91_dec,
                               groupRef = "2",
                               groupInd = "ano_2000")

dec_00_91 <- dec_00_91[-c(3)]
dec_00_91_tf <- dec_00_91[-c(1)]

save(dec_00_91_tf, file = "./output/pt4/dec_00_91_twofold.RData")

rm(df_1991_mod, df_2000_mod, df_00_91, df_00_91_dec, dec_00_91, dec_00_91_tf)

# 2010 - 2000 -------------------------------------------------------------

# Import data
anos <- c("2000", "2010")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

df_10_00 <- df_2000_mod |> 
  bind_rows(df_2010_mod)

df_10_00 <- df_10_00 |> 
  mutate(ano_2010 = case_when(YEAR == 2010 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_10_00,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

###
## Decomposition
###

# Preparando base de dados para decomposição
df_10_00_dec <- df_10_00 |> 
  as.data.frame()

formula <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

gc()
dec_10_00 <- GLM_OaxacaBlinder(formula,
                               family = binomial,
                               data = df_10_00_dec,
                               groupRef = "2",
                               groupInd = "ano_2010")

dec_10_00 <- dec_10_00[-c(3)]
dec_10_00_tf <- dec_10_00[-c(1)]

save(dec_10_00_tf, file = "./output/pt4/dec_10_00_twofold.RData")

rm(df_2000_mod, df_2010_mod, df_10_00, df_10_00_dec, dec_10_00, dec_10_00_tf)

# Comparing estimates -----------------------------------------------------

load("./output/pt4/dec_80_70_twofold.RData", verbose = TRUE)
load("./output/pt4/dec_91_80_twofold.RData", verbose = TRUE)
load("./output/pt4/dec_00_91_twofold.RData", verbose = TRUE)
load("./output/pt4/dec_10_00_twofold.RData", verbose = TRUE)

## Overall

dec_overall <- dec_80_70_tf$twofold$overall |> 
  as_tibble() |> 
  mutate(
    componentes = row.names(dec_80_70_tf$twofold$overall),
    periodo = '80_70',
    componentes = case_when(
      componentes == "char" ~ "efeito atributo",
      componentes == "coeff" ~ "efeito coeficiente",
      TRUE ~ "diferença total")
  ) |> 
  select(periodo, componentes, everything()) |> 
  bind_rows(
    dec_91_80_tf$twofold$overall |> 
      as_tibble() |> 
      mutate(
        componentes = row.names(dec_91_80_tf$twofold$overall),
        periodo = '91_80',
        componentes = case_when(
          componentes == "char" ~ "efeito atributo",
          componentes == "coeff" ~ "efeito coeficiente",
          TRUE ~ "diferença total")
      ) |> 
      select(periodo, componentes, everything())
  ) |> 
  bind_rows(
    dec_00_91_tf$twofold$overall |> 
      as_tibble() |> 
      mutate(
        componentes = row.names(dec_00_91_tf$twofold$overall),
        periodo = '00_91',
        componentes = case_when(
          componentes == "char" ~ "efeito atributo",
          componentes == "coeff" ~ "efeito coeficiente",
          TRUE ~ "diferença total")
      ) |> 
      select(periodo, componentes, everything())
  ) |> 
  bind_rows(
    dec_10_00_tf$twofold$overall |> 
      as_tibble() |> 
      mutate(
        componentes = row.names(dec_10_00_tf$twofold$overall),
        periodo = '10_00',
        componentes = case_when(
          componentes == "char" ~ "efeito atributo",
          componentes == "coeff" ~ "efeito coeficiente",
          TRUE ~ "diferença total")
      ) |> 
      select(periodo, componentes, everything())
  ) |> 
  mutate(periodo = factor(periodo,
                          levels = c("80_70", "91_80", "00_91", "10_00"),
                          labels = c("1980 - 1970", "1991 - 1980", "2000 - 1991", "2010 - 2000"),
                          ordered = TRUE),
         componentes = factor(componentes,
                              levels = c("efeito atributo", "efeito coeficiente",
                                         "diferença total"),
                              ordered = TRUE))

## Detailed

dec_detailed <- dec_80_70_tf$twofold$variables |> 
  as_tibble() |> 
  select(variables,
         atributo = `value (char)`,
         atributo_prop = `prop (% char)`,
         coeficiente = `value (coeff)`,
         coeficiente_prop = `prop (% coeff)`) |> 
  mutate(
    periodo = '80_70',
    atributo = as.numeric(atributo),
    atributo_prop = as.numeric(atributo_prop),
    coeficiente = as.numeric(coeficiente),
    coeficiente_prop = as.numeric(coeficiente_prop)
  ) |> 
  select(periodo, everything()) |> 
  bind_rows(
    dec_91_80_tf$twofold$variables |> 
      as_tibble() |> 
      select(variables,
             atributo = `value (char)`,
             atributo_prop = `prop (% char)`,
             coeficiente = `value (coeff)`,
             coeficiente_prop = `prop (% coeff)`) |> 
      mutate(
        periodo = '91_80',
        atributo = as.numeric(atributo),
        atributo_prop = as.numeric(atributo_prop),
        coeficiente = as.numeric(coeficiente),
        coeficiente_prop = as.numeric(coeficiente_prop)
      ) |> 
      select(periodo, everything())
  ) |> 
  bind_rows(
    dec_00_91_tf$twofold$variables |> 
      as_tibble() |> 
      select(variables,
             atributo = `value (char)`,
             atributo_prop = `prop (% char)`,
             coeficiente = `value (coeff)`,
             coeficiente_prop = `prop (% coeff)`) |> 
      mutate(
        periodo = '00_91',
        atributo = as.numeric(atributo),
        atributo_prop = as.numeric(atributo_prop),
        coeficiente = as.numeric(coeficiente),
        coeficiente_prop = as.numeric(coeficiente_prop)
      ) |> 
      select(periodo, everything())
  ) |> 
  bind_rows(
    dec_10_00_tf$twofold$variables |> 
      as_tibble() |> 
      select(variables,
             atributo = `value (char)`,
             atributo_prop = `prop (% char)`,
             coeficiente = `value (coeff)`,
             coeficiente_prop = `prop (% coeff)`) |> 
      mutate(
        periodo = '10_00',
        atributo = as.numeric(atributo),
        atributo_prop = as.numeric(atributo_prop),
        coeficiente = as.numeric(coeficiente),
        coeficiente_prop = as.numeric(coeficiente_prop)
      ) |> 
      select(periodo, everything())
  ) |> 
  mutate(periodo = factor(periodo,
                          levels = c("80_70", "91_80", "00_91", "10_00"),
                          labels = c("1980 - 1970", "1991 - 1980", "2000 - 1991", "2010 - 2000"),
                          ordered = TRUE))

## Fatores

fatores <- c("nao_observados",rep("demografico",6),rep("socioeconomico",7),rep("domiciliar",4))

dec_factors <- dec_detailed |> 
  mutate(fatores = rep(fatores, 4)) |>
  group_by(periodo, fatores) |> 
  summarise(
    atributo = sum(atributo),
    atributo_prop = sum(atributo_prop),
    coeficiente = sum(coeficiente),
    coeficiente_prop = sum(coeficiente_prop)
  ) |>
  mutate(fatores = factor(fatores,
                          levels = c("nao_observados", "demografico", "socioeconomico", "domiciliar"),
                          labels = c("Não observados", "Demográfico", "Socioeconômico", "Domiciliar"),
                          ordered = TRUE))

dec_factors |> 
  group_by(periodo) |> 
  summarise(atributo = sum(atributo), 
            atributo_prop = sum(atributo_prop), 
            coeficiente = sum(coeficiente), 
            coeficiente_prop = sum(coeficiente_prop))

dec_overall

# Saving data -------------------------------------------------------------

df_dec <- list(overall = dec_overall, detailed = dec_detailed, factors = dec_factors)

save(df_dec, file = "./output/pt4/resultados_decomposicao.RData")
