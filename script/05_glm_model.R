options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(lmtest)

# 1970 ---------------------------------------------------------------------

# Import data
load("./dados/sample/df_1970_mod.RData")

df_70 <- df_1970_mod |> 
  bind_cols(dummy::dummy(df_1970_mod,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# Complex sample plan

df_70_plan <- df_70 |> 
  as_survey_design(id = id_pes,
                   weights = PERWT)

# Assessing data

df_70_plan |> 
  group_by(responsavel) |> 
  summarise(n = srvyr::survey_total(),
            prop = srvyr::survey_mean())

####
## Modeling
####


# Modelo 1
m1_70 <- svyglm(responsavel ~ grupo_etario + female +
                  educ_atingida_Fundamental.incompleto + educ_atingida_Fundamental.completo +
                  educ_atingida_Médio.completo + educ_atingida_Superior.completo,
                family = binomial, design = df_70_plan)

summary(m1_70)
round(exp(m1_70$coefficients)[summary(m1_70)$coefficients[,4] <= .05],5)
m1_70$deviance/m1_70$null.deviance # Pseudo R2

# Modelo 2
m2_70 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior,
                family = binomial, design = df_70_plan)

summary(m2_70)
round(exp(m2_70$coefficients)[summary(m2_70)$coefficients[,4] <= .05],5)
m2_70$deviance/m2_70$null.deviance # Pseudo R2

# Modelo 3
m3_70 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                     educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                     educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                     inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                     inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
                     status_marital_Separadas.Divorciadas + status_marital_Viúvas,
                family = binomial, design = df_70_plan)

summary(m3_70)
round(exp(m3_70$coefficients)[summary(m3_70)$coefficients[,4] <= .05],5)
m3_70$deviance/m3_70$null.deviance # Pseudo R2

# Joint Analysis

stargazer::stargazer(m1_70, m2_70,m3_70,
          type = "text", #na hora de rodar para entregar, colocar "latex"!
          title = "Resultados '1970'",
          align = TRUE)

waldtest(m1_70, m2_70,m3_70) # teste entre modelos

# removing all data files
model_output_70 <- list(m1_70, m2_70,m3_70)

rm(df_1970_mod, df_70, df_70_plan, m1_70, m2_70,m3_70)

save(model_output_70, file = "./output/pt3/model_output_70.RData")

# 1980 -------------------------------------------------------------

# Import data
load("./dados/sample/df_1980_mod.RData")

df_80 <- df_1980_mod |> 
  bind_cols(dummy::dummy(df_1980_mod,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# Complex sample plan

df_80_plan <- df_80 |> 
  as_survey_design(id = id_pes,
                   weights = PERWT)

# Assessing data

df_80_plan |> 
  group_by(responsavel) |> 
  summarise(n = srvyr::survey_total(),
            prop = srvyr::survey_mean())

####
## Modeling
####


# Modelo 1
m1_80 <- svyglm(responsavel ~ grupo_etario + female +
                  educ_atingida_Fundamental.incompleto + educ_atingida_Fundamental.completo +
                  educ_atingida_Médio.completo + educ_atingida_Superior.completo,
                family = binomial, design = df_80_plan)

summary(m1_80)
round(exp(m1_80$coefficients)[summary(m1_80)$coefficients[,4] <= .05],5)
m1_80$deviance/m1_80$null.deviance # Pseudo R2

# Modelo 2
m2_80 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior,
                family = binomial, design = df_80_plan)

summary(m2_80)
round(exp(m2_80$coefficients)[summary(m2_80)$coefficients[,4] <= .05],5)
m2_80$deviance/m2_80$null.deviance # Pseudo R2

# Modelo 3
m3_80 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
                  status_marital_Separadas.Divorciadas + status_marital_Viúvas,
                family = binomial, design = df_80_plan)

summary(m3_80)
round(exp(m3_80$coefficients)[summary(m3_80)$coefficients[,4] <= .05],5)
m3_80$deviance/m3_80$null.deviance # Pseudo R2

# Joint Analysis

stargazer::stargazer(m1_80, m2_80,m3_80,
                     type = "text", #na hora de rodar para entregar, colocar "latex"!
                     title = "Resultados '1980'",
                     align = TRUE)

waldtest(m1_80, m2_80,m3_80) # teste entre modelos

# removing all data files
model_output_80 <- list(m1_80, m2_80,m3_80)

rm(df_1980_mod, df_80, df_80_plan, m1_80, m2_80, m3_80)

save(model_output_80, file = "./output/pt3/model_output_80.RData")

# 1991 -------------------------------------------------------------

# Import data
load("./dados/sample/df_1991_mod.RData")

df_91 <- df_1991_mod |> 
  bind_cols(dummy::dummy(df_1991_mod,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# Complex sample plan

df_91_plan <- df_91 |> 
  as_survey_design(id = id_pes,
                   weights = PERWT)

# Assessing data

df_91_plan |> 
  group_by(responsavel) |> 
  summarise(n = srvyr::survey_total(),
            prop = srvyr::survey_mean())

####
## Modeling
####


# Modelo 1
m1_91 <- svyglm(responsavel ~ grupo_etario + female +
                  educ_atingida_Fundamental.incompleto + educ_atingida_Fundamental.completo +
                  educ_atingida_Médio.completo + educ_atingida_Superior.completo,
                family = binomial, design = df_91_plan)

summary(m1_91)
round(exp(m1_91$coefficients)[summary(m1_91)$coefficients[,4] <= .05],5)
m1_91$deviance/m1_91$null.deviance # Pseudo R2

# Modelo 2
m2_91 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior,
                family = binomial, design = df_91_plan)

summary(m2_91)
round(exp(m2_91$coefficients)[summary(m2_91)$coefficients[,4] <= .05],5)
m2_91$deviance/m2_91$null.deviance # Pseudo R2

# Modelo 3
m3_91 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
                  status_marital_Separadas.Divorciadas + status_marital_Viúvas,
                family = binomial, design = df_91_plan)

summary(m3_91)
round(exp(m3_91$coefficients)[summary(m3_91)$coefficients[,4] <= .05],5)
m3_91$deviance/m3_91$null.deviance # Pseudo R2

# Joint Analysis

stargazer::stargazer(m1_91, m2_91,m3_91,
                     type = "text", #na hora de rodar para entregar, colocar "latex"!
                     title = "Resultados '1991'",
                     align = TRUE)

waldtest(m1_91, m2_91,m3_91) # teste entre modelos

# removing all data files
model_output_91 <- list(m1_91, m2_91,m3_91)

rm(df_1991_mod, df_91, df_91_plan, m1_91, m2_91, m3_91)

save(model_output_91, file = "./output/pt3/model_output_91.RData")

# 2000 -------------------------------------------------------------

# Import data
load("./dados/sample/df_2000_mod.RData")

df_00 <- df_2000_mod |> 
  bind_cols(dummy::dummy(df_2000_mod,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# Complex sample plan

df_00_plan <- df_00 |> 
  as_survey_design(id = id_pes,
                   weights = PERWT)

# Assessing data

df_00_plan |> 
  group_by(responsavel) |> 
  summarise(n = srvyr::survey_total(),
            prop = srvyr::survey_mean())

####
## Modeling
####


# Modelo 1
m1_00 <- svyglm(responsavel ~ grupo_etario + female +
                  educ_atingida_Fundamental.incompleto + educ_atingida_Fundamental.completo +
                  educ_atingida_Médio.completo + educ_atingida_Superior.completo,
                family = binomial, design = df_00_plan)

summary(m1_00)
round(exp(m1_00$coefficients)[summary(m1_00)$coefficients[,4] <= .05],5)
m1_00$deviance/m1_00$null.deviance # Pseudo R2

# Modelo 2
m2_00 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior,
                family = binomial, design = df_00_plan)

summary(m2_00)
round(exp(m2_00$coefficients)[summary(m2_00)$coefficients[,4] <= .05],5)
m2_00$deviance/m2_00$null.deviance # Pseudo R2

# Modelo 3
m3_00 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
                  status_marital_Separadas.Divorciadas + status_marital_Viúvas,
                family = binomial, design = df_00_plan)

summary(m3_00)
round(exp(m3_00$coefficients)[summary(m3_00)$coefficients[,4] <= .05],5)
m3_00$deviance/m3_00$null.deviance # Pseudo R2

# Joint Analysis

stargazer::stargazer(m1_00, m2_00,m3_00,
                     type = "text", #na hora de rodar para entregar, colocar "latex"!
                     title = "Resultados '2000'",
                     align = TRUE)

waldtest(m1_00, m2_00,m3_00) # teste entre modelos

# removing all data files
model_output_00 <- list(m1_00, m2_00,m3_00)

rm(df_2000_mod, df_00, df_00_plan, m1_00, m2_00,m3_00)

save(model_output_00, file = "./output/pt3/model_output_00.RData")

# 2010 -------------------------------------------------------------

# Import data
load("./dados/sample/df_2010_mod.RData")

df_10 <- df_2010_mod |> 
  bind_cols(dummy::dummy(df_2010_mod,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# Complex sample plan

df_10_plan <- df_10 |> 
  as_survey_design(id = id_pes,
                   weights = PERWT)

# Assessing data

df_10_plan |> 
  group_by(responsavel) |> 
  summarise(n = srvyr::survey_total(),
            prop = srvyr::survey_mean())

####
## Modeling
####


# Modelo 1
m1_10 <- svyglm(responsavel ~ grupo_etario + female +
                  educ_atingida_Fundamental.incompleto + educ_atingida_Fundamental.completo +
                  educ_atingida_Médio.completo + educ_atingida_Superior.completo,
                family = binomial, design = df_10_plan)

summary(m1_10)
round(exp(m1_10$coefficients)[summary(m1_10)$coefficients[,4] <= .05],5)
m1_10$deviance/m1_10$null.deviance # Pseudo R2

# Modelo 2
m2_10 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior,
                family = binomial, design = df_10_plan)

summary(m2_10)
round(exp(m2_10$coefficients)[summary(m2_10)$coefficients[,4] <= .05],5)
m2_10$deviance/m2_10$null.deviance # Pseudo R2

# Modelo 3
m3_10 <- svyglm(responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
                  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
                  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
                  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
                  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
                  status_marital_Separadas.Divorciadas + status_marital_Viúvas,
                family = binomial, design = df_10_plan)

summary(m3_10)
round(exp(m3_10$coefficients)[summary(m3_10)$coefficients[,4] <= .05],5)
m3_10$deviance/m3_10$null.deviance # Pseudo R2

# Joint Analysis

stargazer::stargazer(m1_10, m2_10,m3_10,
                     type = "text", #na hora de rodar para entregar, colocar "latex"!
                     title = "Resultados '2010'",
                     align = TRUE)

waldtest(m1_10, m2_10,m3_10) # teste entre modelos

# removing all data files
model_output_10 <- list(m1_10, m2_10,m3_10)

rm(df_2010_mod, df_10, df_10_plan, m1_10, m2_10, m3_10)

save(model_output_10, file = "./output/pt3/model_output_10.RData")

