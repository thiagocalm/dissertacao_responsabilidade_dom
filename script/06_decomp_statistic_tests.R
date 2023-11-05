options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(lmtest)
library(car)
library(DescTools)

# Criando arquivo com resultados dos testes

estimativas <- tibble()

# 1980 - 1970 -------------------------------------------------------------

# Import data
anos <- c("1970", "1980")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

# juncao dos dados
df_80_70 <- df_1970_mod |> 
  bind_rows(df_1980_mod)

df_80_70 <- df_80_70 |> 
  mutate(ano_1980 = case_when(YEAR == 1980 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_80_70,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# formula modelo completo
formula_completo <- responsavel ~ ano_1980 + grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

# formula modelo restrito
formula_restrito <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

mod_ir <- glm(formula_completo, family = binomial, data = df_80_70)

mod_res <- glm(formula_restrito, family = binomial, data = df_80_70)

# estimativas do Teste Wald
estimativas <- waldtest(mod_ir, mod_res)[2,3:4] #teste Wald

rm(list = ls(pattern = c("mod")))
gc()

# Estimando efeito predito dos anos separadamente
mod_2 <- glm(formula_restrito, family = binomial, 
             data = df_80_70 %>% filter(ano_1980 == 1))

mod_1 <- glm(formula_restrito, family = binomial, 
             data = df_80_70 %>% filter(ano_1980 == 0))

# salvando media predita de cada ano
estimativas[1,3] <- mean(mod_2$fitted.values)
estimativas[1,4] <- mean(mod_1$fitted.values)

rm(list = ls(pattern = c("_")))
gc()

# 1991 - 1980 -------------------------------------------------------------

# Import data
anos <- c("1980", "1991")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

# juncao dos dados
df_91_80 <- df_1980_mod |> 
  bind_rows(df_1991_mod)

df_91_80 <- df_91_80 |> 
  mutate(ano_1991 = case_when(YEAR == 1991 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_91_80,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# formula modelo completo
formula_completo <- responsavel ~ ano_1991 + grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

# formula modelo restrito
formula_restrito <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

mod_ir <- glm(formula_completo, family = binomial, data = df_91_80)

mod_res <- glm(formula_restrito, family = binomial, data = df_91_80)

# estimativas do Teste Wald
estimativas[2,] <- waldtest(mod_ir, mod_res)[2,3:4] #teste Wald

rm(list = ls(pattern = c("mod")))
gc()

# Estimando efeito predito dos anos separadamente
mod_2 <- glm(formula_restrito, family = binomial,
             data = df_91_80 %>% filter(ano_1991 == 1))

mod_1 <- glm(formula_restrito, family = binomial,
             data = df_91_80 %>% filter(ano_1991 == 0))

# salvando media predita de cada ano
estimativas[2,3] <- mean(mod_2$fitted.values)
estimativas[2,4] <- mean(mod_1$fitted.values)

rm(list = ls(pattern = c("_")))
gc()

# 2000 - 1991 -------------------------------------------------------------

# Import data
anos <- c("1991", "2000")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

# juncao dos dados
df_00_91 <- df_1991_mod |> 
  bind_rows(df_2000_mod)

df_00_91 <- df_00_91 |> 
  mutate(ano_2000 = case_when(YEAR == 2000 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_00_91,int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# formula modelo completo
formula_completo <- responsavel ~ ano_2000 + grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

# formula modelo restrito
formula_restrito <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

mod_ir <- glm(formula_completo, family = binomial, data = df_00_91)

mod_res <- glm(formula_restrito, family = binomial, data = df_00_91)

# estimativas do Teste Wald
estimativas[3,] <- waldtest(mod_ir, mod_res)[2,3:4] #teste Wald

rm(list = ls(pattern = c("mod")))
gc()

# Estimando efeito predito dos anos separadamente
mod_2 <- glm(formula_restrito, family = binomial,
             data = df_00_91 %>% filter(ano_2000 == 1))

mod_1 <- glm(formula_restrito, family = binomial,
             data = df_00_91 %>% filter(ano_2000== 0))

# salvando media predita de cada ano
estimativas[3,3] <- mean(mod_2$fitted.values)
estimativas[3,4] <- mean(mod_1$fitted.values)

rm(list = ls(pattern = c("_")))
gc()

# 2010 - 2000 -------------------------------------------------------------

# Import data
anos <- c("2000", "2010")

for(i in anos){
  dir <- paste0("./dados/sample/df_", i,"_mod.RData")
  load(dir)
}

# juncao dos dados
df_10_00 <- df_2000_mod |> 
  bind_rows(df_2010_mod)

df_10_00 <- df_10_00 |> 
  mutate(ano_2010 = case_when(YEAR == 2010 ~ 1, TRUE ~ 0)) |>
  bind_cols(dummy::dummy(df_10_00, int = TRUE)) |> 
  select(-c(status_marital_NA.NR, educ_atingida_NA.NR))

# formula modelo completo
formula_completo <- responsavel ~ ano_2010 + grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

# formula modelo restrito
formula_restrito <- responsavel ~ grupo_etario + female + educ_atingida_Fundamental.incompleto + 
  educ_atingida_Fundamental.completo + educ_atingida_Médio.completo + 
  educ_atingida_Superior.completo + inc_decil_P40 + inc_decil_P60 + inc_decil_P80 +
  inc_decil_P100 + status_ocupacional_Ativo + inc_dom_prop_cat_Intermediaria +
  inc_dom_prop_cat_Maior + status_marital_Casadas + status_marital_União.consensual + 
  status_marital_Separadas.Divorciadas + status_marital_Viúvas

mod_ir <- glm(formula_completo, family = binomial, data = df_10_00)

mod_res <- glm(formula_restrito, family = binomial, data = df_10_00)

# estimativas do Teste Wald
estimativas[4,] <- waldtest(mod_ir, mod_res)[2,3:4] #teste Wald

rm(list = ls(pattern = c("mod")))
gc()

# Estimando efeito predito dos anos separadamente
mod_2 <- glm(formula_restrito, family = binomial,
             data = df_10_00 %>% filter(ano_2010 == 1))

mod_1 <- glm(formula_restrito, family = binomial,
             data = df_10_00 %>% filter(ano_2010== 0))

# salvando media predita de cada ano
estimativas[4,3] <- mean(mod_2$fitted.values)
estimativas[4,4] <- mean(mod_1$fitted.values)

rm(list = ls(pattern = c("_")))
gc()


# Salvando arquivos -------------------------------------------------------
estimativas[,5] <- c("1980-1970","1991-1980","2000-1991","2010-2000")
estimativas <- estimativas %>% 
  mutate(diferenca = V3-V4) %>%
  select(Intervalo = V5, `Estatistica-F` = `F`, `p-value` = `Pr(>F)`, Periodo_2 = V3,
         Periodo_1 = V4, `Diferenca (2-1)` = diferenca)

save(estimativas, file = "./output/pt4/testes_estatisticos_decomp_ob.RData")

write_csv(estimativas, file = "./output/pt4/testes_estatisticos_decomp_ob.csv")
