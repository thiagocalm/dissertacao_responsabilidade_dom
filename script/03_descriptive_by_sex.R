options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(srvyr)

# Importing data -----------------------------------------

anos <- c(seq(1970,1980, 10), 1991, 2000, 2010)

for (i in anos) {
  dir <- paste0("./dados/sample/df_", i, "_sample_all.RData")
  load(dir)
}

# Applying complex plan ---------------------------------------------------

# 1970
df_1970 <- df_1970 |> 
  as_survey_design(ids = id_pes,
                   weights = PERWT)
# 1980
df_1980 <- df_1980 |> 
  as_survey_design(ids = id_pes,
                   weights = PERWT)
# 1991
df_1991 <- df_1991 |> 
  as_survey_design(ids = id_pes,
                   weights = PERWT)
# 2000
df_2000 <- df_2000 |> 
  as_survey_design(ids = id_pes,
                   weights = PERWT)
# 2010
df_2010 <- df_2010 |> 
  as_survey_design(ids = id_pes,
                   weights = PERWT)


# 2 - Responsaveis por grupo etario -----------------------------------

# 1970

t_2_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(female, grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |> 
      group_by(female, grupo_etario) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_2_1980 <- df_1980 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(female, grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |>
      group_by(female, grupo_etario) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_2_1991 <- df_1991 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(female, grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |> 
      group_by(female, grupo_etario) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_2_2000 <- df_2000 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(female, grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |> 
      group_by(female, grupo_etario) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_2_2010 <- df_2010 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(female, grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |> 
      group_by(female, grupo_etario) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_2_by_sex <- t_2_1970 |> 
  bind_rows(t_2_1980) |> 
  bind_rows(t_2_1991) |> 
  bind_rows(t_2_2000) |> 
  bind_rows(t_2_2010)


# 4 - Responsaveis por status marital -------------------------------------

# 1970

t_4_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(female, status_marital) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(female, status_marital) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_4_1980 <- df_1980 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(female, status_marital) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(female, status_marital) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_4_1991 <- df_1991 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(female, status_marital) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(female, status_marital) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_4_2000 <- df_2000 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(female, status_marital) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(female, status_marital) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_4_2010 <- df_2010 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(female, status_marital) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(female, status_marital) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_4_by_sex <- t_4_1970 |> 
  bind_rows(t_4_1980) |> 
  bind_rows(t_4_1991) |> 
  bind_rows(t_4_2000) |> 
  bind_rows(t_4_2010)


# 5 - Responsaveis por escolaridade atingida ------------------------------

# 1970

t_5_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(female, educ_atingida) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(female, educ_atingida) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_5_1980 <- df_1980 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(female, educ_atingida) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(female, educ_atingida) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_5_1991 <- df_1991 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(female, educ_atingida) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(female, educ_atingida) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_5_2000 <- df_2000 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(female, educ_atingida) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(female, educ_atingida) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_5_2010 <- df_2010 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(female, educ_atingida) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(female, educ_atingida) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_5_by_sex <- t_5_1970 |> 
  bind_rows(t_5_1980) |> 
  bind_rows(t_5_1991) |> 
  bind_rows(t_5_2000) |> 
  bind_rows(t_5_2010)


# 6 - Responsaveis por condicao na força de trabalho ----------------------

# 1970

t_6_1970 <- df_1970 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Inativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, female, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |>  
      filter(grupo_etario >= "10") |>
      select(female, status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino"),
        status_ocupacional = as.factor(case_when(
          status_ocupacional == "Desempregado" ~ "Inativo",
          status_ocupacional == "Empregado" ~ "Ativo",
          TRUE ~ "Inativo"))) |> 
      group_by(female, status_ocupacional) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_6_1980 <- df_1980 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Inativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, female, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |>  
      filter(grupo_etario >= "10") |>
      select(female, status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino"),
        status_ocupacional = as.factor(case_when(
          status_ocupacional == "Desempregado" ~ "Inativo",
          status_ocupacional == "Empregado" ~ "Ativo",
          TRUE ~ "Inativo"))) |> 
      group_by(female, status_ocupacional) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_6_1991 <- df_1991 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Inativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, female, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |>  
      filter(grupo_etario >= "10") |>
      select(female, status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino"),
        status_ocupacional = as.factor(case_when(
          status_ocupacional == "Desempregado" ~ "Inativo",
          status_ocupacional == "Empregado" ~ "Ativo",
          TRUE ~ "Inativo"))) |> 
      group_by(female, status_ocupacional) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_6_2000 <- df_2000 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Inativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, female, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |>  
      filter(grupo_etario >= "10") |>
      select(female, status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino"),
        status_ocupacional = as.factor(case_when(
          status_ocupacional == "Desempregado" ~ "Inativo",
          status_ocupacional == "Empregado" ~ "Ativo",
          TRUE ~ "Inativo"))) |> 
      group_by(female, status_ocupacional) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_6_2010 <- df_2010 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Inativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, female, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |>  
      filter(grupo_etario >= "10") |>
      select(female, status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino"),
        status_ocupacional = as.factor(case_when(
          status_ocupacional == "Desempregado" ~ "Inativo",
          status_ocupacional == "Empregado" ~ "Ativo",
          TRUE ~ "Inativo"))) |> 
      group_by(female, status_ocupacional) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_6_by_sex <- t_6_1970 |> 
  bind_rows(t_6_1980) |> 
  bind_rows(t_6_1991) |> 
  bind_rows(t_6_2000) |> 
  bind_rows(t_6_2010)

# 7 - Responsaveis por quintil de renda domiciliar ----------------------

# 1970

t_7_1970 <- df_1970 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |>
      group_by(female, inc_decil) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_7_1980 <- df_1980 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |>
      group_by(female, inc_decil) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_7_1991 <- df_1991 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |>
      group_by(female, inc_decil) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_7_2000 <- df_2000 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |>
      group_by(female, inc_decil) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_7_2010 <- df_2010 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5),
             female = case_when(
               female == 1 ~ "Feminino",
               TRUE ~ "Masculino")) |>
      group_by(female, inc_decil) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_7_by_sex <- t_7_1970 |> 
  bind_rows(t_7_1980) |> 
  bind_rows(t_7_1991) |> 
  bind_rows(t_7_2000) |> 
  bind_rows(t_7_2010)

# 8 - Responsaveis por parcela da renda domiciliar ----------------------

# 1970

t_8_1970 <- df_1970 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_prop_cat) |>
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |>
      group_by(female, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())
  ) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_8_1980 <- df_1980 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_prop_cat) |>
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |>
      group_by(female, inc_dom_prop_cat) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())
  ) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_8_1991 <- df_1991 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_prop_cat) |>
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |>
      group_by(female, inc_dom_prop_cat) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())
  ) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_8_2000 <- df_2000 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_prop_cat) |>
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |>
      group_by(female, inc_dom_prop_cat) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())
  ) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_8_2010 <- df_2010 |>  
  filter(grupo_etario >= "10") |>
  select(responsavel, female, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |>
  group_by(responsavel, female, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |>  
      filter(grupo_etario >= "10") |>
      select(female, inc_dom_prop_cat) |>
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |>
      group_by(female, inc_dom_prop_cat) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())
  ) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_8_by_sex <- t_8_1970 |> 
  bind_rows(t_8_1980) |> 
  bind_rows(t_8_1991) |> 
  bind_rows(t_8_2000) |> 
  bind_rows(t_8_2010)

# Analises ----------------------------------------------------------------

## 2 - Por grupo etário

# Responsaveis em relacao a pop. total

t2 |> 
  filter(!is.na(grupo_etario)) |> 
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = grupo_etario, y = prop,
      color = ANO,
      group=interaction(female, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Grupo etário quinquenal",
    y = "Distribuição relativa (%)"
  ) +
  theme_light() +
  lemon::facet_rep_grid(female ~ responsavel, repeat.tick.labels = TRUE) +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,max(t2$prop),2)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t2_by_sex |> 
  filter(!is.na(grupo_etario)) |> 
  filter(responsavel == "responsavel") |> 
  mutate(grupo_etario = case_when(grupo_etario >= 80 ~ 80, TRUE ~ grupo_etario)) %>%
  ungroup() %>% 
  reframe(n = sum(n), .by = c(ANO, responsavel, female, grupo_etario)) %>%
  summarise(grupo_etario = grupo_etario,
            prop = round(n/sum(n)*100,2), .by = c(ANO,responsavel,female)) %>% 
  ggplot() +
  aes(x = grupo_etario, y = prop,
      color = ANO,
      group=interaction(female, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  labs(
    title = "(i)",
    x = "Grupo etário quinquenal",
    y = "Distribuição relativa (%)"
  ) +
  theme_light() +
  lemon::facet_rep_grid(. ~ female, repeat.tick.labels = TRUE) +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,max(t2_by_sex$prop),2)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

## 4 - Por status marital

# Responsaveis e pop. total

t_4_by_sex |> 
  filter(responsavel != "nao_responsavel") |>
  ggplot() +
  aes(x = ANO, y = prop, fill = status_marital) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(female ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Status marital")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t_4_by_sex |> 
  filter(responsavel == "responsavel") |>
  ggplot() +
  aes(x = ANO, y = prop, fill = status_marital) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ female, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Status marital")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 5 - Por escolaridade atingida

# Responsaveis e pop. total

t_5_by_sex |> 
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = educ_atingida) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(female ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Nível de escolaridade atingido")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t_5_by_sex |> 
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = educ_atingida) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ female, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Nível de escolaridade atingido")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 6 - Por condição na força de trabalho

# Responsaveis e pop. total

t_6_by_sex |> 
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = status_ocupacional) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(female ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Condição de ocupação")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t_6_by_sex |> 
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = status_ocupacional) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ female, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Condição de ocupação")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 7 - Por quintil de renda

# Responsaveis e pop. total

t_7_by_sex <- t_7_by_sex |> 
  mutate(inc_decil =
           factor(inc_decil,
                  levels = c(1,2,3,4,5),
                  labels = c("P20","P40",
                             "P60","P80","P100")))

# Responsaveis e nao responsaveis
t_7_by_sex |>
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_decil) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(female ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Quintil de renda dom. per capita")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis
t_7_by_sex |>
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_decil) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ female, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Quintil de renda dom. per capita")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 8.1 - Por parcela de renda no domicílio (categorica)

# Responsaveis e pop. total

t_8_by_sex |>
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_dom_prop_cat) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(female ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Contribuição para a renda domiciliar total")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t_8_by_sex |>
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_dom_prop_cat) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ female, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Contribuição para a renda domiciliar total")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 8.2 - Por parcela de renda no domicílio (continua)

# Responsaveis

ggplot() +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_1970 |> as.data.frame() |> filter(responsavel == 1) |> 
      mutate(female = case_when(female == 0 ~ "Masculino", TRUE ~ "Feminino")),
    fill = "#b2182b", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_1980 |> as.data.frame() |> filter(responsavel == 1) |> 
      mutate(female = case_when(female == 0 ~ "Masculino", TRUE ~ "Feminino")),
    fill = "#ef8a62", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_1991 |> as.data.frame() |> filter(responsavel == 1) |> 
      mutate(female = case_when(female == 0 ~ "Masculino", TRUE ~ "Feminino")),
    fill = "#fddbc7", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_2000 |> as.data.frame() |> filter(responsavel == 1) |> 
      mutate(female = case_when(female == 0 ~ "Masculino", TRUE ~ "Feminino")),
    fill = "#67a9cf", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_2010 |> as.data.frame() |> filter(responsavel == 1) |> 
      mutate(female = case_when(female == 0 ~ "Masculino", TRUE ~ "Feminino")),
    fill = "#2166ac", alpha = 0.5
  ) +
  lemon::facet_rep_wrap(.~female, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = seq(-1,1.2,.2)) +
  labs(
    x = "Contribuição da renda do responsável na renda domiciliar total",
    y = "Densidade da distribuição") +
  theme(
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  ) +
  theme_light()

# Saving results ----------------------------------------------------------

saveRDS(t_2_by_sex, file = "./output/pt1/t_2_by_sex.rds")
saveRDS(t_4_by_sex, file = "./output/pt1/t_4_by_sex.rds")
saveRDS(t_5_by_sex, file = "./output/pt1/t_5_by_sex.rds")
saveRDS(t_6_by_sex, file = "./output/pt1/t_6_by_sex.rds")
saveRDS(t_7_by_sex, file = "./output/pt1/t_7_by_sex.rds")
saveRDS(t_8_by_sex, file = "./output/pt1/t_8_by_sex.rds")
