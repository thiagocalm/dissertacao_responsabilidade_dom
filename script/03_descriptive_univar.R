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

# 1 - Numero absoluto de pessoas, segundo condicao de responsabilidade -----

# 1970

t_1_1970 <- df_1970 |>
  filter(grupo_etario >= "10") |> 
  select(responsavel) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |>
      filter(grupo_etario >= "10") |> 
      select(responsavel) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_1_1980 <- df_1980 |>
  filter(grupo_etario >= "10") |>
  select(responsavel) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |>
      filter(grupo_etario >= "10") |> 
      select(responsavel) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_1_1991 <- df_1991 |>
  filter(grupo_etario >= "10") |>
  select(responsavel) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |>
      filter(grupo_etario >= "10") |>
      select(responsavel) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_1_2000 <- df_2000 |>
  filter(grupo_etario >= "10") |>
  select(responsavel) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |>
      filter(grupo_etario >= "10") |>
      select(responsavel) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_1_2010 <- df_2010 |>
  filter(grupo_etario >= "10") |>
  select(responsavel) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |>
      filter(grupo_etario >= "10") |>
      select(responsavel) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_1 <- t_1_1970 |> 
  bind_rows(t_1_1980) |> 
  bind_rows(t_1_1991) |> 
  bind_rows(t_1_2000) |> 
  bind_rows(t_1_2010)


# 2 - Responsaveis por grupo etario -----------------------------------

# 1970

t_2_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario)) |> 
  group_by(responsavel, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario)) |> 
      group_by(grupo_etario) |> 
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
  select(responsavel, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario)) |> 
  group_by(responsavel, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario)) |> 
      group_by(grupo_etario) |> 
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
  select(responsavel, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario)) |> 
  group_by(responsavel, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario)) |> 
      group_by(grupo_etario) |> 
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
  select(responsavel, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario)) |> 
  group_by(responsavel, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario)) |> 
      group_by(grupo_etario) |> 
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
  select(responsavel, grupo_etario) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    grupo_etario = as.integer(grupo_etario)) |> 
  group_by(responsavel, grupo_etario) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(grupo_etario) |> 
      mutate(grupo_etario = as.integer(grupo_etario)) |> 
      group_by(grupo_etario) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_2 <- t_2_1970 |> 
  bind_rows(t_2_1980) |> 
  bind_rows(t_2_1991) |> 
  bind_rows(t_2_2000) |> 
  bind_rows(t_2_2010)

# 3 - Responsaveis por sexo ---------------------------------------------------

# 1970

t_3_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(female) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      group_by(female) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1970,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1980

t_3_1980 <- df_1980 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(female) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      group_by(female) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1980,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 1991

t_3_1991 <- df_1991 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(female) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      group_by(female) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 1991,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2000

t_3_2000 <- df_2000 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(female) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      group_by(female) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2000,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# 2010

t_3_2010 <- df_2010 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, female) |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    female = case_when(
      female == 1 ~ "Feminino",
      TRUE ~ "Masculino")) |> 
  group_by(responsavel, female) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(female) |> 
      mutate(female = case_when(
        female == 1 ~ "Feminino",
        TRUE ~ "Masculino")) |> 
      group_by(female) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_3 <- t_3_1970 |> 
  bind_rows(t_3_1980) |> 
  bind_rows(t_3_1991) |> 
  bind_rows(t_3_2000) |> 
  bind_rows(t_3_2010)

# 4 - Responsaveis por status marital -------------------------------------

# 1970

t_4_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(status_marital) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(status_marital) |> 
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
  select(responsavel, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(status_marital) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(status_marital) |> 
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
  select(responsavel, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(status_marital) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(status_marital) |> 
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
  select(responsavel, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(status_marital) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(status_marital) |> 
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
  select(responsavel, status_marital) |> 
  filter(status_marital != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, status_marital) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(status_marital) |> 
      filter(status_marital != "NA/NR") |> 
      group_by(status_marital) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_4 <- t_4_1970 |> 
  bind_rows(t_4_1980) |> 
  bind_rows(t_4_1991) |> 
  bind_rows(t_4_2000) |> 
  bind_rows(t_4_2010)


# 5 - Responsaveis por escolaridade atingida ------------------------------

# 1970

t_5_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(educ_atingida) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(educ_atingida) |> 
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
  select(responsavel, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(educ_atingida) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(educ_atingida) |> 
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
  select(responsavel, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(educ_atingida) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(educ_atingida) |> 
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
  select(responsavel, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(educ_atingida) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(educ_atingida) |> 
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
  select(responsavel, educ_atingida) |> 
  filter(educ_atingida != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, educ_atingida) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(educ_atingida) |> 
      filter(educ_atingida != "NA/NR") |> 
      group_by(educ_atingida) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_5 <- t_5_1970 |> 
  bind_rows(t_5_1980) |> 
  bind_rows(t_5_1991) |> 
  bind_rows(t_5_2000) |> 
  bind_rows(t_5_2010)

# 6 - Responsaveis por condicao na força de trabalho ----------------------

# 1970

t_6_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Ativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(status_ocupacional = as.factor(case_when(
        status_ocupacional == "Desempregado" ~ "Ativo",
        status_ocupacional == "Empregado" ~ "Ativo",
        TRUE ~ "Inativo"))) |> 
      group_by(status_ocupacional) |> 
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
  select(responsavel, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Ativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(status_ocupacional = as.factor(case_when(
        status_ocupacional == "Desempregado" ~ "Ativo",
        status_ocupacional == "Empregado" ~ "Ativo",
        TRUE ~ "Inativo"))) |> 
      group_by(status_ocupacional) |> 
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
  select(responsavel, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Ativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |>
      mutate(status_ocupacional = as.factor(case_when(
        status_ocupacional == "Desempregado" ~ "Ativo",
        status_ocupacional == "Empregado" ~ "Ativo",
        TRUE ~ "Inativo"))) |> 
      group_by(status_ocupacional) |> 
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
  select(responsavel, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Ativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |>  
  group_by(responsavel, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(status_ocupacional = as.factor(case_when(
        status_ocupacional == "Desempregado" ~ "Ativo",
        status_ocupacional == "Empregado" ~ "Ativo",
        TRUE ~ "Inativo"))) |> 
      group_by(status_ocupacional) |> 
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
  select(responsavel, status_ocupacional) |> 
  filter(status_ocupacional != "NA/NR") |> 
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel"),
    status_ocupacional = as.factor(case_when(
      status_ocupacional == "Desempregado" ~ "Ativo",
      status_ocupacional == "Empregado" ~ "Ativo",
      TRUE ~ "Inativo"))) |> 
  group_by(responsavel, status_ocupacional) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(status_ocupacional) |> 
      filter(status_ocupacional != "NA/NR") |> 
      mutate(status_ocupacional = as.factor(case_when(
        status_ocupacional == "Desempregado" ~ "Ativo",
        status_ocupacional == "Empregado" ~ "Ativo",
        TRUE ~ "Inativo"))) |> 
      group_by(status_ocupacional) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_6 <- t_6_1970 |> 
  bind_rows(t_6_1980) |> 
  bind_rows(t_6_1991) |> 
  bind_rows(t_6_2000) |> 
  bind_rows(t_6_2010)

# 7 - Responsaveis por quintil de renda domiciliar ----------------------

# 1970

t_7_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5)) |> 
      group_by(inc_decil) |> 
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
  select(responsavel, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5)) |> 
      group_by(inc_decil) |> 
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
  select(responsavel, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5)) |> 
      group_by(inc_decil) |> 
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
  select(responsavel, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5)) |> 
      group_by(inc_decil) |> 
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
  select(responsavel, inc_dom_pc) |>
  filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
  mutate(
    inc_decil = ntile(inc_dom_pc, 5),
    responsavel = case_when(
      responsavel == 1 ~ "responsavel",
      TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_decil) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_pc) |>
      filter(inc_dom_pc != 9999999 | inc_dom_pc != 9999998) |> 
      mutate(inc_decil = ntile(inc_dom_pc, 5)) |> 
      group_by(inc_decil) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_7 <- t_7_1970 |> 
  bind_rows(t_7_1980) |> 
  bind_rows(t_7_1991) |> 
  bind_rows(t_7_2000) |> 
  bind_rows(t_7_2010)

# 8 - Responsaveis por parcela da renda domiciliar ----------------------

# 1970

t_8_1970 <- df_1970 |> 
  filter(grupo_etario >= "10") |>
  select(responsavel, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1970) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1970 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_prop_cat) |>
      group_by(inc_dom_prop_cat) |> 
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
  select(responsavel, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1980) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1980 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_prop_cat) |>
      group_by(inc_dom_prop_cat) |> 
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
  select(responsavel, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 1991) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_1991 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_prop_cat) |>
      group_by(inc_dom_prop_cat) |> 
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
  select(responsavel, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2000) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2000 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_prop_cat) |>
      group_by(inc_dom_prop_cat) |> 
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
  select(responsavel, inc_dom_prop_cat) |>
  mutate(responsavel = case_when(
    responsavel == 1 ~ "responsavel",
    TRUE ~ "nao_responsavel")) |> 
  group_by(responsavel, inc_dom_prop_cat) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
  mutate(ANO = 2010) |> 
  select(ANO, everything()) |> 
  bind_rows(
    df_2010 |> 
      filter(grupo_etario >= "10") |>
      select(inc_dom_prop_cat) |>
      group_by(inc_dom_prop_cat) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(ANO = 2010,
             responsavel = "total") |> 
      select(ANO, responsavel, everything())
  ) |> 
  arrange(ANO, responsavel)

gc()

# Agregado

t_8 <- t_8_1970 |> 
  bind_rows(t_8_1980) |> 
  bind_rows(t_8_1991) |> 
  bind_rows(t_8_2000) |> 
  bind_rows(t_8_2010)

# Analises ----------------------------------------------------------------

## 1 - Numero absoluto/relativo de pessoas responsaveis ao longo do tempo

## 2 - Por grupo etário

# Responsaveis em relacao a pop. total

t_2 |> 
  filter(!is.na(grupo_etario) & grupo_etario >= 10) |>
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = grupo_etario, y = prop, color = ANO,
      group=interaction(ANO, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = prop-prop_se, ymax = prop+prop_se)) +
  labs(
    x = "Grupo etário quinquenal",
    y = "Distribuição relativa (%)"
  ) +
  theme_light() +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,max(t_2$prop),2)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

graf_prop_idade <- t2 |> 
  filter(!is.na(grupo_etario) & grupo_etario >= 10) |>
  filter(responsavel == "responsavel") |> 
  mutate(grupo_etario = case_when(grupo_etario >= 80 ~ 80, TRUE ~ grupo_etario)) %>%
  ungroup() %>% 
  reframe(n = sum(n), .by = c(ANO, responsavel, grupo_etario)) %>%
  summarise(grupo_etario = grupo_etario,
            prop = round(n/sum(n)*100,2), .by = c(ANO,responsavel)) %>% 
  ggplot() +
  aes(x = grupo_etario, y = prop, color = ANO,
      group=interaction(ANO, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  labs(
    title = "(i)",
    x = "Grupo etário quinquenal",
    y = "Distribuição relativa (%)"
  ) +
  theme_light() +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1960,1970, 1980, 1991, 2000, 2010)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,max(t2$prop),2)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

## 3 - Por sexo

# Responsaveis e pop. total

t_3 |>
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = female) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,100)) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "BuPu") +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
  guides(fill = guide_legend(title = "Sexo")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t_3 |> 
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = female) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,100)) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "BuPu") +
  guides(fill = guide_legend(title = "Sexo")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 4 - Por status marital

# Responsaveis e pop. total

t4 |> 
  filter(responsavel != "nao_responsavel") |> 
  ungroup() %>% 
  mutate(responsavel = case_when(
    responsavel == "responsavel" ~ "Pop. Responsável",
    TRUE ~ "Pop. Total"
  )) %>% 
  ggplot() +
  aes(x = ANO, y = prop, fill = status_marital) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Tipo de União")) +
  theme(
    strip.text = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

# Responsaveis

t4 |> 
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = status_marital) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)"
  ) +
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

t_5 |> 
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = educ_atingida) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
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

graf_prop_escolaridade <- t5 |> 
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = educ_atingida) +
  geom_col(position = "dodge", show.legend = F) +
  labs(
    title = "(i)",
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  theme_light() +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Nível de escolaridade atingido")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

## 6 - Por condição na força de trabalho

# Responsaveis e pop. total

t_6 |> 
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = status_ocupacional) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
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

t_6 |> 
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = status_ocupacional) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
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

t_7 <- t_7 |> 
  mutate(inc_decil =
           factor(inc_decil,
                  levels = c(1,2,3,4,5),
                  labels = c("P20","P40","P60","P80", "P100")))

t_7 |>
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_decil) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
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

t_7 |>
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_decil) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
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

t_8 |>
  filter(responsavel != "nao_responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_dom_prop_cat) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  lemon::facet_rep_grid(. ~ responsavel, repeat.tick.labels = TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Contribuição na renda dom. total (%)")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Responsaveis

t_8 |>
  filter(responsavel == "responsavel") |> 
  ggplot() +
  aes(x = ANO, y = prop, fill = inc_dom_prop_cat) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Distribuição relativa (%)") +
  theme_light() +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Contribuição na renda dom. total (%)")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 8.2 - Por parcela de renda no domicílio (continua)

# Responsaveis e pop. total

ggplot() +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_1970 |> as.data.frame() |> filter(responsavel == 1),
    fill = "#b2182b", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_1980 |> as.data.frame() |> filter(responsavel == 1),
    fill = "#ef8a62", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_1991 |> as.data.frame() |> filter(responsavel == 1),
    fill = "#fddbc7", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_2000 |> as.data.frame() |> filter(responsavel == 1),
    fill = "#67a9cf", alpha = 0.5
  ) +
  ggridges::geom_density_ridges2(
    aes(x = inc_dom_prop, y = as.factor(responsavel)),
    data = df_2010 |> as.data.frame() |> filter(responsavel == 1),
    fill = "#2166ac", alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(-1,1.2,.2)) +
  labs(
    x = "Contribuição da renda do responsável na renda domiciliar total",
    y = "Densidade da distribuição") +
  theme_light() +
  theme(
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Saving results ----------------------------------------------------------

saveRDS(t_1, file = "./output/pt1/t_1.rds")
saveRDS(t_2, file = "./output/pt1/t_2.rds")
saveRDS(t_3, file = "./output/pt1/t_3.rds")
saveRDS(t_4, file = "./output/pt1/t_4.rds")
saveRDS(t_5, file = "./output/pt1/t_5.rds")
saveRDS(t_6, file = "./output/pt1/t_6.rds")
saveRDS(t_7, file = "./output/pt1/t_7.rds")
saveRDS(t_8, file = "./output/pt1/t_8.rds")
