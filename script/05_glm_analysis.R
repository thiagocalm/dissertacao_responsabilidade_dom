options(scipen = 999999)
gc()


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(lmtest)

# Model 1 - Demographic factors -----------------------------------------------

####
## Import data
####

# 1970
load("./output/pt3/model_output_70.RData", verbose = TRUE)

m1_70 <- model_output_70[[1]] # Selecting model

rm(model_output_70)
gc()

# 1980
load("./output/pt3/model_output_80.RData", verbose = TRUE)

m1_80 <- model_output_80[[1]] # Selecting model

rm(model_output_80)
gc()

# 1991
load("./output/pt3/model_output_91.RData", verbose = TRUE)

m1_91 <- model_output_91[[1]] # Selecting model

rm(model_output_91)
gc()

# 2000
load("./output/pt3/model_output_00.RData", verbose = TRUE)

m1_00 <- model_output_00[[1]] # Selecting model

rm(model_output_00)
gc()

# 2010
load("./output/pt3/model_output_10.RData", verbose = TRUE)

m1_10 <- model_output_10[[1]] # Selecting model

rm(model_output_10)
gc()

####
## Summarying models
####

summary(m1_70)
summary(m1_80)
summary(m1_91)
summary(m1_00)
summary(m1_10)

m1_summarys_results <- stargazer::stargazer(m1_70, m1_80, m1_91, m1_00, m1_10,
                                            intercept.top = TRUE,
                                            intercept.bottom = FALSE,
                                            apply.coef = exp,
                                            dep.var.labels=c("Probabilidade de ser responsável"),
                                            omit.stat=c("ser"),
                                            ci=TRUE,
                                            ci.level=0.95,
                                            type = "text", #na hora de rodar para entregar, colocar "latex"!
                                            title = "Odds Ratio (OR) Modelo 1 (controlando-se por fatores demográficos)",
                                            align = TRUE)


m1 <- summary(m1_70)$coefficients[,c(1,4)] |>
  as_tibble() |> 
  mutate(ano = 1970,
         coeficientes = Estimate,
         p_value = round(`Pr(>|t|)`,5),
         variaveis = row.names(summary(m1_70)$coefficients[,0]),
         se_95 = summary(m1_70)$coefficients[,2]*1.96) |> 
  select(-c(Estimate,`Pr(>|t|)`)) |> 
  select(ano, variaveis, coeficientes, p_value, se_95) |> 
  # Inserindo 1980
  bind_rows(
    summary(m1_80)$coefficients[,c(1,4)] |>
      as.tibble() |> 
      mutate(ano = 1980,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m1_80)$coefficients[,0]),
             se_95 = summary(m1_80)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 1991
  bind_rows(
    summary(m1_91)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 1991,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m1_91)$coefficients[,0]),
             se_95 = summary(m1_91)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 2000
  bind_rows(
    summary(m1_00)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 2000,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m1_00)$coefficients[,0]),
             se_95 = summary(m1_00)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 2010
  bind_rows(
    summary(m1_10)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 2010,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m1_10)$coefficients[,0]),
             se_95 = summary(m1_10)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  mutate(
    coeficientes = round(coeficientes, 5),
    se_95 = round(se_95, 4),
    coeficientes_OR = round(exp(coeficientes),5),
    OR_confit = paste0("[",round(coeficientes_OR-se_95,4),"; ",round(coeficientes_OR+se_95,4),"]")
  ) |> 
  select(ano, variaveis, coeficientes, se_95, coeficientes_OR, OR_confit, p_value)

rm(m1_70, m1_80, m1_91, m1_00, m1_10)
gc()

# Model 2 - Demographic and socioeconomic factors -----------------------------------------------

####
## Import data
####

# 1970
load("./output/pt3/model_output_70.RData", verbose = TRUE)

m2_70 <- model_output_70[[2]] # Selecting model

rm(model_output_70)
gc()

# 1980
load("./output/pt3/model_output_80.RData", verbose = TRUE)

m2_80 <- model_output_80[[2]] # Selecting model

rm(model_output_80)
gc()

# 1991
load("./output/pt3/model_output_91.RData", verbose = TRUE)

m2_91 <- model_output_91[[2]] # Selecting model

rm(model_output_91)
gc()

# 2000
load("./output/pt3/model_output_00.RData", verbose = TRUE)

m2_00 <- model_output_00[[2]] # Selecting model

rm(model_output_00)
gc()

# 2010
load("./output/pt3/model_output_10.RData", verbose = TRUE)

m2_10 <- model_output_10[[2]] # Selecting model

rm(model_output_10)
gc()

####
## Summarying models
####

summary(m2_70)
summary(m2_80)
summary(m2_91)
summary(m2_00)
summary(m2_10)

m2_summarys_results <- stargazer::stargazer(m2_70, m2_80, m2_91, m2_00, m2_10,
                                            intercept.top = TRUE,
                                            intercept.bottom = FALSE,
                                            apply.coef = exp,
                                            dep.var.labels=c("Probabilidade de ser responsável"),
                                            omit.stat=c("ser"),
                                            ci=TRUE,
                                            ci.level=0.95,
                                            type = "text", #na hora de rodar para entregar, colocar "latex"!
                                            title = "Odds Ratio (OR) Modelo 2 (controlando-se por fatores demográficos e socioeconômicos)",
                                            align = TRUE)


m2 <- summary(m2_70)$coefficients[,c(1,4)] |>
  as_tibble() |> 
  mutate(ano = 1970,
         coeficientes = Estimate,
         p_value = round(`Pr(>|t|)`,5),
         variaveis = row.names(summary(m2_70)$coefficients[,0]),
         se_95 = summary(m2_70)$coefficients[,2]*1.96) |> 
  select(-c(Estimate,`Pr(>|t|)`)) |> 
  select(ano, variaveis, coeficientes, p_value, se_95) |> 
  # Inserindo 1980
  bind_rows(
    summary(m2_80)$coefficients[,c(1,4)] |>
      as.tibble() |> 
      mutate(ano = 1980,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m2_80)$coefficients[,0]),
             se_95 = summary(m2_80)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 1991
  bind_rows(
    summary(m2_91)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 1991,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m2_91)$coefficients[,0]),
             se_95 = summary(m2_91)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 2000
  bind_rows(
    summary(m2_00)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 2000,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m2_00)$coefficients[,0]),
             se_95 = summary(m2_00)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 2010
  bind_rows(
    summary(m2_10)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 2010,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m2_10)$coefficients[,0]),
             se_95 = summary(m2_10)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  mutate(
    coeficientes = round(coeficientes, 5),
    se_95 = round(se_95, 4),
    coeficientes_OR = round(exp(coeficientes),5),
    OR_confit = paste0("[",round(coeficientes_OR-se_95,4),"; ",round(coeficientes_OR+se_95,4),"]")
  ) |> 
  select(ano, variaveis, coeficientes, se_95, coeficientes_OR, OR_confit, p_value)

rm(m2_70, m2_80, m2_91, m2_00, m2_10)
gc()

# Model 3 - Demographic, socioeconomic and household factors ------------------

####
## Import data
####

# 1970
load("./output/pt3/model_output_70.RData", verbose = TRUE)

m3_70 <- model_output_70[[3]] # Selecting model

rm(model_output_70)
gc()

# 1980
load("./output/pt3/model_output_80.RData", verbose = TRUE)

m3_80 <- model_output_80[[3]] # Selecting model

rm(model_output_80)
gc()

# 1991
load("./output/pt3/model_output_91.RData", verbose = TRUE)

m3_91 <- model_output_91[[3]] # Selecting model

rm(model_output_91)
gc()

# 2000
load("./output/pt3/model_output_00.RData", verbose = TRUE)

m3_00 <- model_output_00[[3]] # Selecting model

rm(model_output_00)
gc()

# 2010
load("./output/pt3/model_output_10.RData", verbose = TRUE)

m3_10 <- model_output_10[[3]] # Selecting model

rm(model_output_10)
gc()

####
## Summarying models
####

summary(m3_70)
summary(m3_80)
summary(m3_91)
summary(m3_00)
summary(m3_10)

m3_summarys_results <- stargazer::stargazer(m3_70, m3_80, m3_91, m3_00, m3_10,
                                            intercept.top = TRUE,
                                            intercept.bottom = FALSE,
                                            apply.coef = exp,
                                            dep.var.labels=c("Probabilidade de ser responsável"),
                                            omit.stat=c("ser"),
                                            ci=TRUE,
                                            ci.level=0.95,
                                            type = "text", #na hora de rodar para entregar, colocar "latex"!
                                            title = "Odds Ratio (OR) Modelo 3 (controlando-se por fatores demográficos, socioeconômicos e domiciliares)",
                                            align = TRUE)


m3 <- summary(m3_70)$coefficients[,c(1,4)] |>
  as_tibble() |> 
  mutate(ano = 1970,
         coeficientes = Estimate,
         p_value = round(`Pr(>|t|)`,5),
         variaveis = row.names(summary(m3_70)$coefficients[,0]),
         se_95 = summary(m3_70)$coefficients[,2]*1.96) |> 
  select(-c(Estimate,`Pr(>|t|)`)) |> 
  select(ano, variaveis, coeficientes, p_value, se_95) |> 
  # Inserindo 1980
  bind_rows(
    summary(m3_80)$coefficients[,c(1,4)] |>
      as.tibble() |> 
      mutate(ano = 1980,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m3_80)$coefficients[,0]),
             se_95 = summary(m3_80)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 1991
  bind_rows(
    summary(m3_91)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 1991,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m3_91)$coefficients[,0]),
             se_95 = summary(m3_91)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 2000
  bind_rows(
    summary(m3_00)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 2000,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m3_00)$coefficients[,0]),
             se_95 = summary(m3_00)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  # Inserindo 2010
  bind_rows(
    summary(m3_10)$coefficients[,c(1,4)] |>
      as_tibble() |> 
      mutate(ano = 2010,
             coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary(m3_10)$coefficients[,0]),
             se_95 = summary(m3_10)$coefficients[,2]*1.96) |> 
      select(-c(Estimate,`Pr(>|t|)`)) |> 
      select(ano, variaveis, coeficientes, p_value, se_95)
  ) |> 
  mutate(
    coeficientes = round(coeficientes, 5),
    se_95 = round(se_95, 4),
    coeficientes_OR = round(exp(coeficientes),5),
    OR_confit = paste0("[",round(coeficientes_OR-se_95,4),"; ",round(coeficientes_OR+se_95,4),"]")
  ) |> 
  select(ano, variaveis, coeficientes, se_95, coeficientes_OR, OR_confit, p_value)

rm(m3_70, m3_80, m3_91, m3_00, m3_10)
gc()


# Grafics by variable -----------------------------------------------------

### Idade

m3 |> 
  filter(variaveis == "grupo_etario") |>
  ggplot() +
  aes(x = ano, y = coeficientes_OR) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#636363") +
  geom_pointrange(aes(ymin = coeficientes_OR-se_95, ymax = coeficientes_OR+se_95)) +
  labs(
    title = "Razão de Chance (OR) de ser Responsável pelo domicílio por idade - 1070 a 2010",
    x = "Ano",
    y = "Idade (Odds Ratio)"
  ) +
  coord_cartesian(ylim = c(0.8,1.2)) +
  theme_light() +
  scale_y_continuous(breaks = seq(0.5,1.5,.1)) +
  # scale_color_brewer(palette = "Set1") +
  # guides(color = guide_legend(title = "Sexo")) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = .0),
    # legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    # legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    # legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

### Sexo

m3 |> 
  filter(variaveis == "female") |>
  ggplot() +
  aes(x = ano, y = coeficientes_OR,
      interaction(ano,ano)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#636363") +
  geom_line(color = "#636363") +
  geom_pointrange(aes(ymin = coeficientes_OR-se_95, ymax = coeficientes_OR+se_95)) +
  labs(
    title = "Razão de Chance (OR) de ser Responsável pelo domicílio por sexo - 1070 a 2010",
    x = "Ano",
    y = "Ser do sexo feminino (Odds Ratio)"
  ) +
  coord_cartesian(ylim = c(0.0,1.1)) +
  theme_light() +
  scale_y_continuous(breaks = seq(0.0,1.5,.1)) +
  # scale_color_brewer(palette = "Set1") +
  # guides(color = guide_legend(title = "Sexo")) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = .0),
    # legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    # legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    # legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

### Escolaridade

m3 |> 
  filter(variaveis == "educ_atingida_Fundamental.incompleto" |
           variaveis == "educ_atingida_Fundamental.completo" |
           variaveis == "educ_atingida_Médio.completo" |
           variaveis == "educ_atingida_Superior.completo") |>
  ggplot() +
  aes(x = ano, y = coeficientes_OR, group = variaveis, color = variaveis,
      interaction(ano,ano)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#636363") +
  geom_line(color = "#636363") +
  geom_pointrange(aes(ymin = coeficientes_OR-se_95, ymax = coeficientes_OR+se_95), size = 1) +
  labs(
    title = "Razão de Chance (OR) de ser Responsável pelo domicílio por sexo - 1070 a 2010",
    x = "Ano",
    y = "Ser do sexo feminino (Odds Ratio)"
  ) +
  coord_cartesian(ylim = c(0.5,2)) +
  theme_light() +
  scale_y_continuous(breaks = seq(0.0,2,.2)) +
  scale_x_continuous(breaks = c(1970,1980,1991,2000,2010)) +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Escolaridade atingida")) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = .0),
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )
### CONTINUAR FAZENDO ESSES GRAFICOS PARA AS DEMAIS VARIAVEIS! GRAFICOS TÊM QUE MELHORAR
# Saving model's data -----------------------------------------------------

models_summaries <- list(m1,m2,m3)

save(models_summaries, file = "./output/pt3/models_summaries.RData")

library(openxlsx)

wb <- createWorkbook()

modifyBaseFont(wb, fontSize = "11", fontName = "Calibri")

addWorksheet(wb, sheetName = "ReadMe")
addWorksheet(wb, sheetName = "2 - Por dem")
addWorksheet(wb, sheetName = "3 - Por dem, socioec")
addWorksheet(wb, sheetName = "4 - Por dem, socioec, dom")

writeDataTable(wb, sheet = 2, x = m1, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 3, x = m2, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 4, x = m3, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb, "./output/pt3/resumos_modelos_prob_responsavel.xlsx", overwrite = TRUE)