options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Importando materiais ----------------------------------------------------

t2_by_sex <- readRDS("./output/pt1/t_2_by_sex.rds")
t4_by_sex <- readRDS("./output/pt1/t_4_by_sex.rds")
t5_by_sex <- readRDS("./output/pt1/t_5_by_sex.rds")
t6_by_sex <- readRDS("./output/pt1/t_6_by_sex.rds")
t7_by_sex <- readRDS("./output/pt1/t_7_by_sex.rds")
t8_by_sex <- readRDS("./output/pt1/t_8_by_sex.rds")

## 2 - Taxa de chefia por grupo etário -----------------------------------

tx_t2_by_sex <- t2_by_sex |> 
  ungroup() |> 
  filter(grupo_etario >= 10) |>
  filter(responsavel != "nao_responsavel") |> 
  select(-c(n_se,prop,prop_se)) |> 
  mutate(grupo_etario = if_else(grupo_etario >= 80, 80, grupo_etario)) |> 
  summarise(n = sum(n), .by = c(ANO, responsavel, female, grupo_etario)) |>
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por grupo etario

tx_t2_by_sex |> 
  filter(grupo_etario >= 10) |> 
  ggplot() +
  aes(x = grupo_etario, y = round(tx*100,1), color = ANO,
      group=interaction(ANO, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  labs(
    title = "(ii)",
    x = "Grupo etário quinquenal",
    y = "Taxa de responsabilidade domiciliar (%)"
  ) +
  theme_light() +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1970, 1980, 1991, 2000, 2010)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
  coord_cartesian(ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

# Taxa por grupo etario relativa

tx_t2_by_sex |>
  filter(grupo_etario >= 10) |> 
  filter(!is.na(tx)) |> 
  group_by(ANO, female) |> 
  mutate(tx_total = sum(tx)) |> 
  ungroup() |> 
  mutate(tx_rel = (tx/tx_total)*100) |>
  ggplot() +
  aes(x = grupo_etario, y = tx_rel, color = ANO,
      group=interaction(ANO, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  labs(
    title = "(iii)",
    x = "Grupo etário quinquenal",
    y = "Taxa de responsabilidade domiciliar relativa (%)"
  ) +
  theme_light() +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1970, 1980, 1991, 2000, 2010)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,20,2)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

## 4 - Taxas de chefia por status marital --------------------------------

tx_t4_by_sex <- t4_by_sex |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por status marital
tx_t4_by_sex |> 
  ggplot() +
  aes(x = ANO, y = round(tx*100,1), fill = status_marital,
      group=interaction(status_marital, status_marital)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar (%)"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,100)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
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

## 5 - Taxas de chefia por escolaridade atingida -------------------------

tx_t5_by_sex <- t5_by_sex |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por escolaridade atingida
tx_t5_by_sex |> 
  ggplot() +
  aes(x = ANO, y = round(tx,4), fill = educ_atingida,
      group=interaction(educ_atingida, educ_atingida)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,1)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Nível de escolaridade atingido")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 6 - Taxa de chefia por condição na força de trabalho ------------------

tx_t6_by_sex <- t6_by_sex |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por condição na força de trabalho
tx_t6_by_sex |> 
  ggplot() +
  aes(x = ANO, y = round(tx,4), fill = status_ocupacional,
      group=interaction(status_ocupacional, status_ocupacional)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,1)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Condição de ocupação")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 7 - Por quintil de renda ----------------------------------------------

tx_t7_by_sex <- t7_by_sex |> 
  select(-c(n_se,prop,prop_se)) |> 
  mutate(inc_decil = factor(inc_decil, levels = c(1,2,3,4,5),
                            labels = c("P20","P40","P60","P80","P100"))) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por quintil de renda
tx_t7_by_sex |> 
  ggplot() +
  aes(x = ANO, y = round(tx,4), fill = inc_decil,
      group=interaction(inc_decil, inc_decil)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,1)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Quintil de renda domiciliar per capita")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 8 - Por parcela na renda domiciliar ----------------------------------------

tx_t8_by_sex <- t8_by_sex |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por contribuição na renda domiciliar
tx_t8_by_sex |> 
  ggplot() +
  aes(x = ANO, y = round(tx,4), fill = inc_dom_prop_cat,
      group=interaction(inc_dom_prop_cat, inc_dom_prop_cat)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,1)) +
  lemon::facet_rep_wrap(. ~ female, repeat.tick.labels = TRUE) +
  scale_x_continuous(breaks = c(1960,1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Contribuição na renda domiciliar")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Saving results ----------------------------------------------------------

saveRDS(tx_t2_by_sex, file = "./output/pt2/tx_t2_by_sex.rds")
saveRDS(tx_t4_by_sex, file = "./output/pt2/tx_t4_by_sex.rds")
saveRDS(tx_t5_by_sex, file = "./output/pt2/tx_t5_by_sex.rds")
saveRDS(tx_t6_by_sex, file = "./output/pt2/tx_t6_by_sex.rds")
saveRDS(tx_t7_by_sex, file = "./output/pt2/tx_t7_by_sex.rds")
saveRDS(tx_t8_by_sex, file = "./output/pt2/tx_t8_by_sex.rds")
