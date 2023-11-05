options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Importando materiais ----------------------------------------------------

t1 <- readRDS("./output/pt1/t_1.rds")
t2 <- readRDS("./output/pt1/t_2.rds")
t3 <- readRDS("./output/pt1/t_3.rds")
t4 <- readRDS("./output/pt1/t_4.rds")
t5 <- readRDS("./output/pt1/t_5.rds")
t6 <- readRDS("./output/pt1/t_6.rds")
t7 <- readRDS("./output/pt1/t_7.rds")
t8 <- readRDS("./output/pt1/t_8.rds")

## 1 - Taxas de chefia ao longo do tempo ---------------------------------

tx_t1 <- t1 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

## 2 - Taxa de chefia por grupo etário -----------------------------------

tx_t2 <- t2 |> 
  ungroup() |> 
  filter(grupo_etario >= 10) |>
  filter(responsavel != "nao_responsavel") |> 
  select(-c(n_se,prop,prop_se)) |> 
  mutate(grupo_etario = if_else(grupo_etario >= 80, 80, grupo_etario)) |> 
  summarise(n = sum(n), .by = c(ANO, responsavel, grupo_etario)) |>
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por grupo etario

grafico_terd_idade <- tx_t2 |> 
  filter(grupo_etario >= 10) |> 
  ggplot() +
  aes(x = grupo_etario, y = round(tx*100,2), color = ANO,
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
graf_prop_idade + grafico_terd_idade
# Taxa por grupo etario relativa

tx_t2 |>
  filter(grupo_etario >= 10) |> 
  filter(!is.na(tx)) |> 
  group_by(ANO) |> 
  mutate(tx_total = sum(tx),
         tx_rel = (tx/tx_total)*100) |> 
  ggplot() +
  aes(x = grupo_etario, y = round(tx_rel,4), color = ANO,
      group=interaction(ANO, ANO)) +
  geom_line(linewidth= 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Grupo etário quinquenal",
    y = "Taxa de responsabilidade domiciliar relativa"
  ) +
  theme_light() +
  scale_colour_gradient(guide = "legend", low = "#d9d9d9", high = "#252525",
                        breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  guides(color = guide_legend(title = "Ano calendário")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 3 - Taxas de chefia por sexo ------------------------------------------

tx_t3 <- t3 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por sexo

tx_t3 |> 
  ggplot() +
  aes(x = ANO, y = round(tx*100,2), fill = female,
      group=interaction(female, female)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar (%)"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,50)) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "BuPu") +
  guides(fill = guide_legend(title = "Sexo")) +
  theme(
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )


## 4 - Taxas de chefia por status marital --------------------------------

tx_t4 <- t4 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por status marital
tx_t4 |> 
  ggplot() +
  aes(x = ANO, y = round(tx,4), fill = status_marital,
      group=interaction(status_marital, status_marital)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,1,.1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Status marital")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

## 5 - Taxas de chefia por escolaridade atingida -------------------------

tx_t5 <- t5 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por escolaridade atingida
graf_terd_por_escolaridade <- tx_t5 |> 
  ggplot() +
  aes(x = ANO, y = round(tx*100,2), fill = educ_atingida,
      group=interaction(educ_atingida, educ_atingida)) +
  geom_col(position = "dodge") +
  labs(
    title = "(ii)",
    x = "Ano calendário",
    y = "Taxa de responsabilidade domiciliar (%)"
  ) +
  theme_light() +
  coord_cartesian(ylim = c(0,100)) +
  scale_x_continuous(breaks = c(1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Nível de escolaridade atingido")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.text = element_text(size = 10, hjust = .0, vjust = .5),
    legend.position = "right",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )

graf_prop_escolaridade + graf_terd_por_escolaridade
## 6 - Taxa de chefia por condição na força de trabalho ------------------

tx_t6 <- t6 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por condição na força de trabalho
tx_t6 |> 
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

tx_t7 <- t7 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por quintil de renda
tx_t7 |> 
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

tx_t8 <- t8 |> 
  select(-c(n_se,prop,prop_se)) |> 
  pivot_wider(names_from = "responsavel", values_from = "n") |> 
  mutate(tx = responsavel/total)

# Taxa por contribuição na renda domiciliar
tx_t8 |> 
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
  scale_x_continuous(breaks = c(1960,1970, 1980, 1991, 2000, 2010)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  guides(fill = guide_legend(title = "Contribuição na renda domiciliar total")) +
  theme(
    legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
    legend.text = element_text(size = 8, hjust = .5, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
  )

# Saving results ----------------------------------------------------------

saveRDS(tx_t1, file = "./output/pt2/tx_t1.rds")
saveRDS(tx_t2, file = "./output/pt2/tx_t2.rds")
saveRDS(tx_t3, file = "./output/pt2/tx_t3.rds")
saveRDS(tx_t4, file = "./output/pt2/tx_t4.rds")
saveRDS(tx_t5, file = "./output/pt2/tx_t5.rds")
saveRDS(tx_t6, file = "./output/pt2/tx_t6.rds")
saveRDS(tx_t7, file = "./output/pt2/tx_t7.rds")
saveRDS(tx_t8, file = "./output/pt2/tx_t8.rds")
