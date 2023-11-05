options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

load("./output/pt4/resultados_decomposicao.RData")

# Overall -----------------------------------------------------------------

dec_overall <- df_dec$overall

# diff_total <- dec_overall |> filter(componentes == "diferença total")
# 
# dec_overall |>
#   # filter(componentes != "diferença total") |>
#   ggplot() +
#   aes(x = periodo, y = value, color = componentes) +
#   # geom_col() +
#   # geom_point(data = diff_total, aes(x = periodo, y = value),
#   #            stat='identity', fill="black", size=13, shape=16) +
#   # geom_segment(data = diff_total,
#   #              aes(y = 0,
#   #                  x = periodo,
#   #                  yend = value,
#   #                  xend = periodo),
#   #              color = "black") +
#   geom_point(size=13) +   # Draw points
#   geom_segment(aes(x=periodo,
#                    xend=periodo,
#                    y=min(value),
#                    yend=max(value)),
#                linetype="dashed",
#                size=0.1) +
#   geom_text(data = diff_total, aes(x = periodo, label = round(value,4)),color = "white", size = 2.2) +
#   scale_x_discrete(limits = rev(levels(dec_overall$periodo))) +
#   scale_y_continuous(breaks = seq(-.04,.04,0.005),) +
#   coord_flip() +
#   labs(
#     title = "Resultado agregado da decomposição da diferença entre períodos, segundo cada efeito",
#     subtitle = "Brasil, 1970 - 2010",
#     x = "Períodos considerados para a decomposição da diferença",
#     y = "Efeito (%) da decomposição da diferença"
#   ) +
#   theme_light() +
#   scale_color_manual(values = c("tomato2","#fdae61","#35978f")) +
#   guides(fill = guide_legend(title = "Efeitos")) +
#   theme(
#     plot.title = element_text(face = "bold", size = 12, hjust = 0),
#     plot.subtitle = element_text(face = "bold", size = 12, hjust = .1),
#     legend.title = element_text(face = "bold", size = 8, hjust = .5, vjust = .5),
#     legend.text = element_text(size = 8, hjust = .5, vjust = .5),
#     legend.position = "bottom",
#     axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
#     axis.text = element_text(size = 8, hjust = .5, vjust = .5)
#   )

dec_overall |>
  ggplot() +
  aes(x = periodo, y = value, color = componentes) +
  geom_hline(yintercept = 0,linetype = "dashed", color = "black", linewidth = 1) +
  geom_point(size=7, shape = 16) +
  # geom_text(aes(x = periodo, label = round(value,4)),color = "black", size = 2.2) +
  scale_x_discrete(limits = rev(levels(dec_overall$periodo))) +
  scale_y_continuous(breaks = seq(-.04,.04,0.005),) +
  coord_flip() +
  labs(
    title = "Decomposição da diferença entre períodos na probabilidade condicional de ser responsável pelo domicílio - Brasil, 1970-2010.",
    x = "Períodos considerados para a decomposição \nda diferença",
    y = "Diferença na probabilidade condicional de ser responsável pelo domicílio"
  ) +
  theme_light() +
  scale_color_manual(values = c("#35978f", "#fdae61", "#737373"),
                     labels=c('Efeito Composição', 'Efeito Taxa', 'Diferença Total')) +
  guides(color = guide_legend(title = "Efeitos")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    legend.title = element_text(face = "bold", size = 10, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "right",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )
  

# Factors -----------------------------------------------------------------

dec_factors <- df_dec$factors

dec_factors <- dec_factors |> 
  pivot_longer(cols = atributo:coeficiente_prop, names_to = "efeito", values_to = "value")

fatores <- c("Não observados", "Demográfico", "Socioeconômico", "Domiciliar")

dec_factors |>
  filter(efeito != "atributo_prop") |>
  filter(efeito != "coeficiente_prop") |> 
  mutate(efeito = as.factor(efeito)) |> 
  ggplot() +
  aes(x = fatores, y = value, color = efeito) +
  geom_hline(yintercept = 0,linetype = "dashed", color = "black", linewidth = 1) +
  geom_point(size=4, shape = 16) +
  scale_y_continuous(breaks = seq(-.04,.04,0.005)) +
  coord_flip() + 
  labs(
    title = "Decomposição da diferença entre períodos na probabilidade condicional de ser responsável pelo domicílio - Brasil, 1970-2010.",
    x = "Fatores decompostos",
    y = "Diferença na probabilidade condicional de ser responsável pelo domicílio"
  ) +
  theme_light() +
  scale_color_manual(values = c("#35978f", "#fdae61"),
                     labels=c('Efeito Composição', 'Efeito Taxa')) +
  lemon::facet_rep_grid(periodo ~ ., repeat.tick.labels = TRUE) +
  guides(color = guide_legend(title = "Efeitos")) +
  theme(
    strip.text = element_text(face = "bold", size = 9, vjust = .5), 
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 10, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "right",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

# Detailed -----------------------------------------------------------------

dec_detailed <- df_dec$detailed

patterns <- c("educ_atingida_|inc_decil_|status_ocupacional_|status_marital_|inc_dom_prop_cat_")

labs <- c("Não observado", "Grupo etário", "Sexo: Feminino", "Esc.: Fund. Incompleto",
          "Esc.: Fund. Completo","Esc.: Médio Completo", "Esc.: Superior Completo","Estrato Renda: P40",
          "Estrato Renda: P60","Estrato Renda: P80","Estrato Renda: P100","Cond. ocupacação: Ativo",
          "Renda rel.: Intermediaria","Renda rel.: Maior","Tipo união: Casada","Tipo união: União Consensual",
          "Tipo união: Separada/Divorciada","Tipo união: Viúva")

dec_detailed <- dec_detailed |>
  pivot_longer(cols = atributo:coeficiente_prop, names_to = "efeito", values_to = "value") |>
  mutate(efeito = as.factor(efeito),
         variables = stringr::str_replace_all(variables, patterns, ""),
         variables = stringr::str_replace_all(variables, c("\\.|\\_"), " "),
         variables = stringr::str_to_lower(variables),
         variables = stringr::str_to_title(variables)) %>% 
  mutate(variables = as_factor(case_when(
    variables == "(Intercept)" ~ "Não observado",
    variables == "Grupo Etario" ~ "Grupo etário",
    variables == "Female" ~ "Sexo: Feminino",
    variables == "Fundamental Incompleto" ~ "Esc.: Fund. Incompleto",
    variables == "Fundamental Completo" ~ "Esc.: Fund. Completo",
    variables == "Médio Completo" ~ "Esc.: Med. Completo",
    variables == "Superior Completo" ~ "Esc.: Sup. Completo",
    variables == "P40" ~ "Estrato Renda: P40",
    variables == "P60" ~ "Estrato Renda: P60",
    variables == "P80" ~ "Estrato Renda: P80",
    variables == "P100" ~ "Estrato Renda: P100",
    variables == "Ativo" ~ "Cond. ocupacação: Ativo",
    variables == "Intermediaria" ~ "Renda rel.: Intermediaria",
    variables == "Maior" ~ "Renda rel.: Maior",
    variables == "Casadas" ~ "Tipo união: Casada",
    variables == "União Consensual" ~ "Tipo união: União Consensual",
    variables == "Separadas Divorciadas" ~ "Tipo união: Separada/Divorciada",
    variables == "Viúvas" ~ "Tipo união: Viúva")
  ))

dec_detailed |> 
  filter(efeito != "atributo_prop") |>
  filter(efeito != "coeficiente_prop") |> 
  mutate(variables = fct_reorder(variables, desc(variables))) |>
  ggplot() +
  aes(x = variables, y = value, color = efeito) +
  geom_hline(yintercept = 0,linetype = "dashed", color = "black", linewidth = 1) +
  geom_point(size=4, shape = 16) +
  scale_y_continuous(breaks = seq(-.04,.04,0.01)) +
  coord_flip() + 
  labs(
    x = "Variáveis (características) decompostas",
    y = "Diferença na probabilidade condicional de ser identificado como responsável pelo domicílio"
  ) +
  theme_light() +
  scale_color_manual(values = c("#1b9e77", "#fe9929"),
                     labels=c('Efeito Composição', 'Efeito Propensão')) +
  facet_grid(. ~ periodo) +
  guides(color = guide_legend(title = "Efeitos")) +
  theme(
    strip.text = element_text(face = "bold", size = 10, vjust = .5), 
    legend.title = element_text(face = "bold", size = 12, hjust = 0, vjust = .5),
    legend.text = element_text(size = 10, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(size = 10, hjust = .5, vjust = .5)
  )


# Simulacao de cenarios ---------------------------------------------------

# import valores preditos
load("./output/pt4/testes_estatisticos_decomp_ob.RData")

# criando dataframe com valores preditos

preditos <- tibble(
  ano = c(1970,1980,1991,2000,2010),
  prob = c(0.26679, 0.30007, 0.30432, 0.32433, 0.35081))

## Simulacao 1 - sem efeito composicao

# Cenario 1 - sem efeito de composicao
cenario1 <- dec_overall |> filter(componentes == "efeito atributo") |> select(value) |> as.vector()

# Cenario 2 - sem efeito de composicao demografico
cenario2 <- dec_factors |> filter(fatores == "Demográfico" & efeito == "atributo") |> ungroup() |> select(value)

# Cenario 3 - sem efeito de composicao socioecomico
cenario3 <- dec_factors |> filter(fatores == "Socioeconômico" & efeito == "atributo") |> ungroup() |> select(value)

# Cenario 4 - sem efeito de composicao Domiciliar
cenario4 <- dec_factors |> filter(fatores == "Domiciliar" & efeito == "atributo") |> ungroup() |> select(value)

preditos_cenario_total <- preditos |> 
  mutate(
    cenario0 = prob,
    cenario1 = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario1[[1]][1],
      ano == 1991 ~ prob - cenario1[[1]][2],
      ano == 2000 ~ prob - cenario1[[1]][3],
      ano == 2010 ~ prob - cenario1[[1]][4]),
    cenario2 = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario2[[1]][1],
      ano == 1991 ~ prob - cenario2[[1]][2],
      ano == 2000 ~ prob - cenario2[[1]][3],
      ano == 2010 ~ prob - cenario2[[1]][4]),
    cenario3 = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario3[[1]][1],
      ano == 1991 ~ prob - cenario3[[1]][2],
      ano == 2000 ~ prob - cenario3[[1]][3],
      ano == 2010 ~ prob - cenario3[[1]][4]),
    cenario4 = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario4[[1]][1],
      ano == 1991 ~ prob - cenario4[[1]][2],
      ano == 2000 ~ prob - cenario4[[1]][3],
      ano == 2010 ~ prob - cenario4[[1]][4]),
  ) |> 
  select(-prob) |> 
  pivot_longer(cenario0:cenario4,names_to = "cenarios", values_to = "probs")

preditos_cenario_total |> 
  mutate(cenarios = factor(
    cenarios,
    levels = c("cenario0","cenario1","cenario2","cenario3","cenario4"),
    labels = c("Probs. preditas","Sem efeito composição (agregado)",
               "Sem efeito composição (demográfico)","Sem efeito composição (socioeconômico)",
               "Sem efeito composição (domiciliar)"))) |> 
  ggplot() +
  aes(x = ano, y = probs, group = cenarios) +
  geom_line(aes(color = cenarios), linewidth = 1.2) +
  geom_point(aes(color = cenarios), size = 3) +
  labs(
    y = "Probabilidade (predita) de ser responsável pelo domicílio",
    x = "Ano censitário",
    color = "Cenários"
  ) +
  scale_y_continuous(breaks = seq(0,.5,.02)) +
  scale_x_continuous(breaks = c(1970,1980,1991,2000,2010)) +
  scale_color_manual(values = c("#4d4d4d","#969696","#1b9e77","#d95f02","#7570b3")) +
  theme_light() +
  theme(
    axis.title = element_text(face = "bold", size = 14, vjust = .5, hjust = .5),
    axis.text = element_text(size = 12, vjust = .5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

## Simulacao 2 - sem efeito de variaveis importantes

# Cenario 1 - sem efeito de idade
cenario1_atrib <- dec_detailed |> 
  filter(variables == "Grupo etário" & efeito %in% c("atributo")) |> 
  select(value)

cenario1_coef <- dec_detailed |> 
  filter(variables == "Grupo etário" & efeito %in% c("coeficiente")) |> 
  select(value)

# Cenario 2 - sem efeito de sexo
cenario2_atrib <- dec_detailed |> 
  filter(variables == "Sexo: Feminino" & efeito %in% c("atributo")) |> 
  select(value)

cenario2_coef <- dec_detailed |> 
  filter(variables == "Sexo: Feminino" & efeito %in% c("coeficiente")) |> 
  select(value)

# Cenario 3 - sem efeito de contribuicao para a renda do domiclio
cenario3_atrib <- dec_detailed |> 
  filter(variables %in% c("Renda rel.: Intermediaria","Renda rel.: Maior") &
           efeito %in% c("atributo")) |>
  group_by(periodo) |> 
  reframe(value = sum(value)) |> 
  select(value)

cenario3_coef <- dec_detailed |> 
  filter(variables %in% c("Renda rel.: Intermediaria","Renda rel.: Maior") &
           efeito %in% c("coeficiente")) |>
  group_by(periodo) |> 
  reframe(value = sum(value)) |> 
  select(value)

preditos_cenario_variaveis <- preditos |> 
  mutate(
    cenario1 = prob,
    cenario1_atrib = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario1_atrib[[1]][1],
      ano == 1991 ~ prob - cenario1_atrib[[1]][2],
      ano == 2000 ~ prob - cenario1_atrib[[1]][3],
      ano == 2010 ~ prob - cenario1_atrib[[1]][4]),
    cenario1_coef = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario1_coef[[1]][1],
      ano == 1991 ~ prob - cenario1_coef[[1]][2],
      ano == 2000 ~ prob - cenario1_coef[[1]][3],
      ano == 2010 ~ prob - cenario1_coef[[1]][4]),
    cenario2 = prob,
    cenario2_atrib = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario2_atrib[[1]][1],
      ano == 1991 ~ prob - cenario2_atrib[[1]][2],
      ano == 2000 ~ prob - cenario2_atrib[[1]][3],
      ano == 2010 ~ prob - cenario2_atrib[[1]][4]),
    cenario2_coef = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario2_coef[[1]][1],
      ano == 1991 ~ prob - cenario2_coef[[1]][2],
      ano == 2000 ~ prob - cenario2_coef[[1]][3],
      ano == 2010 ~ prob - cenario2_coef[[1]][4]),
    cenario3 = prob,
    cenario3_atrib = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario3_atrib[[1]][1],
      ano == 1991 ~ prob - cenario3_atrib[[1]][2],
      ano == 2000 ~ prob - cenario3_atrib[[1]][3],
      ano == 2010 ~ prob - cenario3_atrib[[1]][4]),
    cenario3_coef = case_when(
      ano == 1970 ~ prob,
      ano == 1980 ~ prob - cenario3_coef[[1]][1],
      ano == 1991 ~ prob - cenario3_coef[[1]][2],
      ano == 2000 ~ prob - cenario3_coef[[1]][3],
      ano == 2010 ~ prob - cenario3_coef[[1]][4])
  ) |> 
  select(-prob) |> 
  pivot_longer(cenario1:cenario3_coef,names_to = "cenarios", values_to = "probs") |> 
  mutate(variaveis = rep(c(c("Idade","Idade","Idade"),
                           c("Sexo","Sexo","Sexo"),
                           c("Renda relativa no domicílio","Renda relativa no domicílio","Renda relativa no domicílio")),5),
         cenarios = rep(c("Prob. predita", "Sem efeito composição","Sem efeito propensão"),15))

preditos_cenario_variaveis |> 
  ggplot() +
  aes(x = ano, y = probs, group = as_factor(cenarios)) +
  geom_line(aes(color = cenarios), linewidth = 1.2) +
  geom_point(aes(color = cenarios), size = 3) +
  labs(
    y = "Probabilidade (predita) de ser responsável pelo domicílio",
    x = "Ano censitário",
    color = "Cenários"
  ) +
  scale_y_continuous(breaks = seq(0,.5,.02)) +
  scale_x_continuous(breaks = c(1970,1980,1991,2000,2010)) +
  lemon::facet_rep_wrap(. ~ variaveis, repeat.tick.labels = T) +
  scale_color_manual(values = c("#4d4d4d","#fe9929","#1b9e77")) +
  theme_light() +
  theme(
    axis.title = element_text(face = "bold", size = 14, vjust = .5, hjust = .5),
    axis.text = element_text(face = "italic", size = 12, vjust = .5),
    strip.text = element_text(face = "bold", size = 12, vjust = .5, hjust = .5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Saving data -------------------------------------------------------------

library(openxlsx)

wb <- createWorkbook()

modifyBaseFont(wb, fontSize = "11", fontName = "Calibri")

addWorksheet(wb, sheetName = "ReadMe")
addWorksheet(wb, sheetName = "1 - Decomposição agregada")
addWorksheet(wb, sheetName = "2 - Decomposição por fatores")
addWorksheet(wb, sheetName = "3 - Decomposição detalhada")

writeDataTable(wb, sheet = 2, x = dec_overall, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 3, x = dec_factors, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 4, x = dec_detailed, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb, "./output/pt4/20231103_decomposicao_probabilidade_responsavel.xlsx", overwrite = TRUE)
