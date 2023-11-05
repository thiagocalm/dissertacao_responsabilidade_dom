options(scipen = 999999)


# Libraries ---------------------------------------------------------------

if(!require("ipumsr")){install.packages("ipumsr"); require("ipumsr")}
library(tidyverse)
library(magrittr)


# importing data from ipums -------------------------------------------

ddi <- read_ipums_ddi("./dados/raw/ipumsi_00014.xml") # reading ddi
df_2010 <- read_ipums_micro(ddi) #reading data using ddi informations

# Selecting important variables -------------------------------------------

vars <- c("YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT",
          # Individual variables
          "AGE", "SEX", "MARSTD",
          # Household variables
          "RELATED", "PERNUM", "HHTYPE", "NFAMS", "NCOUPLES", "NMOTHERS", "PERSONS",
          "NFATHERS", "NCHILD", "ELDCH", "YNGCH", "MOMLOC", "POPLOC", "SPLOC",
          # Schooling
          #"YRSCHOOL", variável indisponível para 2010! 
          "EDATTAIND", "EDUCBR",
          # Occupation, labor and income
          "INCTOT", "EMPSTAT", "LABFORCE")


df_2010 <- df_2010 |> 
  select(all_of(vars))

head(df_2010)

# Manipulating some variables ---------------------------------------------

df_2010 <- df_2010 |> 
  mutate(
    id_dom = as.numeric(paste0(SAMPLE, SERIAL)),
    id_pes = as.numeric(paste0(id_dom, PERNUM)),
    quesito_tipo = "Chefe do domicíliO",
    grupo_etario = cut(AGE, breaks = seq(0,max(AGE),5),
                       right = FALSE),
    quesito_tipo = "Responsavel pelo domicilio",
    YRSCHOOL = case_when(
      EDUCBR == 0000 ~ "0",
      EDUCBR == 2110 ~ "1",
      EDUCBR == 2120 ~ "2",
      EDUCBR == 2130 ~ "3",
      EDUCBR == 2141 ~ "4",
      EDUCBR == 2142 ~ "5",
      EDUCBR == 2143 ~ "6",
      EDUCBR == 2210 ~ "5",
      EDUCBR == 2220 ~ "6",
      EDUCBR == 2230 ~ "7",
      EDUCBR == 2241 ~ "8",
      EDUCBR == 2242 ~ "9",
      EDUCBR == 3100 ~ "9",
      EDUCBR == 3200 ~ "10",
      EDUCBR == 3300 ~ "11",
      EDUCBR == 3400 ~ "12",
      EDUCBR == 4000 |
        EDUCBR == 4100 |
        EDUCBR == 4110 |
        EDUCBR == 4120 |
        EDUCBR == 4130 |
        EDUCBR == 4140 |
        EDUCBR == 4150 |
        EDUCBR == 4160 |
        EDUCBR == 4170 |
        EDUCBR == 4180 |
        EDUCBR == 4190 |
        EDUCBR == 4200 |
        EDUCBR == 4210 |
        EDUCBR == 4220 |
        EDUCBR == 4230 |
        EDUCBR == 4240 |
        EDUCBR == 4250 |
        EDUCBR == 4260 |
        EDUCBR == 4270 |
        EDUCBR == 4280 |
        EDUCBR == 4300 |
        EDUCBR == 4310 |
        EDUCBR == 4320 |
        EDUCBR == 4330 |
        EDUCBR == 4340 |
        EDUCBR == 4350 |
        EDUCBR == 4360 |
        EDUCBR == 4370 ~ "Mais que 12",
      TRUE ~ "9999")
  ) |> 
  mutate(
    grupo_etario = str_extract(
      str_remove(grupo_etario, 
                 pattern = "\\["), 
      pattern = "^\\d+")) |>  
  mutate(
    grupo_etario = as.integer(grupo_etario)) |> 
  select(-c(SAMPLE, SERIAL, AGE)) |> 
  select(id_dom, id_pes, everything())

glimpse(df_2010)


# Saving in .RData file ---------------------------------------------------

# To save data in .RData for better use in R
df_2010 |> 
  save(file = "./dados/raw/censo_2010.RData")

rm(list = ls()) #cleaning R environment
