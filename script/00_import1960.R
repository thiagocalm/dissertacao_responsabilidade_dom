options(scipen = 999999)


# Libraries ---------------------------------------------------------------

if(!require("ipumsr")){install.packages("ipumsr"); require("ipumsr")}
library(tidyverse)
library(magrittr)


# importing data from ipums -------------------------------------------

ddi <- read_ipums_ddi("./dados/raw/ipumsi_00008.xml") # reading ddi
df_1960 <- read_ipums_micro(ddi) #reading data using ddi informations

# Selecting important variables -------------------------------------------

vars <- c("YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT",
          # Individual variables
          "AGE", "SEX", "MARSTD",
          # Household variables
          "RELATED", "PERNUM", "HHTYPE", "NFAMS", "NCOUPLES", "NMOTHERS", "PERSONS",
          "NFATHERS", "NCHILD", "ELDCH", "YNGCH", "MOMLOC", "POPLOC", "SPLOC",
          # Schooling
          "YRSCHOOL", "EDATTAIND",
          # Occupation, labor and income
          "INCTOT", "EMPSTAT", "LABFORCE")

vars_60 <- c("YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT",
             # Individual variables
             "AGE", "SEX", "MARSTD",
             # Household variables
             "RELATED", "PERNUM", "HHTYPE", "NFAMS", "NCOUPLES", "NMOTHERS", "PERSONS",
             "NFATHERS", "NCHILD", "ELDCH", "YNGCH", "MOMLOC", "POPLOC", "SPLOC",
             # Schooling
             "YRSCHOOL", "EDATTAIND",
             # Occupation, labor and income
             "INCTBR", "EMPSTAT", "LABFORCE")

df_1960 <- df_1960 |> 
  select(all_of(vars_60))

# Manipulating some variables ---------------------------------------------

df_1960 <- df_1960 |> 
  mutate(
    id_dom = as.numeric(paste0(SAMPLE, SERIAL)),
    id_pes = as.numeric(paste0(id_dom, PERNUM)),
    quesito_tipo = "Chefe do domicíliO",
    grupo_etario = cut(AGE, breaks = seq(0,max(AGE),5),
                       right = FALSE)
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

glimpse(df_1960)

# Saving in .RData file ---------------------------------------------------

# To save data in .RData for better use in R
df_1960 |> 
  save(file = "./dados/raw/censo_1960.RData")

rm(list = ls()) #cleaning R environment
