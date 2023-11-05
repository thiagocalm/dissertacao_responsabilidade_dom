options(scipen = 999999)
gc()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)

# Import data -------------------------------------------------------------

anos <- c("1970", "1980", "1991", "2000", "2010")

for(i in anos){
  dir <- paste0("./dados/df_", i,".RData")
  load(dir)
}

# Selecting variables and cases to sample ---------------------------------

# 1970

df_1970 <- df_1970 |> 
  na.omit() |>  
  mutate(
    id_pes = as.numeric(id_pes),
    id_dom = as.numeric(id_dom),
    YEAR = as.integer(YEAR),
    PERWT = as.numeric(PERWT),
    grupo_etario = as.character(grupo_etario),
    SEX = as.character(SEX),
    MARSTD = as.character(MARSTD),
    RELATED = as.character(RELATED),
    EDATTAIND = as.character(EDATTAIND),
    INCTOT = as.numeric(INCTOT),
    EMPSTAT = as.character(EMPSTAT),
    HHTYPE = as.character(HHTYPE),
    PERNUM = as.integer(PERNUM),
    PERSONS = as.integer(PERSONS),
    inc_dom = as.numeric(inc_dom))

# 1980

df_1980 <- df_1980 |> 
  na.omit() |>  
  mutate(
    id_pes = as.numeric(id_pes),
    id_dom = as.numeric(id_dom),
    YEAR = as.integer(YEAR),
    PERWT = as.numeric(PERWT),
    grupo_etario = as.character(grupo_etario),
    SEX = as.character(SEX),
    MARSTD = as.character(MARSTD),
    RELATED = as.character(RELATED),
    EDATTAIND = as.character(EDATTAIND),
    INCTOT = as.numeric(INCTOT),
    EMPSTAT = as.character(EMPSTAT),
    HHTYPE = as.character(HHTYPE),
    PERNUM = as.integer(PERNUM),
    PERSONS = as.integer(PERSONS),
    inc_dom = as.numeric(inc_dom))

# 1991

df_1991 <- df_1991 |> 
  na.omit() |>  
  mutate(
    id_pes = as.numeric(id_pes),
    id_dom = as.numeric(id_dom),
    YEAR = as.integer(YEAR),
    PERWT = as.numeric(PERWT),
    grupo_etario = as.character(grupo_etario),
    SEX = as.character(SEX),
    MARSTD = as.character(MARSTD),
    RELATED = as.character(RELATED),
    EDATTAIND = as.character(EDATTAIND),
    INCTOT = as.numeric(INCTOT),
    EMPSTAT = as.character(EMPSTAT),
    HHTYPE = as.character(HHTYPE),
    PERNUM = as.integer(PERNUM),
    PERSONS = as.integer(PERSONS),
    inc_dom = as.numeric(inc_dom))

# 2000

df_2000 <- df_2000 |> 
  na.omit() |>  
  mutate(
    id_pes = as.numeric(id_pes),
    id_dom = as.numeric(id_dom),
    YEAR = as.integer(YEAR),
    PERWT = as.numeric(PERWT),
    grupo_etario = as.character(grupo_etario),
    SEX = as.character(SEX),
    MARSTD = as.character(MARSTD),
    RELATED = as.character(RELATED),
    EDATTAIND = as.character(EDATTAIND),
    INCTOT = as.numeric(INCTOT),
    EMPSTAT = as.character(EMPSTAT),
    HHTYPE = as.character(HHTYPE),
    PERNUM = as.integer(PERNUM),
    PERSONS = as.integer(PERSONS),
    inc_dom = as.numeric(inc_dom))

# 2010

df_2010 <- df_2010 |> 
  na.omit() |>  
  mutate(
    id_pes = as.numeric(id_pes),
    id_dom = as.numeric(id_dom),
    YEAR = as.integer(YEAR),
    PERWT = as.numeric(PERWT),
    grupo_etario = as.character(grupo_etario),
    SEX = as.character(SEX),
    MARSTD = as.character(MARSTD),
    RELATED = as.character(RELATED),
    EDATTAIND = as.character(EDATTAIND),
    INCTOT = as.numeric(INCTOT),
    EMPSTAT = as.character(EMPSTAT),
    HHTYPE = as.character(HHTYPE),
    PERNUM = as.integer(PERNUM),
    PERSONS = as.integer(PERSONS),
    inc_dom = as.numeric(inc_dom))

# Sampling data -----------------------------------------------------------

### 1970

# Sampling

set.seed(100)

sample_size <- nrow(df_1970)*.1

ids <- sample(1:nrow(df_1970), sample_size, replace = F)

df_1970_sample <- df_1970[ids, ]

# Testing

pvalues_1970 <- list()

df_1970_test <- df_1970 |> as.data.frame()
df_1970_sample_test <- df_1970_sample |> as.data.frame()

for (col in names(df_1970_sample_test)) {
  
  if (class(df_1970_sample_test[,col]) %in% c("numeric","integer")) {
    # Numeric variable. Using Kolmogorov-Smirnov test
    
    pvalues_1970[[col]] = ks.test(df_1970_sample_test[[col]], df_1970_test[[col]])$p.value
    
  } else {
    # Categorical variable. Using Pearson's Chi-square test
    
    probs <- table(df_1970_test[[col]])/nrow(df_1970_test)
    
    pvalues_1970[[col]] <- chisq.test(table(df_1970_sample_test[[col]]), p=probs)$p.value
    
  }
}

pvalues_1970

# Increasing the PERWT in 10

df_1970_sample <- df_1970_sample |> 
  mutate(PERWT = PERWT*10)

# Removing objects

rm(df_1970, df_1970_sample_test, df_1970_test)
gc()

###
### 1980

# Sampling

set.seed(100)

sample_size <- nrow(df_1980)*.1

ids <- sample(1:nrow(df_1980), sample_size, replace = F)

df_1980_sample <- df_1980[ids, ]

# Testing

pvalues_1980 <- list()

df_1980_test <- df_1980 |> as.data.frame()
df_1980_sample_test <- df_1980_sample |> as.data.frame()

for (col in names(df_1980_sample_test)) {
  
  if (class(df_1980_sample_test[,col]) %in% c("numeric","integer")) {
    # Numeric variable. Using Kolmogorov-Smirnov test
    
    pvalues_1980[[col]] = ks.test(df_1980_sample_test[[col]], df_1980_test[[col]])$p.value
    
  } else {
    # Categorical variable. Using Pearson's Chi-square test
    
    probs <- table(df_1980_test[[col]])/nrow(df_1980_test)
    
    pvalues_1980[[col]] <- chisq.test(table(df_1980_sample_test[[col]]), p=probs)$p.value
    
  }
}

pvalues_1980

# Increasing the PERWT in 10

df_1980_sample <- df_1980_sample |> 
  mutate(PERWT = PERWT*10)

# Removing objects

rm(df_1980, df_1980_sample_test, df_1980_test)
gc()

###
### 1991

# Sampling

set.seed(100)

sample_size <- nrow(df_1991)*.1

ids <- sample(1:nrow(df_1991), sample_size, replace = F)

df_1991_sample <- df_1991[ids, ]

# Testing

pvalues_1991 <- list()

df_1991_test <- df_1991 |> as.data.frame()
df_1991_sample_test <- df_1991_sample |> as.data.frame()

for (col in names(df_1991_sample_test)) {
  
  if (class(df_1991_sample_test[,col]) %in% c("numeric","integer")) {
    # Numeric variable. Using Kolmogorov-Smirnov test
    
    pvalues_1991[[col]] = ks.test(df_1991_sample_test[[col]], df_1991_test[[col]])$p.value
    
  } else {
    # Categorical variable. Using Pearson's Chi-square test
    
    probs <- table(df_1991_test[[col]])/nrow(df_1991_test)
    
    pvalues_1991[[col]] <- chisq.test(table(df_1991_sample_test[[col]]), p=probs)$p.value
    
  }
}

pvalues_1991

# Increasing the PERWT in 10

df_1991_sample <- df_1991_sample |> 
  mutate(PERWT = PERWT*10)

# Removing objects

rm(df_1991, df_1991_sample_test, df_1991_test)
gc()

###
### 2000

# Sampling

set.seed(100)

sample_size <- nrow(df_2000)*.1

ids <- sample(1:nrow(df_2000), sample_size, replace = F)

df_2000_sample <- df_2000[ids, ]

# Testing

pvalues_2000 <- list()

df_2000_test <- df_2000 |> as.data.frame()
df_2000_sample_test <- df_2000_sample |> as.data.frame()

for (col in names(df_2000_sample_test)) {
  
  if (class(df_2000_sample_test[,col]) %in% c("numeric","integer")) {
    # Numeric variable. Using Kolmogorov-Smirnov test
    
    pvalues_2000[[col]] = ks.test(df_2000_sample_test[[col]], df_2000_test[[col]])$p.value
    
  } else {
    # Categorical variable. Using Pearson's Chi-square test
    
    probs <- table(df_2000_test[[col]])/nrow(df_2000_test)
    
    pvalues_2000[[col]] <- chisq.test(table(df_2000_sample_test[[col]]), p=probs)$p.value
    
  }
}

pvalues_2000

# Increasing the PERWT in 10

df_2000_sample <- df_2000_sample |> 
  mutate(PERWT = PERWT*10)

# Removing objects

rm(df_2000, df_2000_sample_test, df_2000_test)
gc()

###
### 2010

# Sampling

set.seed(100)

sample_size <- nrow(df_2010)*.1

ids <- sample(1:nrow(df_2010), sample_size, replace = F)

df_2010_sample <- df_2010[ids, ]

# Testing

pvalues_2010 <- list()

df_2010_test <- df_2010|> as.data.frame()
df_2010_sample_test <- df_2010_sample |> as.data.frame()

for (col in names(df_2010_sample_test)) {
  
  if (class(df_2010_sample_test[,col]) %in% c("numeric","integer")) {
    # Numeric variable. Using Kolmogorov-Smirnov test
    
    pvalues_2010[[col]] = ks.test(df_2010_sample_test[[col]], df_2010_test[[col]])$p.value
    
  } else {
    # Categorical variable. Using Pearson's Chi-square test
    
    probs <- table(df_2010_test[[col]])/nrow(df_2010_test)
    
    pvalues_2010[[col]] <- chisq.test(table(df_2010_sample_test[[col]]), p=probs)$p.value
    
  }
}

pvalues_2010

# Increasing the PERWT in 10

df_2010_sample <- df_2010_sample |> 
  mutate(PERWT = PERWT*10)

# Removing objects

rm(df_2010, df_2010_sample_test, df_2010_test)
gc()


# Statistic Significance of the Samples -----------------------------------

pvalues <- pvalues_1970 |> as_tibble() |> mutate(ano = 1970) |> 
  bind_rows(pvalues_1980 |> as_tibble() |> mutate(ano = 1980)) |> 
  bind_rows(pvalues_1991 |> as_tibble() |> mutate(ano = 1991)) |> 
  bind_rows(pvalues_2000 |> as_tibble() |> mutate(ano = 2000)) |> 
  bind_rows(pvalues_2010 |> as_tibble() |> mutate(ano = 2010)) |> 
  select(ano, everything())

pvalues > .05
  
saveRDS(pvalues, "./dados/sample/pvalues_sample.rds")


# Saving data samples -----------------------------------------------------

save(df_1970_sample, file = "./dados/sample/df_1970_sample.RData")
save(df_1980_sample, file = "./dados/sample/df_1980_sample.RData")
save(df_1991_sample, file = "./dados/sample/df_1991_sample.RData")
save(df_2000_sample, file = "./dados/sample/df_2000_sample.RData")
save(df_2010_sample, file = "./dados/sample/df_2010_sample.RData")

