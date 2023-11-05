
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(openxlsx)

# Importando materiais ----------------------------------------------------

t1 <- readRDS("./output/pt1/t_1.rds")
t2 <- readRDS("./output/pt1/t_2.rds")
t3 <- readRDS("./output/pt1/t_3.rds")
t4 <- readRDS("./output/pt1/t_4.rds")
t5 <- readRDS("./output/pt1/t_5.rds")
t6 <- readRDS("./output/pt1/t_6.rds")
t7 <- readRDS("./output/pt1/t_7.rds")
t8 <- readRDS("./output/pt1/t_8.rds")

# Importando materiais ----------------------------------------------------

t2_by_sex <- readRDS("./output/pt1/t_2_by_sex.rds")
t4_by_sex <- readRDS("./output/pt1/t_4_by_sex.rds")
t5_by_sex <- readRDS("./output/pt1/t_5_by_sex.rds")
t6_by_sex <- readRDS("./output/pt1/t_6_by_sex.rds")
t7_by_sex <- readRDS("./output/pt1/t_7_by_sex.rds")
t8_by_sex <- readRDS("./output/pt1/t_8_by_sex.rds")
# Salvando em excel -------------------------------------------------------

# Salvando em excel
wb <- createWorkbook()

modifyBaseFont(wb, fontSize = "11", fontName = "Calibri")

addWorksheet(wb, sheetName = "ReadMe")
addWorksheet(wb, sheetName = "1 - Por ano (ambos sexos)")
addWorksheet(wb, sheetName = "1.2 - Por ano e sexo")
addWorksheet(wb, sheetName = "2 - Por grupo etario")
addWorksheet(wb, sheetName = "2.1 - Por grupo etario e sexo")
addWorksheet(wb, sheetName = "3 - Por status marital")
addWorksheet(wb, sheetName = "3.1 - Por status marital e sexo")
addWorksheet(wb, sheetName = "4 - Por escolaridade")
addWorksheet(wb, sheetName = "4.1 - Por escolaridade e sexo")
addWorksheet(wb, sheetName = "5 - Por condição de ocupação")
addWorksheet(wb, sheetName = "5.1 - Por cond. ocupação e sexo")
addWorksheet(wb, sheetName = "6 - Por quintil de renda")
addWorksheet(wb, sheetName = "6.1 - Por quintil de renda")
addWorksheet(wb, sheetName = "7 - Por cont. renda")
addWorksheet(wb, sheetName = "7.1 - Por cont. renda e sexo")


writeDataTable(wb, sheet = 2, x = t1, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 3, x = t3, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 4, x = t2, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 5, x = t2_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 6, x = t4, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 7, x = t4_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 8, x = t5, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 9, x = t5_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 10, x = t6, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 11, x = t6_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 12, x = t7, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 13, x = t7_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 14, x = t8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 15, x = t8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb, "./output/pt1/descritivas_responsabilidade_domiciliar.xlsx", overwrite = TRUE)
