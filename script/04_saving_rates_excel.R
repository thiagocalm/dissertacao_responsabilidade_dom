
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(openxlsx)

# Importando materiais ----------------------------------------------------

t1 <- readRDS("./output/pt2/tx_t1.rds")
t2 <- readRDS("./output/pt2/tx_t2.rds")
t3 <- readRDS("./output/pt2/tx_t3.rds")
t4 <- readRDS("./output/pt2/tx_t4.rds")
t5 <- readRDS("./output/pt2/tx_t5.rds")
t6 <- readRDS("./output/pt2/tx_t6.rds")
t7 <- readRDS("./output/pt2/tx_t7.rds")
t8 <- readRDS("./output/pt2/tx_t8.rds")

# Importando materiais ----------------------------------------------------

t2_by_sex <- readRDS("./output/pt2/tx_t2_by_sex.rds")
t4_by_sex <- readRDS("./output/pt2/tx_t4_by_sex.rds")
t5_by_sex <- readRDS("./output/pt2/tx_t5_by_sex.rds")
t6_by_sex <- readRDS("./output/pt2/tx_t6_by_sex.rds")
t7_by_sex <- readRDS("./output/pt2/tx_t7_by_sex.rds")
t8_by_sex <- readRDS("./output/pt2/tx_t8_by_sex.rds")

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
addWorksheet(wb, sheetName = "7 - Por cont. no dom")
addWorksheet(wb, sheetName = "7.1 - Por cont. no dom e sexo")

writeDataTable(wb, sheet = 2, x = tx_t1, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 3, x = tx_t3, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 4, x = tx_t2, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 5, x = tx_t2_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 6, x = tx_t4, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 7, x = tx_t4_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 8, x = tx_t5, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 9, x = tx_t5_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 10, x = tx_t6, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 11, x = tx_t6_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 12, x = tx_t7, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 13, x = tx_t7_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 14, x = tx_t8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 15, x = tx_t8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb, "./output/pt2/descritivas_taxas_responsabilidade_domiciliar.xlsx", overwrite = TRUE)



# Salvando arquivos para trabalhar no corpo do texto ----------------------

# Salvando em excel
wb <- createWorkbook()

modifyBaseFont(wb, fontSize = "11", fontName = "Calibri")

addWorksheet(wb, sheetName = "Taxa cont no dom")
addWorksheet(wb, sheetName = "Taxa cont dom e sexo")
addWorksheet(wb, sheetName = "Taxa quint. inc")
addWorksheet(wb, sheetName = "Taxa qui inc e sex")
addWorksheet(wb, sheetName = "Taxa stat ocup")
addWorksheet(wb, sheetName = "Taxa st ocup e sex")


writeDataTable(wb, sheet = 1, x = tx_t8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 2, x = tx_t8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 1, x = tx_t8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 2, x = tx_t8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 1, x = tx_t8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 2, x = tx_t8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb, "../Escrita/3. Perfil da pop. responsavel/para_quadros.xlsx", overwrite = TRUE)
