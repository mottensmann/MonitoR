# library(openxlsx)
# library(readxl)
#
# path <- "D:/BirdNET/2MM25557_20260402"
#
# xlsx_path <- file.path(path, "BirdNET.xlsx")
# wb <- loadWorkbook(xlsx_path)
#
# # Read sheet, fix the slash in the taxon name
# df <- read_xlsx(xlsx_path)
# df$Taxon <- gsub("/", "-", df$Taxon, fixed = TRUE)
#
# # Overwrite the Taxon column (adjust sheet name/column index if needed)
# writeData(wb, sheet = 1, x = df$Taxon, startCol = which(names(df) == "Taxon"),
#           startRow = 2, colNames = FALSE)
# saveWorkbook(wb, xlsx_path, overwrite = TRUE)
