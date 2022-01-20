#' ---
#' Title: "Import muni data"
#' Author: "Matthieu"
#' Date: 2022-01-19
#' ---



## download main data
url_imaflora <- "https://www.imaflora.org/public/media/biblioteca/5565c1ebcb122_MF_INCRA_completo.xls"
download.file(url_imaflora, "01_Raw_Data/IMAFORA-municipios_modulos_fiscais.xls")

## download ALTER source, with MUN_CODES
file.copy("../Spatial/BR_admin_units_Municipios/03_Final_Data/BR_Municipios_2020_clean_DATA.csv",
          "01_Raw_Data/BR_Municipios_2020_clean_DATA.csv")

