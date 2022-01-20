#' ---
#' Title: "Clean raw data"
#' Author: "Matthieu"
#' Date: 2022-01-17
#' ---

library(tidyverse)
library(readxl)

################################
#'## Import data
################################

## Read data: but has two useless rows at the end
dat_Fisc_v1 <- readxl::read_xls("01_Raw_data/IMAFORA-municipios_modulos_fiscais.xls")
nrow(dat_Fisc_v1)

## now read without rows
dat_Fisc_v2 <- readxl::read_xls("01_Raw_data/IMAFORA-municipios_modulos_fiscais.xls",
                                n_max= nrow(dat_Fisc_v1)-2)

dat_Fisc_v2 %>% tail

################################
#'## Process data
################################

dat_Fisc_v2_c <- dat_Fisc_v2 %>% 
  rename(MUN_CODE=Codigo,
         UF_ABBREV = UF,
         MUN_NAME = Municipio,
         Modulo_fiscal_ha=`MF (há)`) %>% 
  relocate(MUN_NAME, MUN_CODE, UF_ABBREV) %>% 
  arrange(MUN_CODE) %>% 
  mutate(MUN_NAME = str_to_title(MUN_NAME) %>% 
           str_replace_all(" Do ", " do ") %>% 
           str_replace_all(" Da ", " da ") %>% 
           str_replace_all(" De ", " de ") %>% 
           str_replace_all("D'oeste", "D'Oeste"),
         MUN_CODE=as.integer(MUN_CODE))

dat_Fisc_v2_c

## any zeros?
dat_Fisc_v2_c %>% 
  filter(Modulo_fiscal_ha==0)

################################
#'## Check data
################################

## 
dat_Fisc_v2_c %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

dat_Fisc_v2_c %>% 
  add_count(MUN_NAME, UF_ABBREV) %>% 
  filter(n>1)

################################
#'## Export
################################


write_csv(dat_Fisc_v2_c,
          "02_In_Process/IMAFORA_municipios_modulos_fiscais_AlterSource_CLEAN.csv")
