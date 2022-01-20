#' ---
#' Title: "Compare two sources of data"
#' Author: "Matthieu"
#' Date: 2022-01-20
#' ---

library(tidyverse)

options(pillar.print_max = 3)
options(pillar.print_min = 3)

################################
#'## Read data
################################


fiscal_dat <- read_csv("02_In_Process/IMAFORA_municipios_modulos_fiscais_FirstSource_CLEAN.csv")
fiscal_dat_ALTER <- read_csv("02_In_Process/IMAFORA_municipios_modulos_fiscais_AlterSource_CLEAN.csv")

################################
#'## Prep data
################################


fiscal_dat_c <- fiscal_dat %>% 
  arrange(MUN_CODE)

fiscal_dat_ALTER_c <- fiscal_dat_ALTER %>% 
  arrange(MUN_CODE)

nrow(fiscal_dat_c)
nrow(fiscal_dat_ALTER_c)

## different names?
inner_joined <- fiscal_dat_c %>% 
  inner_join(fiscal_dat_ALTER_c, by = "MUN_CODE") 

outer_joined <- fiscal_dat_c %>% 
  full_join(fiscal_dat_ALTER_c, by = "MUN_CODE") 

outer_joined

## different names? many!
name_diffs <-  inner_joined%>% 
  filter(MUN_NAME.x!=MUN_NAME.y) %>% 
  select(contains("MUN_NAME."))


## which miss?
miss_in_ALTER <- fiscal_dat_c %>% 
  anti_join(fiscal_dat_ALTER_c, by = "MUN_CODE") 
miss_in_v1 <- fiscal_dat_ALTER_c %>% 
  anti_join(fiscal_dat_c, by = "MUN_CODE")

missings <- miss_in_ALTER%>% 
  mutate(miss = "in_ALTER") %>% 
  bind_rows(miss_in_v1 %>% 
              mutate(miss = "in_v1"))
missings

##
UF_all <- unique(fiscal_dat$UF_ABBREV)
check_1 <- function(str, UF=NULL){
  if(is.null(UF)) UF <- UF_all
  fiscal_dat_c %>% 
    filter(str_detect(MUN_NAME, str) & UF_ABBREV %in% UF) %>% 
    print()
  fiscal_dat_ALTER_c %>% 
    filter(str_detect(MUN_NAME, str)& UF_ABBREV %in% UF) %>% 
    print()
}

print(missings, n=Inf)
check_1("Aroeiras|Itaim|Aroe")
check_1("Figueirão|Figueira")
check_1("Ipiranga do N|Tapurah")
check_1("Itanhangá|Itanhan")

check_1("Serra Caiada|Presidente Juscelino")
check_1("Piçarras|Pi[a-z]+ras", UF="SC")
check_1("Pinto Bandeira|Pinto|Bandeira", UF="RS")

### same values for inner join?
inner_joined %>% 
  filter(Modulo_fiscal_ha.x!=Modulo_fiscal_ha.y) %>% 
  select(contains("MUN_NAME"), starts_with("Mod"))

## 7 is right, see also https://www.embrapa.br/codigo-florestal/area-de-reserva-legal-arl/modulo-fiscal

################################
#'## Manual binding
################################

## Correct one, add missings
fiscal_dat_crct <- fiscal_dat %>% 
  mutate(Modulo_fiscal_ha = ifelse(MUN_NAME=="Ilha de Itamaracá",
                                   7, Modulo_fiscal_ha)) %>% 
  bind_rows(miss_in_v1) 

## check last added
fiscal_dat_crct %>% 
  tail(3)

## clean final
fiscal_dat_crct_c <- fiscal_dat_crct %>% 
  arrange(MUN_CODE)

################################
#'## Export
################################


write_csv(fiscal_dat_crct_c,
          "03_Final_data/IMAFORA_municipios_modulos_fiscais_CLEAN.csv")
