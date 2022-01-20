#' ---
#' Title: "Compare two sources of data"
#' Author: "Matthieu"
#' Date: 2022-01-20
#' ---

library(tidyverse)


################################
#'## Read data
################################


fiscal_dat_Src1 <- read_csv("02_In_Process/IMAFORA_municipios_modulos_fiscais_Source_1_CLEAN.csv")
fiscal_dat_Src2 <- read_csv("02_In_Process/IMAFORA_municipios_modulos_fiscais_Source_2_CLEAN.csv")
fiscal_dat_Src3 <- read_csv("02_In_Process/EMBRAPA_municipios_modulos_fiscais_Source_3_CLEAN.csv")

dat_Muni <- read_csv("01_Raw_Data/BR_Municipios_2020_clean_DATA.csv")

################################
#'## Prep data
################################

fiscal_dat_all <- lst(fiscal_dat_Src1,
                      fiscal_dat_Src2,
                      fiscal_dat_Src3)

map_int(fiscal_dat_all, nrow)

## CHECK:: any NAs?
map_int(fiscal_dat_all, ~nrow(filter(., is.na(Modulo_fiscal_ha))))

## Get inner join
inner_joined <- fiscal_dat_all %>% 
  map(~select(., MUN_CODE, Modulo_fiscal_ha)) %>% 
  reduce(~inner_join(.x,.y, by = "MUN_CODE", suffix =c("_s1", "_s2"))) %>% 
  rename(Modulo_fiscal_ha_s3 = Modulo_fiscal_ha)

inner_joined

## differences? YES, one!
## looking at pdf, 7 is the correct one!
inner_joined %>% 
  filter(Modulo_fiscal_ha_s1 !=Modulo_fiscal_ha_s2|Modulo_fiscal_ha_s1 !=Modulo_fiscal_ha_s3)

## correct
inner_joined_crct <- inner_joined %>% 
  select(MUN_CODE, Modulo_fiscal_ha_s2) %>% 
  rename(Modulo_fiscal_ha = Modulo_fiscal_ha_s2)

################################
#'## Different ones
################################

full_joined <- fiscal_dat_all %>% 
  map(~select(., MUN_CODE, Modulo_fiscal_ha, MUN_NAME)) %>% 
  reduce(~full_join(.x,.y, by = "MUN_CODE", suffix =c("_s1", "_s2"))) %>% 
  rename(Modulo_fiscal_ha_s3 = Modulo_fiscal_ha,
         MUN_NAME_s3=MUN_NAME)

full_joined

## Missing in at least 1
outer_all <- full_joined %>% 
  anti_join(inner_joined,
            by = c("MUN_CODE"))

outer_all %>% 
  select(MUN_CODE, contains("MUN_N"))

outer_all_l <- outer_all %>% 
  pivot_longer(-MUN_CODE, 
               names_pattern = "([aA-zZ]+)_(s[1-3])",
               names_to = c(".value", "Source")) %>% 
  filter(!(is.na(Modulo_fiscal_ha) & is.na(MUN_NAME)))

## different Modulo_fiscal_ha values?
outer_all_l %>% 
  distinct(MUN_CODE, Modulo_fiscal_ha) %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

## different names?
outer_all_l %>% 
  distinct(MUN_CODE, MUN_NAME) %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

## correct name is Balneário Piçarras, see https://www.ibge.gov.br/cidades-e-estados/sc/balneario-picarras.html


## different codes?
outer_all_l %>% 
  distinct(MUN_CODE, MUN_NAME) %>% 
  add_count(MUN_NAME) %>% 
  filter(n>1)

filter(dat_Muni, str_detect(MUN_NAME, "Pinto Bandeira"))

## correct one is 4314548, see :
# https://www.ibge.gov.br/cidades-e-estados/rs/pinto-bandeira.html
## pinto bandeira is appearing twice!! With 4314530 and 4314548

## 
outer_correct <- outer_all_l %>% 
  filter(MUN_CODE!=4314530) %>% 
  filter(MUN_NAME!= "Piçarras") %>% 
  distinct(MUN_CODE, MUN_NAME, Modulo_fiscal_ha)
  
outer_correct

## check no dups? OK!
outer_correct %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

outer_correct %>% 
  add_count(MUN_NAME) %>% 
  filter(n>1)

################################
#'## Merge back
################################

## bind inner join and supplements
final <- inner_joined_crct %>% 
  rbind(outer_correct %>% 
          select(MUN_CODE, Modulo_fiscal_ha)) %>%
  arrange(MUN_CODE) %>% 
  left_join(dat_Muni %>% 
              select(MUN_CODE, MUN_NAME, UF_ABBREV),
            by = "MUN_CODE") %>% 
  relocate(MUN_CODE, MUN_NAME, UF_ABBREV)

final

## CHECK: any NAs? OK!
anyNA(final)

## CHECK: duplicates?
final %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

## CHECK: 0 values?
final %>% 
  filter(Modulo_fiscal_ha==0)
  

##
# UF_all <- unique(fiscal_dat_Src1$UF_ABBREV)
# check_1 <- function(str, UF=NULL){
#   if(is.null(UF)) UF <- UF_all
#   fiscal_dat_Src1 %>% 
#     filter(str_detect(MUN_NAME, str) & UF_ABBREV %in% UF) %>% 
#     print()
#   fiscal_dat_Src2 %>% 
#     filter(str_detect(MUN_NAME, str)& UF_ABBREV %in% UF) %>% 
#     print()
# }
# 
# print(missings, n=Inf)
# check_1("Aroeiras|Itaim|Aroe")
# check_1("Figueirão|Figueira")
# check_1("Ipiranga do N|Tapurah")
# check_1("Itanhangá|Itanhan")
# 
# check_1("Serra Caiada|Presidente Juscelino")
# check_1("Piçarras|Pi[a-z]+ras", UF="SC")
# check_1("Pinto Bandeira|Pinto|Bandeira", UF="RS")
# 
# ### same values for inner join?
# inner_joined %>% 
#   filter(Modulo_fiscal_ha.x!=Modulo_fiscal_ha.y) %>% 
#   select(contains("MUN_NAME"), starts_with("Mod"))




################################
#'## Export
################################


write_csv(final,
          "03_Final_data/municipios_modulos_fiscais_CLEAN.csv")
