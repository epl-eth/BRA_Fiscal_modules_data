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
supplements <- full_joined %>% 
  select(MUN_CODE, contains("Mod")) %>% 
  anti_join(inner_joined,
            by = c("MUN_CODE"))

supplements

## keep unique values
supplements_unique <- supplements%>% 
  gather(source, Modulo_fiscal_ha, starts_with("Modulo_fiscal_ha")) %>% 
  filter(!is.na(Modulo_fiscal_ha)) %>% 
  distinct(MUN_CODE, Modulo_fiscal_ha) 

supplements_unique

## check no dups? OK!
supplements_unique %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

################################
#'## Merge back
################################

## bind inner join and supplements
final <- inner_joined_crct %>% 
  rbind(supplements_unique) %>%
  arrange(MUN_CODE) %>% 
  left_join(dat_Muni %>% 
              select(MUN_CODE, MUN_NAME, UF_ABBREV),
            by = "MUN_CODE") %>% 
  relocate(MUN_CODE, MUN_NAME, UF_ABBREV)

final

## CHECK: any NA?
final %>% 
  filter(is.na(UF_ABBREV))

fiscal_dat_all %>% 
  map_dfr(~filter(., MUN_CODE==4314530))

## Was split 
dat_Muni %>% 
  filter(str_detect(MUN_NAME, "Bento Gonçalves"))

## correct for one
final_c <- final %>% 
  mutate(MUN_NAME = if_else(MUN_CODE==4314530, "Pinto Bandeira", MUN_NAME),
         UF_ABBREV = if_else(MUN_CODE==4314530, "RS", UF_ABBREV))


## CHECK: Nas?
anyNA(final_c)

## duplicates?
final_c %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

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


write_csv(final_c,
          "03_Final_data/municipios_modulos_fiscais_CLEAN.csv")
