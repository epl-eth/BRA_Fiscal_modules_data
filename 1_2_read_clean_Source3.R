#' ---
#' Title: "Clean raw data from third source "
#' Author: "Matthieu"
#' Date: 2022-01-20
#' ---

library(tidyverse)
library(rjson)
source("888_fzy_match_functions.R")

################################
#'## Import data
################################


dat_Muni <- read_csv("01_Raw_Data/BR_Municipios_2020_clean_DATA.csv")

## data from https://www.embrapa.br/codigo-florestal/area-de-reserva-legal-arl/modulo-fiscal
dic <- readLines("01_Raw_Data/muni_fisc_mod.js")

## Clean then read as json
ad <- str_replace_all(dic, "([A-zÀ-ÿ ]+):", '"\\1":')
out_raw <- jsonlite::fromJSON(ad)

## format json to df
out_li <- out_raw %>% 
  map(~tibble(MUN_NAME=names(.x), value=unlist(.x)))
src3_dat_mFisc <- tibble(UF_ABBREV = names(out_li), Modulo_fiscal_ha =out_li) %>% 
  unnest(Modulo_fiscal_ha) %>% 
  rename(Modulo_fiscal_ha=value) %>% 
  mutate(Modulo_fiscal_ha = as.integer(Modulo_fiscal_ha))

src3_dat_mFisc

################################
#'## Merge direct
################################


## prep dat_Muni
dat_Muni_prep <- dat_Muni %>% 
  select(-AREA_KM2) %>% 
  mutate(MUN_NAME=str_to_upper(MUN_NAME))


## Merge direct
src3_dat_mFisc_D0 <- src3_dat_mFisc %>% 
  mutate(MUN_id_temp=1:n()) %>% 
  left_join(dat_Muni_prep,
            by = c("MUN_NAME", "UF_ABBREV"))

## missing matches?
src3_matched_D0 <- src3_dat_mFisc_D0 %>% 
  filter(!is.na(MUN_CODE)) %>% 
  select(-MUN_id_temp)
src3_unmatched_D0 <- src3_dat_mFisc_D0 %>% 
  filter(is.na(MUN_CODE)) %>% 
  select(-MUN_id_temp)

################################
#'## Fuzzy matching for remainder
################################

## new pool
dat_Muni_prep_afterD0 <- dat_Muni_prep %>% 
  anti_join(src3_matched_D0, by="MUN_CODE")

## try fuzzy

src3_fuzzy_matched_D1 <- fzy_match_fuzzy_here(df_unmatched =src3_unmatched_D0,
                                          df_matched = src3_matched_D0)
src3_matched_D1 <- fzy_matches_clean(src3_fuzzy_matched_D1)
src3_unmatched_D1  <- src3_unmatched_D0 %>% 
  anti_join(src3_fuzzy_matched_D1, by=c("MUN_NAME"="MUN_NAME.x"))

src3_unmatched_D1

## Distance of 2 for unmatched at 1
src3_matched_D01 <- bind_rows(src3_matched_D0, src3_matched_D1)
src3_fuzzy_matched_D2 <- fzy_match_fuzzy_here(src3_unmatched_D1,
                                          df_matched = src3_matched_D01,
                                          max_dist=2)
src3_matched_D2 <- fzy_matches_clean(src3_fuzzy_matched_D2)
src3_unmatched_D2  <- src3_unmatched_D1 %>% 
  anti_join(rbind(src3_fuzzy_matched_D2, src3_fuzzy_matched_D1),
            by=c("MUN_NAME"="MUN_NAME.x"))

src3_unmatched_D2

################################
#'## Manual
################################

src3_matched_now <- bind_rows(src3_matched_D0,
                             src3_matched_D1,
                             src3_matched_D2)
src3_unmatched_D2


"COUTO MAGALHÃES DE MINAS"
filter(src3_matched_now, str_detect(MUN_NAME, "COUTO|MAGALH") & UF_ABBREV=="MG")
filter(dat_Muni_prep, str_detect(MUN_NAME, "COUTO|MAGALHÃE") & UF_ABBREV=="MG")
filter(dat_Muni_prep, MUN_CODE ==3120102& UF_ABBREV=="PB")
#3120102

"ITABIRINHA DE MANTENA, MG"
filter(src3_matched_now, str_detect(MUN_NAME, "ITABIRINHA|MANTENA") & UF_ABBREV=="MG")
filter(dat_Muni_prep, str_detect(MUN_NAME, "ITABIRINHA|MANTENA") & UF_ABBREV=="MG")

"SÃO DOMINGOS DE POMBAL, PB"
filter(src3_matched_now, str_detect(MUN_NAME, "DOMINGOS|POMBAL") & UF_ABBREV=="PB")
filter(dat_Muni_prep, str_detect(MUN_NAME, "DOMINGOS|POMBAL") & UF_ABBREV=="PB")

"COUTO DE MAGALHÃES, TO"
filter(src3_matched_now, str_detect(MUN_NAME, "COUTO|MAGALHÃES") & UF_ABBREV=="TO")
filter(dat_Muni_prep, str_detect(MUN_NAME, "COUTO|MAGALHÃES") & UF_ABBREV=="TO")


src3_match_manu <- tribble(
  ~MUN_NAME, ~MUN_NAME_corr,
  "COUTO MAGALHÃES DE MINAS", "COUTO DE MAGALHÃES DE MINAS",
  "ITABIRINHA DE MANTENA", "ITABIRINHA",
  "SANTARÉM", "JOCA CLAUDINO",
  "SÃO DOMINGOS DE POMBAL", "SÃO DOMINGOS",
  "SERIDÓ", "São Vicente do Seridó",
  "Augusto Severo", "Campo Grande",
  "Embu", "Embu das Artes",
  "COUTO DE MAGALHÃES", "COUTO MAGALHÃES",
  "Fortaleza do Tabocão", "Tabocão",
  "São Valério da Natividade", "São Valério"
  # "Campo de Santana", "Tacima",
  # "Olho-D'água do Borges", "Olho d'Água do Borges",
  ) %>% 
  mutate(across(everything(), str_to_upper)) %>% 
  left_join(src3_unmatched_D2 %>% 
              select(MUN_NAME, UF_ABBREV, Modulo_fiscal_ha),
            by = c("MUN_NAME")) %>% 
  left_join(dat_Muni_prep,
            by =c("MUN_NAME_corr"="MUN_NAME", "UF_ABBREV"))

src3_match_manu

## jon all now
src3_matched_final <- bind_rows(src3_matched_D0 %>% 
                                  mutate(is_direct_match=TRUE),
                                src3_matched_D1,
                                src3_matched_D2,
                                src3_match_manu %>% 
                                  rename(MUN_NAME_from_shp=MUN_NAME_corr)) %>% 
  rename(MUN_NAME_corrected=MUN_NAME_from_shp) %>% 
  mutate(is_direct_match=replace_na(is_direct_match, FALSE))

src3_matched_final


## clean final
src3_matched_final_c <- src3_matched_final %>% 
  rename(MUN_NAME_data_orig = MUN_NAME,
         MUN_NAME = MUN_NAME_corrected) %>% 
  mutate(MUN_NAME= if_else(is.na(MUN_NAME),
                           MUN_NAME_data_orig,
                           MUN_NAME)) %>% 
  select(-is_direct_match) %>% 
  select(-MUN_NAME) %>% 
  left_join(dat_Muni %>% 
              select(MUN_NAME, MUN_CODE),
            by = "MUN_CODE") %>% 
  relocate(MUN_NAME, MUN_NAME_data_orig, MUN_CODE, UF_ABBREV) %>% 
  arrange(UF_ABBREV, MUN_NAME) 

src3_matched_final_c

################################
#'## Checks
################################

## CHECK: nas? OK!
src3_matched_final_c %>% 
  filter(is.na(MUN_CODE))

## CHECK: missing modulo?
src3_matched_final_c %>% 
  filter(is.na(Modulo_fiscal_ha))


## CHECK: all matched?
src3_dat_mFisc %>% 
  anti_join(src3_matched_final_c, by=c("MUN_NAME" ="MUN_NAME_data_orig"))

all(sort(src3_matched_final_c$MUN_NAME_data_orig)==sort(src3_dat_mFisc$MUN_NAME))


## CHECK: no dups?
src3_matched_final_c %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

################################
#'## Export data
################################

write_csv(src3_matched_final_c,
          "02_In_Process/EMBRAPA_municipios_modulos_fiscais_Source_3_CLEAN.csv")
