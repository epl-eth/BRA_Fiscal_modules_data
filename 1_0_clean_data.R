#' ---
#' Title: "Clean raw data"
#' Author: "Matthieu"
#' Date: 2022-01-17
#' ---

library(tidyverse)
library(readxl)
library(fuzzyjoin)


################################
#'## Read data
################################

dat_Fisc <- readxl::read_xls("01_Raw_data/IMAFORA-municipios_modulos_fiscais_ManualDownload.xls")
dat_Muni <- read_csv("01_Raw_Data/BR_Municipios_2020_clean_DATA.csv")

################################
#'## Prepare data
################################

## clean: rename variables
dat_Fisc_c <- dat_Fisc %>% 
  rename(Modulo_fiscal_ha = `Módulo fiscal (ha)`,
         UF_ABBREV=UF,
         MUN_NAME = "Município")

dat_Fisc_c

## CHECK: no dups?
dat_Fisc_c %>% 
  add_count(MUN_NAME, UF_ABBREV) %>% 
  filter(n>1)

## CHECK: 0 values?
dat_Fisc_c %>% 
  arrange(Modulo_fiscal_ha)

filter(dat_Fisc_c, str_detect(MUN_NAME, "Noronha"))

## remove 2 weird
dat_Fisc_c2 <- dat_Fisc_c %>% 
  filter(!(Modulo_fiscal_ha==0 & MUN_NAME=="Penha"))


################################
#'## Add MUN codes
################################

## prep dat_Muni
dat_Muni_prep <- dat_Muni %>% 
  select(-AREA_KM2)


### Simple direct match with MUN name
dat_Fisc_Muni <- dat_Fisc_c2 %>% 
  mutate(MUN_id_temp=1:n()) %>% 
  left_join(dat_Muni_prep,
            by = c("MUN_NAME", "UF_ABBREV"))

dat_Fisc_Muni

## too many matches? OK!
dat_Fisc_Muni %>% 
  add_count(MUN_id_temp) %>% 
  filter(n>1)

## missing matches?
matched_D0 <- dat_Fisc_Muni %>% 
  filter(!is.na(MUN_CODE)) %>% 
  select(-MUN_id_temp)
unmatched_D0 <- dat_Fisc_Muni %>% 
  filter(is.na(MUN_CODE)) %>% 
  select(-MUN_id_temp)

## dupli matched?
matched_D0
unmatched_D0

## Check in muni 
dat_Muni %>% 
  filter(str_detect(MUN_NAME, "das Flores"))

################################
#'## Fuzzy matching for remainder
################################

## new pool
dat_Muni_prep_afterD0 <- dat_Muni_prep %>% 
  anti_join(matched_D0, by="MUN_CODE")

## try fuzzy
match_fuzzy_here <- function(df_unmatched,  df_matched= NULL,
                             max_dist=1){
  ## restrict pool of matches
  dat_muni_pool <- dat_Muni_prep %>% 
    anti_join(df_matched, by="MUN_CODE")
  
  df_unmatched %>%
    select(MUN_NAME, UF_ABBREV,Modulo_fiscal_ha) %>% 
    fuzzyjoin::stringdist_left_join(dat_muni_pool, 
                                    by = c("MUN_NAME", "UF_ABBREV"),
                                    max_dist = max_dist,
                                    distance_col="diff") %>% 
    filter(UF_ABBREV.x==UF_ABBREV.y) %>% 
    select(-UF_ABBREV.diff, -one_of("diff")) %>% 
    add_count(MUN_NAME.x, UF_ABBREV.x, name="n_matches")
}

matches_clean <- function(df){
  if(any(df$n_matches>1)) warning("Multiple matches!? Clean first")
  df %>% 
    select(MUN_NAME=MUN_NAME.x,
           UF_ABBREV=UF_ABBREV.x, MUN_CODE, MUN_NAME_from_shp=MUN_NAME.y )
}


## Run at dist 1
fuzzy_matched_D1 <- match_fuzzy_here(df_unmatched =unmatched_D0,
                                     df_matched = matched_D0)
matched_D1 <- matches_clean(fuzzy_matched_D1)
unmatched_D1  <- unmatched_D0 %>% 
  anti_join(fuzzy_matched_D1, by=c("MUN_NAME"="MUN_NAME.x"))

unmatched_D1

## Distance of 2 for unmatched at 1
matched_D01 <- bind_rows(matched_D0, matched_D1)
fuzzy_matched_D2 <- match_fuzzy_here(unmatched_D1,
                                     df_matched = matched_D01,
                                     max_dist=2)
matched_D2 <- matches_clean(fuzzy_matched_D2)
unmatched_D2  <- unmatched_D1 %>% 
  anti_join(rbind(fuzzy_matched_D2, fuzzy_matched_D1), by=c("MUN_NAME"="MUN_NAME.x"))

unmatched_D2

## Distance of 5 for unmatched at D2
matched_D012 <- bind_rows(matched_D01, matched_D2)
fuzzy_matched_D3 <- match_fuzzy_here(unmatched_D2,
                                     matched_D012,
                                     max_dist=5)

fuzzy_matched_D3 %>%
  filter(n_matches==1) %>% 
  select(-UF_ABBREV.y, -Modulo_fiscal_ha, -n_matches)

##
probs <- unmatched_D2 %>% 
  select(MUN_NAME, UF_ABBREV) %>% 
  left_join(fuzzy_matched_D3 %>%
              filter(n_matches==1) %>% 
              select(MUN_NAME.x, MUN_CODE, MUN_NAME.y),
            by = c("MUN_NAME"="MUN_NAME.x")) %>% 
  rename(MUN_NAME_candidate=MUN_NAME.y,
         MUN_CODE_candidate=MUN_CODE) %>% 
  relocate(MUN_CODE_candidate, .after=everything())

probs %>% 
  knitr::kable()
write_csv(probs, "02_In_process/probs_ask.csv")


################################
## search manual
################################

options(pillar.print_min = 7)

## combine matched ones
matched_now <- bind_rows(matched_D0,
                         matched_D1,
                         matched_D2)
probs

"Campo de Santana,PB"
filter(matched_now, str_detect(MUN_NAME, "Tacima|Santana") & UF_ABBREV=="PB")
filter(dat_Muni_prep, str_detect(MUN_NAME, "Tacima|Santana") & UF_ABBREV=="PB")

"São Valério da Natividade,TO" # OK

"Santarém,PB" 
filter(dat_Muni_prep, str_detect(MUN_NAME, "Joca Claudino")|MUN_CODE==1506807)

"Augusto Severo, RN"
filter(dat_Fisc_c, str_detect(MUN_NAME, "Augusto|Severo"))

"Seridó, PB"
filter(dat_Fisc_c, str_detect(MUN_NAME, "Seridó") & UF_ABBREV=="PB")
filter(dat_Muni_prep, str_detect(MUN_NAME, "Seridó") & UF_ABBREV=="PB")
filter(matched_now, str_detect(MUN_NAME, "Seridó") & UF_ABBREV=="PB")

## manual
match_manu <- tribble(
  ~MUN_NAME, ~MUN_NAME_corr,
  "Embu", "Embu das Artes",
  "Fortaleza do Tabocão", "Tabocão",
  "São Valério da Natividade", "São Valério",
  "Campo de Santana", "Tacima",
  "Olho-D'água do Borges", "Olho d'Água do Borges",
  "Augusto Severo", "Campo Grande",
  "Seridó", "São Vicente do Seridó",
  "Santarém", "Joca Claudino") %>% 
  left_join(probs %>% 
              select(MUN_NAME, UF_ABBREV),
            by = c("MUN_NAME")) %>% 
  left_join(dat_Muni_prep,
            by =c("MUN_NAME_corr"="MUN_NAME", "UF_ABBREV"))

match_manu

## bind all
matched_final <- bind_rows(matched_D0 %>% 
                             mutate(is_direct_match=TRUE),
                           matched_D1,
                           matched_D2,
                           match_manu %>% 
                             rename(MUN_NAME_from_shp=MUN_NAME_corr)) %>% 
  rename(MUN_NAME_corrected=MUN_NAME_from_shp) %>% 
  mutate(is_direct_match=replace_na(is_direct_match, FALSE))

matched_final %>% 
  # filter(!is.na(MUN_NAME_corrected)) %>% 
  count(is_direct_match, is.na(MUN_NAME_corrected))

## clean final
matched_final_c <- matched_final %>% 
  rename(MUN_NAME_data_orig = MUN_NAME,
         MUN_NAME = MUN_NAME_corrected) %>% 
  mutate(MUN_NAME= if_else(is.na(MUN_NAME),
                                     MUN_NAME_data_orig,
                           MUN_NAME)) %>% 
  select(-is_direct_match) %>% 
  relocate(MUN_NAME, MUN_NAME_data_orig, MUN_CODE, UF_ABBREV) %>% 
  arrange(UF_ABBREV, MUN_NAME)

matched_final_c

################################
#'## Check results
################################

## CHECK: nas? OK!
matched_final_c %>% 
  filter(is.na(MUN_CODE))

## CHECK: all matched?
dat_Fisc_c %>% 
  anti_join(matched_final_c, by=c("MUN_NAME"))
all(matched_final_c$MUN_NAME==arrange(dat_Fisc_c2, UF_ABBREV, MUN_NAME)$MUN_NAME)

## CHECK: no dups?
matched_final_c %>% 
  add_count(MUN_CODE) %>% 
  filter(n>1)

################################
#'## Export data
################################

write_csv(matched_final_c,
          "02_In_Process/IMAFORA_municipios_modulos_fiscais_FirstSource_CLEAN.csv")

## READ AS:
## fiscal_dat <- read_csv("03_Final_data/IMAFORA_municipios_modulos_fiscais_CLEAN.csv")
