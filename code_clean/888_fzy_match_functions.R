require(fuzzyjoin)

#' fuzzy matching
fzy_match_fuzzy_here <- function(df_unmatched,
                                 df_matched= NULL,
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

#' clean data
#' @param df output of fzy_match_fuzzy_here
fzy_matches_clean <- function(df){
  if(any(df$n_matches>1)) warning("Multiple matches!? Clean first")
  df %>% 
    select(MUN_NAME=MUN_NAME.x,
           UF_ABBREV=UF_ABBREV.x,
           MUN_CODE,
           MUN_NAME_from_shp=MUN_NAME.y,
           Modulo_fiscal_ha)
}
