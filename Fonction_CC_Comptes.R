CC_var_compt <- function(df_num){
  
  df_cptes <- df_num %>% select(FINESS, ANNEE, categetab, statut, matches("Financeur"), matches("TOT"), matches("COMPTE"))
  
  # Il n'y a pas de variables négatives (hors variables FINESS, ANNEE, categetab et statut)
  
  df_cptes_na <- df_cptes %>% mutate_at(vars(-FINESS, -ANNEE, -categetab, -statut), ~ifelse(. < 0, NA, .))
  
  # On enlève les Ehpad du privé lucratif avant 2023 (car ils sont trop peu nombreux à avoir renseigné les produits/charges de la section hébergement)
  
  df_cptes_na <- df_cptes_na %>% filter(!(as.numeric(ANNEE) <= 2022 & statut == "PriveLucratif"))
  
  # La variable TOT_REC_GRI ne peut pas être nulle, autrement elle devient NA, même chose pour les variables TOT_REC_REA_x_TARIF
  
  df_cptes_na <- df_cptes_na %>% mutate(TOT_REC_GRI = ifelse(TOT_REC_GRI == 0, NA, TOT_REC_GRI))
  
  sec_tar <- c("SOI", "DEP", "HEB")
  
  for(sec in sec_tar){
    name_col <- paste0("TOT_REC_REA_", sec, "_TARIF")
    df_cptes_na <- df_cptes_na %>% mutate(!!name_col := ifelse(!!sym(name_col) == 0, NA, !!sym(name_col)))
  }
  
  # Les variables "Financeur_x" doivent être vides si la variable groupe I/TOT_REC_REA_x_TARIF est vide
  
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("TOT")), ~ifelse(is.na(table_cptes_na$TOT_REC_GRI), NA, .))
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("soins")), ~ifelse(is.na(table_cptes_na$TOT_REC_REA_SOI_TARIF), NA, .))
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("dep")), ~ifelse(is.na(table_cptes_na$TOT_REC_REA_DEP_TARIF), NA, .))
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("heb")), ~ifelse(is.na(table_cptes_na$TOT_REC_REA_HEB_TARIF), NA, .))
  
  # la variable TOT_REC_GRII ne doit pas être nulle
  
  df_cptes_na <- df_cptes_na %>% mutate(TOT_REC_GRII = ifelse(TOT_REC_GRII == 0, NA, TOT_REC_GRII))
  
}

