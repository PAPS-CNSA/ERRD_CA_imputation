CC_var_compt <- function(type_rapport, annee, type_donnees){
  
  df_num <- caerrd_charger_donnees(type_rapport = type_rapport, annee = annee, type_donnees = type_donnees)
  
  df_cptes <- df_num %>% select(FINESS, matches("Financeur"), matches("TOT"), matches("COMPTE"))
  
  # Il n'y a pas de variables négatives (hors variables FINESS, ANNEE, categetab et statut)
  
  df_cptes_na <- df_cptes %>% mutate_at(vars(-FINESS), ~ifelse(. < 0, NA, .))
  
  # La variable TOT_REC_GRI ne peut pas être nulle, autrement elle devient NA, même chose pour les variables TOT_REC_REA_x_TARIF
  
  df_cptes_na <- df_cptes_na %>% mutate(TOT_REC_GRI = ifelse(TOT_REC_GRI == 0, NA, TOT_REC_GRI))
  
  sec_tar <- c("SOI", "DEP", "HEB")
  
  for(sec in sec_tar){
    name_col <- paste0("TOT_REC_REA_", sec, "_TARIF")
    df_cptes_na <- df_cptes_na %>% mutate(!!name_col := ifelse(!!sym(name_col) == 0, NA, !!sym(name_col)))
  }
  
  # Les variables "Financeur_x" doivent être vides si la variable groupe I/TOT_REC_REA_x_TARIF est vide
  
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("TOT")), ~ifelse(is.na(df_cptes_na$TOT_REC_GRI), NA, .))
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("soins")), ~ifelse(is.na(df_cptes_na$TOT_REC_REA_SOI_TARIF), NA, .))
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("dep")), ~ifelse(is.na(df_cptes_na$TOT_REC_REA_DEP_TARIF), NA, .))
  df_cptes_na <- df_cptes_na %>% mutate_at(vars(matches("Financeur") & matches("heb")), ~ifelse(is.na(df_cptes_na$TOT_REC_REA_HEB_TARIF), NA, .))
  
  # la variable TOT_REC_GRII ne doit pas être nulle
  
  df_cptes_na <- df_cptes_na %>% mutate(TOT_REC_GRII = ifelse(TOT_REC_GRII == 0, NA, TOT_REC_GRII))
  
}