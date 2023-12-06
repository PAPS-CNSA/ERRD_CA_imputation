Ctrl_cohe <- function(type_rapport, annee, type_donnees){
  
  # Intro : chargement de la base pour l'année t (on privilégie type_rapport = "CAERRD" et type_donnees = "SYNTHESE")
  
  df <- caerrd_charger_donnees(type_rapport = type_rapport, annee = annee, type_donnees = type_donnees)
  
  
  #I. Variables comptables
  
  # Il n'y a pas de variable négative (hors variable FINESS - ce qui est impossible soit-dit en passant)
  
  df <- df %>% mutate_at(vars(-FINESS), ~ifelse(. < 0, NA, .))
  
  # La variable TOT_REC_GRI ne peut pas être nulle, autrement elle devient NA, même chose pour les variables TOT_REC_REA_x_TARIF
  
  df <- df %>% mutate(TOT_REC_GRI = ifelse(TOT_REC_GRI == 0, NA, TOT_REC_GRI))
  
  sec_tar <- c("SOI", "DEP", "HEB")
  
  for(sec in sec_tar){
    name_col <- paste0("TOT_REC_REA_", sec, "_TARIF")
    df <- df %>% mutate(!!name_col := ifelse(!!sym(name_col) == 0, NA, !!sym(name_col)))
  }
  
  # Les variables "Financeur_x" doivent être vides si la variable groupe I/TOT_REC_REA_x_TARIF est vide
  
  df <- df %>% mutate_at(vars(matches("Financeur") & matches("TOT")), ~ifelse(is.na(df$TOT_REC_GRI), NA, .))
  df <- df %>% mutate_at(vars(matches("Financeur") & matches("soins")), ~ifelse(is.na(df$TOT_REC_REA_SOI_TARIF), NA, .))
  df <- df %>% mutate_at(vars(matches("Financeur") & matches("dep")), ~ifelse(is.na(df$TOT_REC_REA_DEP_TARIF), NA, .))
  df <- df %>% mutate_at(vars(matches("Financeur") & matches("heb")), ~ifelse(is.na(df$TOT_REC_REA_HEB_TARIF), NA, .))
  
  # la variable TOT_REC_GRII ne doit pas être nulle
  
  df <- df %>% mutate(TOT_REC_GRII = ifelse(TOT_REC_GRII == 0, NA, TOT_REC_GRII))
  
  # II. Variables personnel
  
  exps_regs <- c("DIR", "SG", "ANIM", "ASH", "AS", "PSY", "INF", "AUX", "PHA", "MED", "GES", "RES", "EDU", "PARA", "AUT")
  
  nom_var_etp <- c("ETP_DIR", "ETP_SG", "ETP_ANIM", "ETP_ASH", "ETP_AS", "ETP_PSY", "ETP_INF", "ETP_AUX", "ETP_PHA", "ETP_MED", "ETP_GES", "ETP_RES", "ETP_EDU", "ETP_PARA", "ETP_AUT")
  nom_var_sal <- c("SAL_DIR", "SAL_SG", "SAL_ANIM", "SAL_ASH", "SAL_AS", "SAL_PSY", "SAL_INF", "SAL_AUX", "SAL_PHA", "SAL_MED", "SAL_GES", "SAL_RES", "SAL_EDU", "SAL_PARA", "SAL_AUT")
  
  for(etp in nom_var_etp){
    for(sal in nom_var_sal){
      for(exp_reg in exps_regs){
        if(any(grepl(exp_reg, nom_var_etp) & grepl(exp_reg, nom_var_sal))){
          
          # Il ne doit pas y avoir de valeurs nulles pour les AS et les ASH.
          
          if(exp_reg %in% c("AS", "ASH")){
            
            df <- df %>% mutate(!!etp := ifelse(!!sym(etp) == 0, NA, !!sym(etp)))
            df <- df %>% mutate(!!sal := ifelse(!!sym(sal) == 0, NA, !!sym(sal)))
            
          }
          
          # On supprime les valeurs sal_x > 0 quand sal_x < etp_x et on supprime les valeurs etp_x > 0 quand etp_x > sal_x.
          # On supprime aussi les valeurs négatives
          
          df <- df %>% mutate(
            
            !!etp := ifelse(!!sym(etp) > 0 & !!sym(etp) > !!sym(sal) | !!sym(etp) < 0, NA, !!sym(etp)),
            !!sal := ifelse(!!sym(sal) > 0 & !!sym(etp) > !!sym(sal) | !!sym(sal) < 0, NA, !!sym(sal))
            
          )
          
          # Quand la variable ETP_x est égale à zéro mais que la variable SAL_x est supérieure à zéro, la variable ETP_x est vidée (et réciproquement).
          
          df <- df %>% mutate(!!etp := ifelse(!!sym(etp) == 0 & !!sym(sal) > 0, NA, !!sym(etp)),
                                      !!sal := ifelse(!!sym(sal) == 0 & !!sym(etp) > 0, NA, !!sym(sal))
          )
          
        }
      }
    }
  }
  
}