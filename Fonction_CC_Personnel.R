# Variables ETP et salaires

CC_var_perso(df_num){
  
df_ETP <- df_num %>% select(FINESS, ANNEE, categetab, statut, matches("ETP"), matches("SAL"))

exps_regs <- c("DIR", "SG", "ANIM", "ASH", "AS", "PSY", "INF", "AUX", "PHA", "MED", "GES", "RES", "EDU", "PARA", "AUT")

nom_var_etp <- c("ETP_DIR", "ETP_SG", "ETP_ANIM", "ETP_ASH", "ETP_AS", "ETP_PSY", "ETP_INF", "ETP_AUX", "ETP_PHA", "ETP_MED", "ETP_GES", "ETP_RES", "ETP_EDU", "ETP_PARA", "ETP_AUT")
nom_var_sal <- c("SAL_DIR", "SAL_SG", "SAL_ANIM", "SAL_ASH", "SAL_AS", "SAL_PSY", "SAL_INF", "SAL_AUX", "SAL_PHA", "SAL_MED", "SAL_GES", "SAL_RES", "SAL_EDU", "SAL_PARA", "SAL_AUT")

for(etp in nom_var_etp){
  for(sal in nom_var_sal){
    for(exp_reg in exps_regs){
      if(any(grepl(exp_reg, nom_var_etp) & grepl(exp_reg, nom_var_sal))){
        
        # Il ne doit pas y avoir de valeurs nulles pour les AS et les ASH.
        
        if(exp_reg %in% c("AS", "ASH")){
          
          df_ETP <- df_ETP %>% mutate(!!etp := ifelse(!!sym(etp) == 0, NA, !!sym(etp)))
          df_ETP <- df_ETP %>% mutate(!!sal := ifelse(!!sym(sal) == 0, NA, !!sym(sal)))
    
        }
        
        # On supprime les valeurs sal_x > 0 quand sal_x < etp_x et on supprime les valeurs etp_x > 0 quand etp_x > sal_x.
        # On supprime aussi les valeurs négatives
        
        df_ETP <- df_ETP %>% mutate(
           
        !!etp := ifelse(!!sym(etp) > 0 & !!sym(etp) > !!sym(sal) | !!sym(etp) < 0, NA, !!sym(etp)),
        !!sal := ifelse(!!sym(sal) > 0 & !!sym(etp) > !!sym(sal) | !!sym(sal) < 0, NA, !!sym(sal))
        
        )
        
        # Quand la variable ETP_x est égale à zéro mais que la variable SAL_x est supérieure à zéro, la variable ETP_x est vidée (et réciproquement).
        
        df_ETP <- df_ETP %>% mutate(!!etp := ifelse(!!sym(etp) == 0 & !!sym(sal) > 0, NA, !!sym(etp)),
                                          !!sal := ifelse(!!sym(sal) == 0 & !!sym(etp) > 0, NA, !!sym(sal))
        )
        
      }
    }
  }
}

df_ETP <- enlever_valeurs_extremes_t(df_ETP, p_min = 0.00, p_max = 0.999)

}