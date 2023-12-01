################ Fonctions pour charger les tableaux/préparer les données

chargement_annee <- function(liste, annee) {
  
  print(paste0("Début jointure avec la base Finess pour l'année ", as.character(annee)))
  
  load("G:/08. DPE/11. STATISTIQUES/12. Bases de Données/04. FINESS/Base_Finess.RData")
  finess <- base_finess_reduite[[as.numeric(annee)]]
  finess <- finess %>% select(nofinesset, departement, categetab, capinstot, statut)
  
  finess <- data.table(finess)
  finess <- finess[, places := sum(capinstot, na.rm = T), by = c("nofinesset", "departement", "categetab", "statut")]
  
  finess <- finess %>% select(-capinstot) %>% unique()
  liste <- liste %>% right_join(finess, by = c("FINESS" = "nofinesset"))
  
  print("Fin jointure avec la base Finess")
  
  return(liste)
}

enlever_valeurs_extremes_t <- function(tableau, p_min, p_max) {
  resultat <- tableau
  numeric_cols <- colnames(resultat)[sapply(resultat, is.numeric)]
  bounds <- lapply(numeric_cols, function(col) {
    lower_bound <- quantile(resultat[[col]], probs = p_min, na.rm = TRUE)
    upper_bound <- quantile(resultat[[col]], probs = p_max, na.rm = TRUE)
    c(lower_bound, upper_bound)
  })
  names(bounds) <- numeric_cols
  
  for (col in numeric_cols) {
    resultat <- resultat %>% 
      mutate(across(all_of(col), 
                    ~ ifelse(is.na(.) | (. >= bounds[[col]][1] & . <= bounds[[col]][2]), 
                             ., NA), 
                    .names = "{.col}"))
  }
  
  return(resultat)
}