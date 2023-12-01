####################################################### Imputations des variables comptes de résultat ######################################################################

# II. Variables comptes des charges et des produits

table_cptes <- table_num %>% select(FINESS, ANNEE, categetab, statut, matches("Financeur"), matches("TOT"), matches("COMPTE"))

# On supprime les valeurs négatives ou qui ne sont pas comprise dans un ratio contenu dans la fourchette [-500, 500].

# Comptes totaux

nom_var_cptes_tot <- c("TOT_REC_GRI_TARIF", "TOT_REC_GRII_AUTRE", "TOT_REC_GRIII_FIN_EXCE_AUT", "TOT_REC_GRIII_FIN", "TOT_REC_GRIII_EXCE", "TOT_REC_GRIII_AUTRE", "TOT_PRODUITS_REALISE_new",
                       "TOT_CHARGES_REALISE", "TOT_DEP_GI_EXPL_COUR", "TOT_DEP_GI_ACHAT", "TOT_DEP_GI_SERV_EXT", "TOT_DEP_GI_AUTR_SERV_EXT", "TOT__DEP_GII_PERSO", "TOT__DEP_GIII_STRUC", "TOT__DEP_GIII_SANS_DENOM",
                       "TOT__DEP_GIII_GEST_COUR", "TOT__DEP_GIII_FIN", "TOT__DEP_GIII_EXCEP", "TOT__DEP_GIII_DOT", "TOT_CHARGES_REALISE_new", "Financeur_Ass_Maladie_TOT", "Financeur_autre_TOT", "Financeur_Departement_TOT",
                       "Financeur_Etat_TOT", "Financeur_indetermine_TOT", "Financeur_Usager_TOT", "Fourn_stocks_TOT", "Impots_taxes_TOT", "Pdt_fin_TOT", "Personnel_TOT", "Tous_financeurs_TOT", 
                       "Autre_charges_TOT", "Autre_pdt_TOT")


for(var_cpte_tot in nom_var_cptes_tot){
  table_cptes <- table_cptes %>% mutate(!!var_cpte_tot := ifelse(EXCEDENT_TOT > -500*TOT_PRODUITS_REALISE & EXCEDENT_TOT < 500*TOT_PRODUITS_REALISE & !!sym(var_cpte_tot) > 0, !!sym(var_cpte_tot), NA))
}

nom_var_tot <- c("EXCEDENT_TOT", "TOT_PRODUITS_REALISE")

for(var_tot in nom_var_tot){
  table_cptes <- table_cptes %>% mutate(!!var_tot := ifelse(!is.na(table_cptes$TOT_CHARGES_REALISE), !!sym(var_tot), NA))
}

# Comptes soins 

nom_var_cptes_soins <- c("Financeur_Ass_Maladie_soins", "Financeur_autre_soins", "Financeur_Departement_soins", "Financeur_Etat_soins", "Financeur_Usager_soins", "TOT_DEP_REA_SOI",
                         "TOT_DEP_REA_SOI_AUTRE", "TOT_DEP_REA_SOI_EXT", "TOT_DEP_REA_SOI_FOURN", "TOT_DEP_REA_SOI_PERSO", "TOT_REC_REA_SOI_AUTRE", "TOT_REC_REA_SOI_FIN", "TOT_REC_REA_SOI_TARIF")

for(var_cpte_soins in nom_var_cptes_soins){
  table_cptes <- table_cptes %>% mutate(!!var_cpte_soins := ifelse(EXCEDENT_SOI > -500*TOT_REC_REA_SOI & EXCEDENT_SOI < 500*TOT_REC_REA_SOI & !!sym(var_cpte_tot) > 0, !!sym(var_cpte_tot), NA))
}

nom_var_soins <- c("EXCEDENT_SOI", "TOT_REC_REA_SOI")

for(var_soins in nom_var_soins){
  table_cptes <- table_cptes %>% mutate(!!var_soins := ifelse(!is.na(Financeur_Ass_Maladie_soins), !!sym(var_soins), NA))
}

# Comptes dépendance 

nom_var_cptes_dep <- c("Financeur_Ass_Maladie_dep", "Financeur_autre_dep", "Financeur_Departement_dep", "Financeur_Etat_dep", "Financeur_Usager_dep", "TOT_DEP_REA_DEP", "TOT_DEP_REA_DEP_AUTRE",
                       "TOT_DEP_REA_DEP_EXT", "TOT_DEP_REA_DEP_FOURN", "TOT_DEP_REA_DEP_PERSO", "TOT_REC_REA_DEP_AUTRE", "TOT_REC_REA_DEP_FIN", "TOT_REC_REA_DEP_TARIF")

for(var_cptes_dep in nom_var_cptes_dep){
  table_cptes <- table_cptes %>% mutate(!!var_cptes_dep := ifelse(EXCEDENT_DEP > -500*TOT_REC_REA_DEP & EXCEDENT_DEP < 500*TOT_REC_REA_DEP & !!sym(var_cptes_dep) > 0, !!sym(var_cptes_dep), NA))
}

nom_var_dep <- c("EXCEDENT_DEP", "TOT_REC_REA_DEP")

for(var_dep in nom_var_dep){
  table_cptes <- table_cptes %>% mutate(!!var_dep := ifelse(!is.na(Financeur_Ass_Maladie_dep), !!sym(var_dep), NA))
}

# Comptes hébergement 

nom_var_cptes_heb <- c("Financeur_Ass_Maladie_heb", "Financeur_autre_heb", "Financeur_Departement_heb", "Financeur_Etat_heb", "Financeur_Usager_heb", "TOT_DEP_REA_HEB_AUTRE", "TOT_DEP_REA_HEB_EXT", "TOT_DEP_REA_HEB_FOURN",
                       "TOT_DEP_REA_HEB_IMP", "TOT_DEP_REA_HEB_PERSO", "TOT_DEP_REA_HEB", "TOT_REC_REA_HEB_AUTRE", "TOT_REC_REA_HEB_FIN", "TOT_REC_REA_HEB_TARIF")

for(var_cptes_heb in nom_var_cptes_heb){
  table_cptes <- table_cptes %>% mutate(!!var_cptes_heb := ifelse(EXCEDENT_HEB > -500*TOT_REC_REA_HEB & EXCEDENT_HEB < 500*TOT_REC_REA_HEB & !!sym(var_cptes_heb) > 0, !!sym(var_cptes_heb), NA))
}

nom_var_heb <- c("EXCEDENT_HEB", "TOT_REC_REA_HEB")

for(var_heb in nom_var_heb){
  table_cptes <- table_cptes %>% mutate(!!var_heb := ifelse(!is.na(Financeur_Ass_Maladie_heb), !!sym(var_heb), NA))
}

# Comptes soins et dependance

nom_var_cptes_dep_soins <- c("Financeur_Ass_Maladie_dep_soins", "Financeur_autre_dep_soins", "Financeur_Departement_dep_soins", "Financeur_Etat_dep_soins", "Financeur_Usager_dep_soins", "TOT_DEP_REA_DEP_SOI_AUTRE",
                             "TOT_DEP_REA_DEP_SOI_EXT", "TOT_DEP_REA_DEP_SOI_FOURN", "TOT_DEP_REA_DEP_SOI_PERSO", "TOT_REC_REA_DEP_SOI_AUTRE", "TOT_REC_REA_DEP_SOI_FIN", "TOT_REC_REA_DEP_SOI_TARIF")

for(var_cptes_dep_soins in nom_var_cptes_dep_soins){
  table_cptes <- table_cptes %>% mutate(!!var_cptes_dep_soins := ifelse(!is.na(EXCEDENT_DEP) & !is.na(EXCEDENT_SOI) & !!sym(var_cptes_dep_soins) > 0, !!sym(var_cptes_dep_soins), NA))
}

# Tests

# TOT_PRODUITS_REALISE

test_TOT_PRODUITS_REALISE <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_PRODUITS_REALISE)

test_TOT_PRODUITS_REALISE <- test_TOT_PRODUITS_REALISE %>% group_by(ANNEE) %>% mutate(sum_TOT_PRODUITS_REALISE = sum(as.numeric(TOT_PRODUITS_REALISE), na.rm = T)
)

test_TOT_PRODUITS_REALISE_sum <- test_TOT_PRODUITS_REALISE %>% select(ANNEE, sum_TOT_PRODUITS_REALISE) %>% group_by(ANNEE) %>% unique()

# TOT_REC_GRI_TARIF

test_TOT_REC_GRI_TARIF <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_REC_GRI_TARIF)

test_TOT_REC_GRI_TARIF <- test_TOT_REC_GRI_TARIF %>% group_by(ANNEE) %>% mutate(sum_TOT_REC_GRI_TARIF = sum(as.numeric(TOT_REC_GRI_TARIF), na.rm = T)
)

test_TOT_REC_GRI_TARIF_sum <- test_TOT_REC_GRI_TARIF %>% select(ANNEE, sum_TOT_REC_GRI_TARIF) %>% group_by(ANNEE) %>% unique()

# TOT_REC_GRII_AUTRE : On observe une diminution de -6,1 % en 2018 et une augmentation de 11,1 % en 2022.

test_TOT_REC_GRII_AUTRE <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_REC_GRII_AUTRE)

test_TOT_REC_GRII_AUTRE <- test_TOT_REC_GRII_AUTRE %>% group_by(ANNEE) %>% mutate(sum_TOT_REC_GRII_AUTRE = sum(as.numeric(TOT_REC_GRII_AUTRE), na.rm = T)
)

test_TOT_REC_GRII_AUTRE_sum <- test_TOT_REC_GRII_AUTRE %>% select(ANNEE, sum_TOT_REC_GRII_AUTRE) %>% group_by(ANNEE) %>% unique()

# TOT_REC_GRIII_FIN_EXCE_AUT : On observe une augmentation de 26,4 % en 2018.

test_TOT_REC_GRIII_FIN_EXCE_AUT <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_REC_GRIII_FIN_EXCE_AUT)

test_TOT_REC_GRIII_FIN_EXCE_AUT <- test_TOT_REC_GRIII_FIN_EXCE_AUT %>% group_by(ANNEE) %>% mutate(sum_TOT_REC_GRIII_FIN_EXCE_AUT = sum(as.numeric(TOT_REC_GRIII_FIN_EXCE_AUT), na.rm = T)
)

test_TOT_REC_GRIII_FIN_EXCE_AUT_sum <- test_TOT_REC_GRIII_FIN_EXCE_AUT %>% select(ANNEE, sum_TOT_REC_GRIII_FIN_EXCE_AUT) %>% group_by(ANNEE) %>% unique()
View(test_TOT_REC_GRIII_FIN_EXCE_AUT_sum)

# TOT_REC_GRIII_FIN : On observe des augmentations/diminutions chaque année.

test_TOT_REC_GRIII_FIN <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_REC_GRIII_FIN)

test_TOT_REC_GRIII_FIN <- test_TOT_REC_GRIII_FIN %>% group_by(ANNEE) %>% mutate(sum_TOT_REC_GRIII_FIN = sum(as.numeric(TOT_REC_GRIII_FIN), na.rm = T)
)

test_TOT_REC_GRIII_FIN_sum <- test_TOT_REC_GRIII_FIN %>% select(ANNEE, sum_TOT_REC_GRIII_FIN) %>% group_by(ANNEE) %>% unique()
View(test_TOT_REC_GRIII_FIN_sum)

# TOT_REC_GRIII_EXCE : On observe une augmentation de 31,9 % en 2018 et de 23,8 % en 2020.

test_TOT_REC_GRIII_EXCE <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_REC_GRIII_EXCE)

test_TOT_REC_GRIII_EXCE <- test_TOT_REC_GRIII_EXCE %>% group_by(ANNEE) %>% mutate(sum_TOT_REC_GRIII_EXCE = sum(as.numeric(TOT_REC_GRIII_EXCE), na.rm = T)
)

test_TOT_REC_GRIII_EXCE_sum <- test_TOT_REC_GRIII_EXCE %>% select(ANNEE, sum_TOT_REC_GRIII_EXCE) %>% group_by(ANNEE) %>% unique()
View(test_TOT_REC_GRIII_EXCE_sum)

# TOT_REC_GRIII_AUTRE : on observe une augmentation de 22,3 % en 2021 et de 20,5 % en 2018.

test_TOT_REC_GRIII_AUTRE <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_REC_GRIII_AUTRE)

test_TOT_REC_GRIII_AUTRE <- test_TOT_REC_GRIII_AUTRE %>% group_by(ANNEE) %>% mutate(sum_TOT_REC_GRIII_AUTRE = sum(as.numeric(TOT_REC_GRIII_AUTRE), na.rm = T)
)

test_TOT_REC_GRIII_AUTRE_sum <- test_TOT_REC_GRIII_AUTRE %>% select(ANNEE, sum_TOT_REC_GRIII_AUTRE) %>% group_by(ANNEE) %>% unique()
View(test_TOT_REC_GRIII_AUTRE_sum)

# TOT_PRODUITS_REALISE_new

test_TOT_PRODUITS_REALISE_new <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_PRODUITS_REALISE_new)

test_TOT_PRODUITS_REALISE_new <- test_TOT_PRODUITS_REALISE_new %>% group_by(ANNEE) %>% mutate(sum_TOT_PRODUITS_REALISE_new = sum(as.numeric(TOT_PRODUITS_REALISE_new), na.rm = T)
)

test_TOT_PRODUITS_REALISE_new_sum <- test_TOT_PRODUITS_REALISE_new %>% select(ANNEE, sum_TOT_PRODUITS_REALISE_new) %>% group_by(ANNEE) %>% unique()
View(test_TOT_PRODUITS_REALISE_new_sum)

# TOT_CHARGES_REALISE

TOT_CHARGES_REALISE <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_CHARGES_REALISE)

TOT_CHARGES_REALISE <- TOT_CHARGES_REALISE %>% group_by(ANNEE) %>% mutate(sum_TOT_CHARGES_REALISE = sum(as.numeric(TOT_CHARGES_REALISE), na.rm = T)
)

TOT_CHARGES_REALISE_sum <- TOT_CHARGES_REALISE %>% select(ANNEE, sum_TOT_CHARGES_REALISE) %>% group_by(ANNEE) %>% unique()
View(TOT_CHARGES_REALISE_sum)

# TOT_DEP_GI_EXPL_COUR : on observe une augmentation de 15,3 % en 2018.

TOT_DEP_GI_EXPL_COUR <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_DEP_GI_EXPL_COUR)

TOT_DEP_GI_EXPL_COUR <- TOT_DEP_GI_EXPL_COUR %>% group_by(ANNEE) %>% mutate(sum_TOT_DEP_GI_EXPL_COUR = sum(as.numeric(TOT_DEP_GI_EXPL_COUR), na.rm = T)
)

TOT_DEP_GI_EXPL_COUR_sum <- TOT_DEP_GI_EXPL_COUR %>% select(ANNEE, sum_TOT_DEP_GI_EXPL_COUR) %>% group_by(ANNEE) %>% unique()
View(TOT_DEP_GI_EXPL_COUR_sum)

# TOT_DEP_GI_EXPL_COUR : on observe une augmentation de 15,3 % en 2018.

TOT_DEP_GI_EXPL_COUR <- table_cptes %>% select(FINESSET_ID_CR_SF, ANNEE, TOT_DEP_GI_EXPL_COUR)

TOT_DEP_GI_EXPL_COUR <- TOT_DEP_GI_EXPL_COUR %>% group_by(ANNEE) %>% mutate(sum_TOT_DEP_GI_EXPL_COUR = sum(as.numeric(TOT_DEP_GI_EXPL_COUR), na.rm = T)
)

TOT_DEP_GI_EXPL_COUR_sum <- TOT_DEP_GI_EXPL_COUR %>% select(ANNEE, sum_TOT_DEP_GI_EXPL_COUR) %>% group_by(ANNEE) %>% unique()
View(TOT_DEP_GI_EXPL_COUR_sum)




"TOT_DEP_GI_ACHAT", "TOT_DEP_GI_SERV_EXT", "TOT_DEP_GI_AUTR_SERV_EXT", "TOT__DEP_GII_PERSO", "TOT__DEP_GIII_STRUC", "TOT__DEP_GIII_SANS_DENOM",
"TOT__DEP_GIII_GEST_COUR", "TOT__DEP_GIII_FIN", "TOT__DEP_GIII_EXCEP", "TOT__DEP_GIII_DOT", "TOT_CHARGES_REALISE_new", "Financeur_Ass_Maladie_TOT", "Financeur_autre_TOT", "Financeur_Departement_TOT",
"Financeur_Etat_TOT", "Financeur_indetermine_TOT", "Financeur_Usager_TOT", "Fourn_stocks_TOT", "Impots_taxes_TOT", "Pdt_fin_TOT", "Personnel_TOT", "Tous_financeurs_TOT", 
"Autre_charges_TOT", "Autre_pdt_TOT", "EXCEDENT_TOT"