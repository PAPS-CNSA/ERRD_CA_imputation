rm = list(ls())

# Lancement

library(dplyr)
library(tidyr)
library(cnsa)
library(data.table)

source("Fonctions.R")

# Chargement et préparation de la table

liste <- readRDS("G:/08. DPE/11. STATISTIQUES/12. Bases de Données/05. CA&ERRD/Donnees/brut_synthese/CAERRD_LISTE_A.rds")

annees <- as.character(2017:2021)
liste_finess <- list()

for(annee in annees){
  
  nom_table <- names(liste[annee])
  table <- liste[[annee]]
  table <- chargement_annee(table, annee)
  liste_finess[[nom_table]] <- table

}

table_finess <- format_liste_a_vers_tablo(liste_finess)

# On renommme certaines variables

names(table_finess)[names(table_finess) == "ETP_AS_"] <- "ETP_AS"
names(table_finess)[names(table_finess) == "ETP_SG_"] <- "ETP_SG"
names(table_finess)[names(table_finess) == "SAL_AS_"] <- "SAL_AS"
names(table_finess)[names(table_finess) == "SAL_SG_"] <- "SAL_SG"

names(table_finess)[names(table_finess) == "ETP_AS__D"] <- "ETP_AS_D"
names(table_finess)[names(table_finess) == "ETP_SG__D"] <- "ETP_SG_D"
names(table_finess)[names(table_finess) == "SAL_AS__D"] <- "SAL_AS_D"
names(table_finess)[names(table_finess) == "SAL_SG__D"] <- "SAL_SG_D"

names(table_finess)[names(table_finess) == "ETP_AS__H"] <- "ETP_AS_H"
names(table_finess)[names(table_finess) == "ETP_SG__H"] <- "ETP_SG_H"
names(table_finess)[names(table_finess) == "SAL_AS__H"] <- "SAL_AS_H"
names(table_finess)[names(table_finess) == "SAL_SG__H"] <- "SAL_SG_H"

names(table_finess)[names(table_finess) == "ETP_AS__S"] <- "ETP_AS_S"
names(table_finess)[names(table_finess) == "ETP_SG__S"] <- "ETP_SG_S"
names(table_finess)[names(table_finess) == "SAL_AS__S"] <- "SAL_AS_S"
names(table_finess)[names(table_finess) == "SAL_SG__S"] <- "SAL_SG_S"

table_num <- table_finess %>% mutate_at(vars(-"FINESS", -"ANNEE", -"departement", -"statut"), as.numeric)
