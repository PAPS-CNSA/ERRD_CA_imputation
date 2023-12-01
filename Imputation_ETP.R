####################################################### Imputations des variables ETP et SAL ######################################################################

# I. Variables ETP et salaires

table_ETP <- table_num %>% select(FINESS, ANNEE, categetab, statut, matches("ETP"), matches("SAL"))

# On supprime les valeurs sal_x > 0 quand sal_x < etp_x et on supprime les valeurs etp_x > 0 quand etp_x > sal_x
# On supprime aussi les valeurs négatives

exps_regs <- c("DIR", "SG", "ANIM", "ASH", "AS", "PSY", "INF", "AUX", "PHA", "MED")

nom_var_etp <- c("ETP_DIR", "ETP_SG", "ETP_ANIM", "ETP_ASH", "ETP_AS", "ETP_PSY", "ETP_INF", "ETP_AUX", "ETP_PHA", "ETP_MED")
nom_var_sal <- c("SAL_DIR", "SAL_SG", "SAL_ANIM", "SAL_ASH", "SAL_AS", "SAL_PSY", "SAL_INF", "SAL_AUX", "SAL_PHA", "SAL_MED")

for(etp in nom_var_etp){
  for(sal in nom_var_sal){
    for(exp_reg in exps_regs){
      if(any(grepl(exp_reg, nom_var_etp) & grepl(exp_reg, nom_var_sal))){
        
        table_ETP <- table_ETP %>% mutate(
          !!etp := ifelse(!!sym(etp) > 0 & !!sym(etp) > !!sym(sal) | !!sym(etp) < 0, NA, !!sym(etp)),
          !!sal := ifelse(!!sym(sal) > 0 & !!sym(etp) > !!sym(sal) | !!sym(sal) < 0, NA, !!sym(sal))
        )
        table_ETP <- table_ETP %>% mutate(!!etp := ifelse(!!sym(etp) == 0 & !!sym(sal) > 0, NA, !!sym(etp)),
                                          !!sal := ifelse(!!sym(sal) == 0 & !!sym(etp) > 0, NA, !!sym(sal))
        )
        
        # Il ne doit pas y avoir de valeurs négatives pour les AS et les ASH
        
        if(etp %in% c("AS", "ASH")){
        table_ETP <- table_ETP %>% mutate(!!etp := ifelse(!!sym(etp) == 0, NA, !!sym(etp)))
        }
      }
    }
  }
}

table_ETP <- enlever_valeurs_extremes_t(table_ETP, p_min = 0.00, p_max = 0.999)

# Tests

## SG :  il n'y a plus que 3099 observations non vides pour les ETP et 2699 pour les salaires. Le total des ETP diminuent à partir de 2020. Les salaires augmentent significativement en 2018, +23,9 %.

test_SG <- table_ETP  %>% select(FINESS, ANNEE, ETP_SG, SAL_SG)

test_SG <- test_SG %>% group_by(ANNEE) %>% mutate(sum_ETP_SG = sum(as.numeric(ETP_SG), na.rm = T)
                                                  , sum_SAL_SG = sum(as.numeric(SAL_SG), na.rm = T)
)

test_SG_sum <- test_SG %>% select(ANNEE, sum_ETP_SG, sum_SAL_SG) %>% group_by(ANNEE) %>% unique()

## DIR : il n'y a plus que 3138 observations non vides pour les ETP et 2704 pour les salaires. Le total des ETP diminuent à partir de 2020. Les salaires augmentent significativement en 2018, +21,6 %.

test_DIR <- table_ETP  %>% select(FINESS, ANNEE, ETP_DIR, SAL_DIR)

test_DIR <- test_DIR %>% group_by(ANNEE) %>% mutate(sum_ETP_DIR = sum(as.numeric(ETP_DIR), na.rm = T)
                                                    , sum_SAL_DIR = sum(as.numeric(SAL_DIR), na.rm = T)
)

test_DIR_sum <- test_DIR %>% select(ANNEE, sum_ETP_DIR, sum_SAL_DIR) %>% group_by(ANNEE) %>% unique()

## ANIM : il n'y a plus que 3072 observations non vides pour les ETP et 2688 pour les salaires.  Le total des ETP diminuent à partir de 2020. Les salaires augmentement significativement en 2018, +16,7 %. Ils
## diminuent aussi en 2020, -3,9 %.

test_ANIM <- table_ETP  %>% select(FINESS, ANNEE, ETP_ANIM, SAL_ANIM)

test_ANIM <- test_ANIM %>% group_by(ANNEE) %>% mutate(sum_ETP_ANIM = sum(as.numeric(ETP_ANIM), na.rm = T)
                                                      , sum_SAL_ANIM = sum(as.numeric(SAL_ANIM), na.rm = T)
)

test_ANIM_sum <- test_ANIM %>% select(ANNEE, sum_ETP_ANIM, sum_SAL_ANIM) %>% group_by(ANNEE) %>% unique()

## ASH : il n'y a plus que 3051 observations non vides pour les ETP et 2665 pour les salaires. Le total des ETP diminuent en 2022. Les salaires augmentent significativement en 2018, +17,9 %.

test_ASH <- table_ETP  %>% select(FINESS, ANNEE, ETP_ASH, SAL_ASH)

test_ASH <- test_ASH %>% group_by(ANNEE) %>% mutate(sum_ETP_ASH = sum(as.numeric(ETP_ASH), na.rm = T)
                                                    , sum_SAL_ASH = sum(as.numeric(SAL_ASH), na.rm = T)
)

test_ASH_sum <- test_ASH %>% select(ANNEE, sum_ETP_ASH, sum_SAL_ASH) %>% group_by(ANNEE) %>% unique()

## AS : il n'y a plus que 3040 observations non vides pour les ETP et 2662 pour les salaires. Le total des ETP diminuent en 2022. Les salaires augmentent significativement en 2018, +24,6 %.

test_AS <- table_ETP  %>% select(FINESS, ANNEE, ETP_AS, SAL_AS)

test_AS <- test_AS %>% group_by(ANNEE) %>% mutate(sum_ETP_AS = sum(as.numeric(ETP_AS), na.rm = T)
                                                  , sum_SAL_AS = sum(as.numeric(SAL_AS), na.rm = T)
)

test_AS_sum <- test_AS %>% select(ANNEE, sum_ETP_AS, sum_SAL_AS) %>% group_by(ANNEE) %>% unique()

## PSY : il n'y a plus que 2992 observations non vides pour les ETP et 2654 pour les salaires.

test_PSY <- table_ETP  %>% select(FINESS, ANNEE, ETP_PSY, SAL_PSY)

test_PSY <- test_PSY %>% group_by(ANNEE) %>% mutate(sum_ETP_PSY = sum(as.numeric(ETP_PSY), na.rm = T)
                                                    , sum_SAL_PSY = sum(as.numeric(SAL_PSY), na.rm = T)
)

test_PSY_sum <- test_PSY %>% select(ANNEE, sum_ETP_PSY, sum_SAL_PSY) %>% group_by(ANNEE) %>% unique()

## INF : il n'y a plus que 2973 observations non vides pour les ETP et 2647 pour les salaires. On observe une nette augmentation du total des ETP en 2018, +24,5 %. Les salaires augmentent significativement en 2018, +20,8 %.

test_INF <- table_ETP  %>% select(FINESS, ANNEE, ETP_INF, SAL_INF)

test_INF <- test_INF %>% group_by(ANNEE) %>% mutate(sum_ETP_INF = sum(as.numeric(ETP_INF), na.rm = T)
                                                    , sum_SAL_INF = sum(as.numeric(SAL_INF), na.rm = T)
)

test_INF_sum <- test_INF %>% select(ANNEE, sum_ETP_INF, sum_SAL_INF) %>% group_by(ANNEE) %>% unique()

## AUX : il n'y a plus que 2888 observations non vides pour les ETP et 2626 pour les salaires. On observe une nette diminution du total des ETP en 2020, -14,8 %.

test_AUX <- table_ETP  %>% select(FINESS, ANNEE, ETP_AUX, SAL_AUX)

test_AUX <- test_AUX %>% group_by(ANNEE) %>% mutate(sum_ETP_AUX = sum(as.numeric(ETP_AUX), na.rm = T)
                                                    , sum_SAL_AUX = sum(as.numeric(SAL_AUX), na.rm = T)
)

test_AUX_sum <- test_AUX %>% select(ANNEE, sum_ETP_AUX, sum_SAL_AUX) %>% group_by(ANNEE) %>% unique()

## PHA : il n'y a plus que 2666 observations non vides pour les ETP et 2607 pour les salaires. Les salaires augmentent significativement en 2018, +24,2 %.

test_PHA <- table_ETP  %>% select(FINESS, ANNEE, ETP_PHA, SAL_PHA)

test_PHA <- test_PHA %>% group_by(ANNEE) %>% mutate(sum_ETP_PHA = sum(as.numeric(ETP_PHA), na.rm = T)
                                                    , sum_SAL_PHA = sum(as.numeric(SAL_PHA), na.rm = T)
)

test_PHA_sum <- test_PHA %>% select(ANNEE, sum_ETP_PHA, sum_SAL_PHA) %>% group_by(ANNEE) %>% unique()

## MED : il n'y a plus que 2599 observations non vides pour les ETP et 2599 pour les salaires. Les salaires augmentement significativement en 2018, +21,3 %. Ils
## augmentent aussi en 2019 (+13,9 %) et en 2021 (+10,2 %).


test_MED <- table_ETP  %>% select(FINESS, ANNEE, ETP_MED, SAL_MED)

test_MED <- test_MED %>% group_by(ANNEE) %>% mutate(sum_ETP_MED = sum(as.numeric(ETP_MED), na.rm = T)
                                                    , sum_SAL_MED = sum(as.numeric(SAL_MED), na.rm = T)
)

test_MED_sum <- test_MED %>% select(ANNEE, sum_ETP_MED, sum_SAL_MED) %>% group_by(ANNEE) %>% unique()
