####################################
####   FONCTION table_auto()    ####
####################################

## PACKAGES ----

# library(tidyverse)
# library(openxlsx)
# library(haven)
# library(gt)
# library(survey)
# library(srvyr)

## MESSAGES ----

print("DERNIERES MÀJ : ")
print("MAJ : 05/08/2025")
print("Ajout des graphiques :")
print("Option view pour choisir de montrer des graphiques ou des tables (ex-view_html)")
print("")
print("MAJ : 24/07/2025")
print("Ajout de vars_num :")
print("Quelques indicateurs pour les variables numériques")


## FONCTION ----

# Lancer la fonction suivante pour pourvoir l'appeler dans vos prochains scripts :

table_auto <- function(data,                     # Un data.frame
                       vars           = NULL,    # Un vecteur avec les noms des variables d'intérêts
                       vars_num       = NULL,    # Un vecteur avec les noms des variables d'intérêts numériques
                       var_col        = NULL,    # Variable à croiser avec celles du vecteur
                       var_weight     = NULL,    # Variable de pondération, sinon = NULL
                       weight_norm    = FALSE,   # Normaliser la pondération
                       table_type     = "all",   # Type de table : "all", "eff", "row", "col", "mix"
                       useNA          = TRUE,    # TRUE/FALSE : Ajout des valeurs manquantes
                       exclude        = NULL,    # Un vecteur avec les noms des modalités à exclure des tables
                       use_test       = "chi2",  # Type de test : "chi2", "fisher", "chi2_noponder", "no"
                       arrondi        = 2,       # Nombre de chiffres après la virgule
                       use_labels     = "no",    # Utiliser les labels : "no", "yes", "both"
                       add_blank_rows = TRUE,    # TRUE/FALSE : Ajout d'une ligne vide entre les variables
                       eff_in_name    = "no",    # Ajout des effectifs dans les noms des modalités : "yes","noponder", "no"
                       view           = "table", # Choix de l'affichage : "table", "graph", ou "no"
                       view_html      = FALSE,   # TRUE/FALSE : Visualisation HTML des tableaux à partir de table_type
                       excel_export   = FALSE,   # TRUE/FALSE : Création d'un fichier excel et son chemin
                       excel_filepath = "./table_auto.xlsx" # Chemin vers le fichier excel
){
  
  #############################
  
  
  
  #############################
  ### OPTIONS                ----
  # Ecriture scientifique
  options(scipen=9999)
  
  ### GESTION LIBRARY        ----
  # Liste des packages à charger
  packages <- c("tidyverse", "openxlsx", "haven", "gt", "survey", "srvyr")
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  #############################
  
  
  
  #############################
  ### GESTION DES ERREURS    ----
  
  # Vérification du parametre table_type
  if (table_type != "eff" && table_type != "row" && table_type != "col" && table_type != "all" && table_type != "mix") {
    stop("Erreur : table_type doit être 'eff', 'row', 'col', 'all' ou 'mix' ")
  }
  
  # Vérification du parametre use_labels
  if (use_labels != "no" && use_labels != "yes" && use_labels != "both") {
    stop("Erreur : use_labels doit être 'no', 'yes', ou 'both' ")
  }
  
  # Vérification de parametre eff_in_name
  if (eff_in_name != "no" && eff_in_name != "yes" && eff_in_name != "noponder") {
    stop("Erreur : eff_in_name doit être 'no', 'yes', ou 'noponder'")
  }
  
  # Vérification du parametre use_test
  if (use_test != "chi2" && use_test != "fisher" && use_test != "chi2_noponder" && use_test != "no") {
    stop("Erreur : use_test doit être 'chi2', 'fisher', 'chi2_noponder' ou 'no' ")
  }
  
  # Vérification de parametre useNA
  if (useNA != TRUE && useNA != FALSE) {
    stop("Erreur : useNA doit être TRUE ou FALSE")
  }
  
  # Vérification de parametre add_blank_rows
  if (add_blank_rows != TRUE && add_blank_rows != FALSE) {
    stop("Erreur : add_blank_rows doit être TRUE ou FALSE")
  }
  
  # Vérification de parametre excel_export
  if (excel_export != TRUE && excel_export != FALSE) {
    stop("Erreur : excel_export doit être TRUE ou FALSE")
  }
  
  # Vérification de parametre weight_norm
  if (weight_norm != TRUE && weight_norm != FALSE) {
    stop("Erreur : weight_norm doit être TRUE ou FALSE")
  }
  
  # Vérification du parametre view
  if (view != "no" && view != "table" && view != "graph") {
    stop("Erreur : view doit être 'no', 'table', ou 'graph' ")
  }
  
  
  # Vérification que les variables numériques 
  if(is.null(vars_num) == F){
    # Gestion des variables pas numériques
    tryCatch(                
      expr = {                      
        data2 = data |> 
          mutate(across(matches(vars_num), as.character)) |> 
          mutate(across(matches(vars_num), as.numeric))
      },
      warning = function(w){       
        print("Attention : dans vars_num, des modalités ne sont pas numériques")
      }
    )
    # On passe en numérique
    data = data |> 
      mutate(across(matches(vars_num), as.character)) |> 
      mutate(across(matches(vars_num), as.numeric))
  } # Fin vérif' numerique
  #############################
  
  
  
  #############################
  ### GESTION DES PARAMETRES ----
  
  # Variable pondération : ----
  if(is.null(var_weight) == T){
    ponder_calc <- rep(1,nrow(data))
  }else if (weight_norm == FALSE){
    ponder_calc <- with(data,get(var_weight))
  }else {
    ponder_calc <- (with(data,get(var_weight))*nrow(data))/sum(with(data,get(var_weight)))
  }
  
  
  # Données et labels:     ----
  if (use_labels == "no") { 
    dt <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars,var_col)),ponderation) %>% 
      mutate(across(!matches("ponderation"), as.factor))
    dt_num <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars_num,var_col)),ponderation) %>% 
      mutate(across(!matches(c("ponderation", vars_num)), as.factor))
  } else if (use_labels == "yes"){
    dt <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars,var_col)),ponderation) %>% 
      mutate(across(where(~ !is.null(attr(.x, "labels"))), haven::as_factor)) %>% 
      mutate(across(where(~ is.null(attr(.x, "labels"))) & !matches("ponderation"), as.factor))
    dt_num <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars_num,var_col)),ponderation) %>% 
      mutate(across(where(~ !is.null(attr(.x, "labels"))), haven::as_factor)) %>% 
      mutate(across(where(~ is.null(attr(.x, "labels"))) & !matches(c("ponderation", vars_num)), as.factor))
  } else if (use_labels == "both"){
    dt <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars,var_col)),ponderation) %>% 
      mutate(across(!matches("ponderation"), ~case_when(
        is.na(.) ~ NA,
        is.null(attributes(.)$labels) ~ as.factor(.),
        TRUE ~ paste0(as.factor(.)," : ",as_factor(.)))
      ))
    dt_num <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars_num,var_col)),ponderation) %>% 
      mutate(across(!matches(c("ponderation", vars_num)), ~case_when(
        is.na(.) ~ NA,
        is.null(attributes(.)$labels) ~ as.factor(.),
        TRUE ~ paste0(as.factor(.)," : ",as_factor(.)))
      ))
  }
  #############################
  
  
  
  #############################
  ### VARIABLES NUMERIQUES ----
  
  # Si pas de variables num
  if(is.null(vars_num) == F){
    
    # Renomme la variable de colonne
    dt_num <- dt_num                        |> 
      select(all_of(var_col), everything()) |> 
      rename(Groupe = 1) 
    
    
    
    ###########
    ## UNIVAR               ----
    desc_uni_num = dt_num |>
      pivot_longer(all_of(vars_num),
                   names_to = "Var",
                   values_to = "values") |> 
      as_survey(ids = 1, weights = ponderation) |> 
      group_by(Var) |> 
      summarise(Minimum   = round(min(values, na.rm = T), arrondi),
                Moyenne   = round(survey_mean(values, na.rm = T, vartype = NULL), arrondi),
                Quartile1 = round(survey_quantile(values,0.25, na.rm = T, vartype = NULL), arrondi) ,
                Mediane   = round(survey_quantile(values,0.5, na.rm = T, vartype = NULL), arrondi)  ,
                Quartile3 = round(survey_quantile(values,0.75, na.rm = T, vartype = NULL), arrondi) ,
                Maximum   = round(max(values, na.rm = T), arrondi),
                NbrNA     = round(sum(ponderation[is.na(values)]), arrondi)
      )|> 
      pivot_longer(2:8,
                   names_to = "Levels",
                   values_to = "ENSEMBLE") 
    
    # Ligne blanche
    if(add_blank_rows == TRUE){
      # Ajouter une ligne vide entre les groupes
      desc_uni_num_split <- desc_uni_num %>%
        group_split(Var)  # séparer les groupes
      # Créer une ligne vide
      empty_row <- desc_uni_num[1, ]
      empty_row[] <- NA
      # Ajouter une ligne vide entre chaque groupe
      desc_uni_num <- do.call(rbind, lapply(desc_uni_num_split, function(group) {
        rbind(group, empty_row)
      }))
    } # Fin Ligne blanche
    
    
    
    ###########
    ## BIVAR                 ----
    
    if(is.null(var_col) == FALSE){
      
      # Gestion des NA
      if(useNA == FALSE){
        dt_num <- dt_num |> 
          filter(Groupe != "Val.Manq." | is.na(Groupe) == TRUE )
      }
      # Exclure des modalités des calculs
      if (is.null(exclude) == FALSE) {
        dt_num <- dt_num |> 
          filter(!Groupe %in% exclude)
      }
      
      desc_bi_num = dt_num |>
        pivot_longer(all_of(vars_num),
                     names_to = "Var",
                     values_to = "values") |> 
        as_survey(ids = 1, weights = ponderation) |> 
        group_by(Groupe,Var) |> 
        summarise(Minimum   = round(min(values, na.rm = T), arrondi),
                  Moyenne   = round(survey_mean(values, na.rm = T, vartype = NULL), arrondi),
                  Quartile1 = round(survey_quantile(values,0.25, na.rm = T, vartype = NULL), arrondi) ,
                  Mediane   = round(survey_quantile(values,0.5, na.rm = T, vartype = NULL), arrondi)  ,
                  Quartile3 = round(survey_quantile(values,0.75, na.rm = T, vartype = NULL), arrondi) ,
                  Maximum   = round(max(values, na.rm = T), arrondi),
                  NbrNA     = round(sum(ponderation[is.na(values)]), arrondi)
        )                  |> 
        rename(Groupe = 1) |> 
        pivot_longer(3:9,
                     names_to = "Levels",
                     values_to = "ENSEMBLE") |> 
        pivot_wider(names_from = Groupe,
                    values_from = ENSEMBLE)  |> 
        left_join(filter(desc_uni_num, !is.na(Var)), by = c("Var","Levels"))
      
      # Ligne blanche
      if(add_blank_rows == TRUE){
        # Ajouter une ligne vide entre les groupes
        desc_bi_num_split <- desc_bi_num %>%
          group_split(Var)  # séparer les groupes
        
        # Créer une ligne vide
        empty_row <- desc_bi_num[1, ]
        empty_row[] <- NA
        
        # Ajouter une ligne vide entre chaque groupe
        desc_bi_num <- do.call(rbind, lapply(desc_bi_num_split, function(group) {
          rbind(group, empty_row)
        }))
      } # Fin Ligne blanche
      
    } # Fin Bivar
  } # Fin numérique
  
  #############################
  
  
  
  #############################
  ### BOUCLES                ----
  
  
  # Création table
  desc_uni <- data.frame()
  desc_bi_eff <- data.frame()
  desc_bi_row <- data.frame()
  desc_bi_col <- data.frame()
  
  
  if(is.null(vars) == FALSE){
    for (i in c(1:length(vars))) {
      ##################
      
      
      ##################
      ### Création du tri à plat  ----
      
      tabuni <- dt %>% 
        group_by(get(vars[i])) %>% 
        summarise(ENSEMBLE = round(sum(ponderation),arrondi),
                  ENSEMBLE_noPonder = n()) %>% 
        rename(Levels = 1) %>% 
        mutate(Levels = if_else(is.na(Levels), "Val.Manq.", Levels),
               Levels = factor(Levels, 
                               levels = c(with(dt,names(table(get(vars[i])))),"Val.Manq."))) %>% 
        mutate(Var = vars[i]) %>%  
        select(Var,Levels,ENSEMBLE,ENSEMBLE_noPonder)
      
      
      # Gestion des NA
      if(useNA == FALSE){
        tabuni <- tabuni %>% 
          filter(Levels != "Val.Manq." | is.na(Levels) == TRUE )
      } # Fin NA
      
      # Exclure des modalités des calculs
      if (is.null(exclude) == FALSE) {
        tabuni <- tabuni |> 
          filter(!Levels %in% exclude)
      } # Fin exclude
      
      
      tabuni = tabuni %>% 
        mutate(Freq = (ENSEMBLE*100)/sum(ENSEMBLE),
               FreqCum = cumsum(Freq)) |>  
        mutate(Freq = round(Freq,arrondi),
               FreqCum = round(FreqCum,arrondi)) 
      
      desc_uni <- rbind(desc_uni,tabuni)
      
      
      # Ajout de ligne blanche entre les variables
      if(add_blank_rows == TRUE){
        desc_uni <- rbind(desc_uni, rep(NA, ncol(desc_uni)))
      } # Fin ligne blanche
      ##################
      
      
      ##################
      ### Création tri croisé si var_col
      if(is.null(var_col) == FALSE){
        
        ##################
        # La table initale : ----
        
        tab_commune <- dt %>% 
          group_by(get(var_col),get(vars[i])) %>% 
          summarise(ENSEMBLE = sum(ponderation), .groups = "drop") %>% 
          rename(Groupe = 1,
                 Levels = 2) %>% 
          mutate(Levels = if_else(is.na(Levels), "Val.Manq.", Levels),
                 Groupe = if_else(is.na(Groupe), "Val.Manq.", Groupe)) %>% 
          complete(Groupe, Levels, fill = list(ENSEMBLE = 0)) %>% 
          mutate(Levels = factor(Levels, levels = c(with(dt,names(table(get(vars[i])))),"Val.Manq.")),
                 Groupe = factor(Groupe, levels = c(with(dt,names(table(get(var_col)))),"Val.Manq."))) %>% 
          arrange(Groupe,Levels)
        
        
        # Gestion des NA
        if(useNA == FALSE){
          tab_commune <- tab_commune %>% 
            filter(Levels != "Val.Manq."| is.na(Levels) == TRUE) %>% 
            filter(Groupe != "Val.Manq."| is.na(Groupe) == TRUE)
        }
        
        # Exclure des modalités des calculs
        if (is.null(exclude) == FALSE) {
          tab_commune <- tab_commune |> 
            filter(!Levels %in% exclude) |> 
            filter(!Groupe %in% exclude)
        }
        
        
        ##################
        # Test de significativité  -----
        
        if(use_test != "no") {
          
          tempo = dt |> 
            select(ponderation, vars[i], all_of(var_col)) |> 
            mutate(var1 = as.character(get(vars[i])),
                   var1 = if_else(is.na(var1), "Val.Manq.", var1),
                   var2 = as.character(get(var_col)),
                   var2 = if_else(is.na(var2), "Val.Manq.", var2)) 
          # Gestion des NA
          if(useNA == FALSE){
            tempo <- tempo %>% 
              filter(var1 != "Val.Manq."| is.na(var1) == TRUE) %>% 
              filter(var2 != "Val.Manq."| is.na(var2) == TRUE)
          }
          # Exclure des modalités des calculs
          if (is.null(exclude) == FALSE) {
            tempo <- tempo |> 
              filter(!var1 %in% exclude) |> 
              filter(!var2 %in% exclude)
          }
          
          # Chi2 
          if(use_test == "chi2") {
            
            # Chi2 non pondéré
            if(is.null(var_weight) == T){
              
              tab_test = xtabs(~var1+var2,data=tempo)
              test = chisq.test(tab_test)
              ddl = test$parameter
              pvalue = round(test$p.value,3)
              
              # Warning si Pct > 20
              warning = as.data.frame(test$expected) |> 
                pivot_longer(1:ncol(test$expected))  |> 
                mutate(n5 = ifelse(value <= 5, 1,0),
                       n5 = ifelse(value < 1, 1000,n5))|> 
                summarise(
                  Eff = sum(n5),
                  Pct = sum(n5)*100/n())
              
              message_test = "Test du Chi²"
              
              # Chi2 pondéré
            } else {
              
              tab_test  = survey::svydesign(id = ~ 1, weights = tempo$ponderation, data = tempo )
              
              #test = survey::svychisq(~ var1 + var2, tab_test, statistic="F")
              
              test = tryCatch({
                svychisq(~ var1 + var2, tab_test, statistic="F")
              },error = function(e) {
                message("ATTENTION : les chi2 ne sont pas calculés : des cases doivent avoir trop peu d'effectifs.
Peut-être qu'un test de Fisher serait plus adapté")
                NA  # Returning NULL in case of an error in outlier detection
              })
              
              if(length(test) < 2){
                warning = data.frame(
                  Eff = 9999,
                  Pct = 100
                )
                ddl = NA
                pvalue = NA
                
              } else{
                warning = as.data.frame(test$expected) |> 
                  pivot_longer(1:ncol(test$expected))  |> 
                  mutate(n5 = ifelse(value <= 5, 1,0),
                         n5 = ifelse(value < 1, 1000,n5)) |> 
                  summarise(Eff = sum(n5),
                            Pct = sum(n5)*100/n())
                
                ddl = round(test$parameter[1],1)
                pvalue = round(test$p.value,3)
              }
              
              # Save message 
              message_test = "Chi2 pondéré avec correction de Rao-Scott"
              
            }
            
            # Fisher 
          } else if (use_test == "fisher"){
            
            tab_test = xtabs(ponderation~var1+var2,data=tempo)
            #tab_test = xtabs(~var1+var2,data=tempo)
            
            test = fisher.test(tab_test, simulate.p.value=TRUE)
            
            if(is.null(var_weight) == T){
              message_test = "Le test de fisher est calculé sur des données non pondérées"
            } else {
              message_test = "Le test de fisher est calculé sur des données pondérées"
            }
            
            # Chi2 non pondéré sur n'importe quelles données  
          } else if (use_test == "chi2_noponder"){
            
            tab_test = xtabs(~var1+var2,data=tempo)
            test = chisq.test(tab_test)
            ddl = test$parameter
            pvalue = round(test$p.value,3)
            
            warning = as.data.frame(test$expected) |> 
              pivot_longer(1:ncol(test$expected))  |> 
              mutate(n5 = ifelse(value <= 5, 1,0),
                     n5 = ifelse(value < 1, 1000,n5))|> 
              summarise(
                Eff = sum(n5),
                Pct = sum(n5)*100/n())
            
            message_test = "Le Chi2 a été calculé sur des données non pondérées"
          }
        } else {
          
          message_test = "Pas de test"
          
        } # Fin Test
        ##################
        
        
        ##################
        ### EFFECTIF
        if(table_type %in% c("eff","all")){
          tab_eff <- tab_commune %>% 
            mutate(ENSEMBLE = round(ENSEMBLE,arrondi)) |> 
            pivot_wider(names_from = Groupe, values_from = ENSEMBLE)  %>% 
            mutate(Var = vars[i]) %>%  
            select(Var,Levels,everything()) %>% 
            left_join(select(desc_uni, Var,Levels, ENSEMBLE), by = c("Var","Levels"))
          
          # Variables du Chi²
          if(use_test %in% c("chi2", "chi2_noponder")){
            tab_eff <- tab_eff %>% mutate(
              chi2_pvalue = pvalue,
              chi2_ddl = ddl,
              chi2_warn = "OK")
            # S'il y a un warning dans chisq.test
            if (warning$Pct > 20) { tab_eff <- tab_eff %>% mutate(chi2_warn = "Pas_OK")}
            
          } else if(use_test == "fisher"){
            tab_eff <- tab_eff %>% mutate(fisher_pvalue = round(test$p.value,3))
          }
          
          # On ajoute la boucle à la table générale
          desc_bi_eff <- rbind(desc_bi_eff,tab_eff)
          
          # Ajout de ligne blanche entre les variables
          if(add_blank_rows == TRUE){
            desc_bi_eff <- rbind(desc_bi_eff, rep(NA, ncol(desc_bi_eff)))
          } # Fin Ligne blanche
          
        } # FIN eff
        ##################
        
        
        ##################
        ### LIGNES
        if(table_type %in% c("row","all")){
          
          tab_row <- tab_commune |> 
            # Pourcentage Row
            group_by(Levels) %>% 
            mutate(Value = round((ENSEMBLE*100)/sum(ENSEMBLE), arrondi)) %>% 
            mutate(ENSEMBLE = sum(Value)) |> 
            pivot_wider(names_from = Groupe, values_from = Value)  %>% 
            mutate(Var = vars[i]) %>%  
            select(Var,Levels,everything()) %>%  
            relocate(ENSEMBLE, .after = last_col()) %>% 
            ungroup()
          
          # Variables du Chi²
          if(use_test %in% c("chi2", "chi2_noponder")){
            tab_row <- tab_row %>% mutate(
              chi2_pvalue = pvalue,
              chi2_ddl = ddl,
              chi2_warn = "OK")
            # S'il y a un warning dans chisq.test
            if (warning$Pct > 20) { tab_row <- tab_row %>% mutate(chi2_warn = "Pas_OK")}
            
          } else if(use_test == "fisher"){
            tab_row <- tab_row %>% mutate(fisher_pvalue = round(test$p.value,3))
            
          }
          
          # On ajoute la boucle à la table générale
          desc_bi_row <- rbind(desc_bi_row,tab_row)
          
          # Ajout de ligne blanche entre les variables
          if(add_blank_rows == TRUE){
            desc_bi_row <- rbind(desc_bi_row, NA) # Test sans rep()
          } # Fin Ligne blanche
          
        } # FIN row
        ##################
        
        
        ##################
        if(table_type %in% c("col","all")){
          
          tab_col <- tab_commune %>%
            # Pourcentage Col
            group_by(Groupe) %>% 
            mutate(ENSEMBLE = round((ENSEMBLE*100)/sum(ENSEMBLE), arrondi)) %>% 
            pivot_wider(names_from = Groupe, values_from = ENSEMBLE)  %>% 
            mutate(Var = vars[i]) %>%  
            select(Var,Levels,everything()) %>% 
            left_join(select(desc_uni, Var,Levels, Freq), by = c("Var","Levels")) %>% 
            rename(ENSEMBLE=Freq) %>% 
            ungroup()
          
          # Variables du Chi²
          if(use_test %in% c("chi2", "chi2_noponder")){
            tab_col <- tab_col %>% mutate(
              chi2_pvalue = pvalue,
              chi2_ddl = ddl,
              chi2_warn = "OK")
            # S'il y a un warning dans chisq.test
            if (warning$Pct > 20) { tab_col <- tab_col %>% mutate(chi2_warn = "Pas_OK")}
            
          } else if(use_test == "fisher"){
            tab_col <- tab_col %>% mutate(fisher_pvalue = round(test$p.value,3))
          }
          
          # On ajoute la boucle à la table générale
          desc_bi_col <- rbind(desc_bi_col,tab_col)
          
          # Ajout de ligne blanche entre les variables
          if(add_blank_rows == TRUE){
            desc_bi_col <- rbind(desc_bi_col, rep(NA, ncol(desc_bi_col)))
          } # Fin Ligne blanche
        } # FIN col
      } # Fin Var Croisé
    } # Fin Boucle
  } # Fin if vars != NULL
  #############################
  
  
  
  #############################
  ### COMPILATIONS ----
  
  ##############
  # UNIVAR         ----
  
  # Première ligne
  first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
    summarise(ENSEMBLE = round(sum(ponderation),arrondi),
              ENSEMBLE_noPonder = n()) %>% 
    mutate(Freq = 100,
           FreqCum = 100) %>% 
    mutate(Var =" ",
           Levels = "ENSEMBLE") %>%  
    select(Var, Levels,everything())
  
  
  
  
  
  ############## ENSEMBLE ou TOTAL ?
  
  if(add_blank_rows == FALSE){
    desc_uni <- first_row %>% bind_rows(desc_uni) # %>%  rename(Total = ENSEMBLE) 
  } else {
    desc_uni <- first_row %>% rbind(NA) %>% bind_rows(desc_uni) # %>% rename(Total = ENSEMBLE)
  }
  
  
  # Variables numériques
  if(is.null(vars_num) == T){
    desc_uni_vf = select(desc_uni,-ENSEMBLE_noPonder) 
  } else {
    desc_uni_vf = select(desc_uni,-ENSEMBLE_noPonder) |> 
      bind_rows(desc_uni_num)
  }
  
  assign("table_auto_univar", desc_uni_vf, envir = .GlobalEnv)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############
  # BIVAR EFF        ----
  
  
  # Si on a une variable en colonne
  if(is.null(var_col) == FALSE){
    
    ### EFF
    if(table_type %in% c("eff","all")){
      
      first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
        group_by(get(var_col)) %>% 
        summarise(Eff = round(sum(ponderation),arrondi)) %>% 
        rename(Groupe = 1) %>% 
        mutate(Groupe = if_else(is.na(Groupe), "Val.Manq.", as.character(Groupe))) 
      
      # Gestion des NA
      if(useNA == FALSE){
        first_row <- first_row %>% 
          filter(Groupe != "Val.Manq." | is.na(Groupe) == TRUE )
      }
      # Exclure des modalités des calculs
      if (is.null(exclude) == FALSE) {
        first_row <- first_row |> 
          filter(!Groupe %in% exclude)
      }
      
      first_row <- first_row %>% 
        mutate(ENSEMBLE = sum(Eff),
               Var =" ",
               Levels = "ENSEMBLE") %>%  
        pivot_wider(names_from = Groupe, values_from = Eff) %>% 
        select(-ENSEMBLE,Var, Levels,everything(),ENSEMBLE)
      
      
      if(add_blank_rows == FALSE){
        desc_bi_eff <- first_row %>% bind_rows(desc_bi_eff)  
      } else {
        desc_bi_eff <- first_row %>% rbind(NA) %>% bind_rows(desc_bi_eff)  
      }
      
      
      ### Effectif dans les noms
      if(eff_in_name == "yes"){
        
        desc_bi_eff <- desc_bi_eff %>% 
          bind_cols(select(desc_uni,Total)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",Total,")"))) %>% 
          select(-Total)
        
      } else if(eff_in_name == "noponder") {
        
        desc_bi_eff <- desc_bi_eff %>% 
          bind_cols(select(desc_uni,ENSEMBLE_noPonder)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",ENSEMBLE_noPonder,")"))) %>% 
          select(-ENSEMBLE_noPonder)
        
      }
      
      ### VAR NUMERIQUES  #############
      
      # Si pas de variables num
      if(is.null(vars_num) == F){
        desc_bi_eff = desc_bi_eff |> 
          bind_rows(desc_bi_num)
      } # Fin numerique
      
      ##################
      
      # Enregistrer le data.frame dans l'environnement global
      assign("table_auto_eff", desc_bi_eff, envir = .GlobalEnv)
      
    }
    
    
    
    ### ROW
    if(table_type %in% c("row","all")){
      
      first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
        group_by(get(var_col)) %>% 
        summarise(Eff = sum(ponderation)) %>% 
        rename(Groupe = 1) %>% 
        mutate(Groupe = if_else(is.na(Groupe), "Val.Manq.", as.character(Groupe)))
      
      # Gestion des NA
      if(useNA == FALSE){
        first_row <- first_row %>% 
          filter(Groupe != "Val.Manq." | is.na(Groupe) == TRUE )
      }
      # Exclure des modalités des calculs
      if (is.null(exclude) == FALSE) {
        first_row <- first_row |> 
          filter(!Groupe %in% exclude)
      }
      
      first_row <- first_row |> 
        mutate(Pct = round((Eff*100)/sum(Eff),arrondi)) %>% 
        select(-Eff)  %>% 
        mutate(ENSEMBLE = sum(Pct),
               Var =" ",
               Levels = "ENSEMBLE") %>%  
        pivot_wider(names_from = Groupe, values_from = Pct) %>% 
        select(-ENSEMBLE,Var, Levels,everything(),ENSEMBLE)
      
      
      if(add_blank_rows == FALSE){
        desc_bi_row <- first_row %>% bind_rows(desc_bi_row)  
      } else {
        desc_bi_row <- first_row %>% rbind(NA) %>% bind_rows(desc_bi_row)  
      } 
      
      
      if(eff_in_name == "yes"){
        
        desc_bi_row <- desc_bi_row %>% 
          bind_cols(select(desc_uni,Total)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",Total,")"))) %>% 
          select(-Total)
        
      } else if(eff_in_name == "noponder") {
        
        desc_bi_row <- desc_bi_row %>% 
          bind_cols(select(desc_uni,ENSEMBLE_noPonder)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",ENSEMBLE_noPonder,")"))) %>% 
          select(-ENSEMBLE_noPonder)
        
      }
      
      ### VAR NUMERIQUES  #############
      
      # Si pas de variables num
      if(is.null(vars_num) == F){
        desc_bi_row = desc_bi_row |> 
          bind_rows(desc_bi_num)
      } # Fin numerique
      
      ##################
      
      
      # Enregistrer le data.frame dans l'environnement global
      assign("table_auto_row", desc_bi_row, envir = .GlobalEnv)
    }
    
    
    
    
    ### COL
    if(table_type %in% c("col","all")){
      
      first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
        group_by(get(var_col)) %>% 
        summarise(Eff = 100) %>% 
        rename(Groupe = 1) %>% 
        mutate(Groupe = if_else(is.na(Groupe), "Val.Manq.", as.character(Groupe))) %>% 
        mutate(ENSEMBLE = 100,
               Var =" ",
               Levels = "ENSEMBLE")
      
      # Gestion des NA
      if(useNA == FALSE){
        first_row <- first_row %>% 
          filter(Groupe != "Val.Manq." | is.na(Groupe) == TRUE )
      }
      # Exclure des modalités des calculs
      if (is.null(exclude) == FALSE) {
        first_row <- first_row |> 
          filter(!Groupe %in% exclude)
      }
      
      first_row <- first_row %>%  
        pivot_wider(names_from = Groupe, values_from = Eff) %>% 
        select(-ENSEMBLE,Var, Levels,everything(),ENSEMBLE)
      
      
      
      if(add_blank_rows == FALSE){
        desc_bi_col <- first_row %>% bind_rows(desc_bi_col)  
      } else {
        desc_bi_col <- first_row %>% rbind(NA) %>% bind_rows(desc_bi_col)  
      }
      
      
      if(eff_in_name == "yes"){
        
        desc_bi_col <- desc_bi_col %>% 
          bind_cols(select(desc_uni,Total)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",Total,")"))) %>% 
          select(-Total)
        
      } else if(eff_in_name == "noponder") {
        
        desc_bi_col <- desc_bi_col %>% 
          bind_cols(select(desc_uni,ENSEMBLE_noPonder)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",ENSEMBLE_noPonder,")"))) %>% 
          select(-ENSEMBLE_noPonder)
        
      }
      
      ### VAR NUMERIQUES  #############
      
      # Si pas de variables num
      if(is.null(vars_num) == F){
        desc_bi_col = desc_bi_col |> 
          bind_rows(desc_bi_num)
      } # Fin numerique
      
      ##################
      
      
      
      # Enregistrer le data.frame dans l'environnement global
      assign("table_auto_col", desc_bi_col, envir = .GlobalEnv)
      
      
    } # Fin col
  } # Fin bivar
  
  
  
  
  ### TABLE_TYPE = mix ----
  
  
  if(table_type %in% c("mix","all")){
    
    # Gestion des NA
    if(useNA == FALSE){
      dt_mix <- dt %>% 
        drop_na()
    } else {
      dt_mix <- dt
    }
    
    # Exclure des modalités des calculs
    if (is.null(exclude) == FALSE) {
      dt_mix <- dt_mix |> 
        filter_at(vars(-ponderation), ~!. %in% exclude)
    }
    
    dt_mix <- dt_mix            |> 
      mutate(across(-ponderation, ~ ifelse(is.na(.), "Val.Manq.",as.character(.)))) |> 
      group_by_at(vars(-ponderation))                      |> 
      summarise(Eff = round(sum(ponderation),arrondi), .groups = "keep")     |>
      ungroup()                                            |> 
      arrange(desc(Eff))                                   |>
      mutate(Pct = Eff*100 / sum(Eff),
             Pct = round(Pct, arrondi),
             PctCum = cumsum(Pct),
             PctCum = round(PctCum, arrondi),
             Eff = round(Eff, arrondi))      
    
    assign("table_auto_mix", dt_mix, envir = .GlobalEnv)
    
    
  }
  
  
  ### MESSAGE ----
  
  # C'est le message du Chi²
  
  if(is.null(var_col) == FALSE){
    print(message_test)
  }
  
  if(view_html == TRUE){
    print("view_html = TRUE est remplacé par view = 'table' ou view = 'graph', et ne fonctionnera bientôt plus")
  }
  
  
  
  
  ### EXPORT                 ----
  
  # Export
  if(excel_export == TRUE){
    
    if(is.null(var_col) == TRUE){
      
      if(!table_type %in% c("mix","all")){
        wb <- createWorkbook()
        addWorksheet(wb,"univar")
        writeData(wb, "univar", desc_uni_vf)
        saveWorkbook(wb, excel_filepath, overwrite = TRUE)
        
      } else {
        
        wb <- createWorkbook()
        addWorksheet(wb,"univar")
        writeData(wb, "univar", desc_uni_vf)
        addWorksheet(wb,"mix")
        writeData(wb, "mix", dt_mix)
        saveWorkbook(wb, excel_filepath, overwrite = TRUE)
        
        
      }
      
      
      
    } else {
      
      # Créez un objet workbook
      wb <- createWorkbook()
      
      if(table_type == "eff"){
        addWorksheet(wb,"eff")
        writeData(wb, "eff", desc_bi_eff)
      }else if(table_type == "row"){
        addWorksheet(wb,"row")
        writeData(wb, "row", desc_bi_row)
      }else if(table_type == "col"){
        addWorksheet(wb,"col")
        writeData(wb, "col", desc_bi_col)
      }else if(table_type == "mix"){
        addWorksheet(wb,"mix")
        writeData(wb, "mix", dt_mix)
      }else{
        addWorksheet(wb,"univar")
        writeData(wb, "univar", desc_uni_vf)
        
        obj_names <- c("desc_bi_eff", "desc_bi_row", "desc_bi_col", "dt_mix")
        nom_onglet <- substr(obj_names,nchar(obj_names)-2,nchar(obj_names))
        
        for (i in c(1:length(obj_names))){
          addWorksheet(wb,nom_onglet[i])
          writeData(wb, nom_onglet[i], get(obj_names[i]))
        }
        
      }
      saveWorkbook(wb, excel_filepath, overwrite = TRUE)  
      
    }
  }
  
  
  ### Graphique              ----
  
  if(view == "graph" & table_type != "mix"){
    
    ### DONNEES 
    if(!is.null(var_col)){ # Si bivar
      
      if(table_type == "row"){
        dt_plot = table_auto_row
      } else if(table_type == "col"){
        dt_plot = table_auto_col
      } else { 
        dt_plot = table_auto_eff
      }
      
      dt_plot = dt_plot                    |> 
        filter(!Var %in% c(vars_num, " ")) |> 
        filter(!is.na(Var))                |> 
        pivot_longer(unique(tab_commune$Groupe),
                     names_to = "Groupe",
                     values_to = "value")  |> 
        mutate(Levels2 = paste(Var,Levels,sep = "__"))                       |> 
        mutate(Var = factor(Var, levels = unique(Var)))                      |> 
        mutate(Levels2 = factor(Levels2, 
                                levels = rev(unique(Levels2)),
                                labels = sub(".*?__", "", rev(unique(Levels2))))) |> 
        mutate(Groupe = factor(Groupe, 
                               levels = rev(unique(Groupe))))                     |> 
        arrange(Var,Levels2,Groupe)  
      
    } else { # Si univar
      dt_plot = table_auto_univar
      dt_plot = dt_plot |> 
        filter(!Var %in% c(vars_num, " ")) |> 
        filter(!is.na(Var))    |> 
        mutate(Levels2 = paste(Var,Levels,sep = "__")) |> 
        mutate(Var = factor(Var, levels = unique(Var))) |> 
        mutate(Levels2 = factor(Levels2, 
                                levels = rev(unique(Levels2)),
                                labels = sub(".*?__", "", rev(unique(Levels2))))) |> 
        arrange(Var,Levels2)    
    }
    
    
    ### PARAMETRES GRAPHIQUES
    
    # Paramètres 
    
    fill_color = c("#8DD3C7","#FDB462","#BEBADA","#FB8072","#80B1D3","#B3DE69","#FCCDE5","#FFFFB3","#D9D9D9","#8DD3C7","#FDB462","#BEBADA","#FB8072","#80B1D3","#B3DE69","#FCCDE5","#FFFFB3","#D9D9D9")
    
    montheme = theme(
      strip.text.y.left = element_text(size = 12, colour = "white",face = 2, angle = 0),
      strip.background.y = element_rect(fill = "#2F4359",colour = NA),
      strip.text.x = element_text(size = 12, colour = "#2F4359",face = 2),
      strip.background.x = element_rect(fill = "#e4e9f2",colour = NA),
      strip.placement = "outside",
      
      
      legend.position="bottom",
      legend.title=element_blank(),
      legend.text = element_text(size = 12,colour = "#2F4359"),
      legend.justification='center',
      legend.margin=margin(t=-8),
      
      panel.grid.minor.y=element_blank(),
      panel.grid.major.y=element_line(color = "lightgrey",linewidth = 0.25,linetype = "dotted"),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_line(color = "lightgrey",linewidth = 0.25,linetype = "dotted"),
      
      axis.text = element_text(size = 12, colour = "#2F4359"),
      axis.title = element_blank()
    ) 
    
    # Legendes
    rowlegend = case_when(
      length(unique(dt_plot$Groupe)) <= 3 ~ 1,
      length(unique(dt_plot$Groupe)) %in% c(4:6)  ~ 2,
      TRUE ~ 3
    )
    monguide = guides(
      fill  = guide_legend(
        title.position = "top",
        reverse = T,
        nrow = rowlegend,
        byrow = T)
    ) 
    
    
    ### GRAPHIQUES
    
    if(!is.null(var_col)){ # Si bivar
      if(table_type == "row"){
        # Pourcentage lignes
        p = dt_plot |> 
          ggplot(aes(x = Levels2, y = value, fill = Groupe)) +
          facet_grid(Var ~ ., scales = "free_y", space = "free_y", switch = "y") +
          geom_col()      +
          scale_fill_manual(values = fill_color) +
          scale_y_continuous(breaks = seq(0,100,20), labels = paste0(seq(0,100,20), "%")) +
          coord_flip()    +
          theme_minimal() +
          montheme        +
          monguide
      } else if(table_type == "col"){
        # Pourcentage colonnes
        p = dt_plot |> 
          ggplot(aes(x = Levels2, y = value, fill = Groupe)) +
          facet_grid(Var ~ Groupe, scales = "free_y", space = "free_y", switch = "y") +
          geom_col(width = 1, colour = "#404040")      +
          scale_fill_manual(values = fill_color) +
          scale_y_continuous(breaks = seq(0,100,20)) +
          ylab("En %")    +
          coord_flip()    +
          theme_minimal() +
          montheme        +
          theme(axis.title.x = element_text(size = 12,colour = "#2F4359", hjust = 1)) +
          monguide
      } else { 
        # Effectif bivar
        p = dt_plot |> 
          ggplot(aes(x = Levels2, y = value, fill = Groupe)) +
          facet_grid(Var ~ ., scales = "free_y", space = "free_y", switch = "y") +
          geom_col()      +
          scale_fill_manual(values = fill_color) +
          coord_flip()    +
          theme_minimal() +
          montheme        +
          monguide
      }
    } else { # Si univar
      # Univarié
      p = dt_plot |> 
        ggplot(aes(x = Levels2, y = Freq, fill = Var, color = Var)) +
        facet_grid(Var ~ ., scales = "free_y", space = "free_y", switch = "y") +
        geom_segment(aes(yend = 0 , xend = Levels2), 
                     linewidth = 1.25, alpha = 0.33) +
        geom_point(position = "identity", 
                   shape = 20, 
                   size = 4) +
        scale_fill_manual(values = fill_color) +
        scale_color_manual(values = fill_color) +
        scale_y_continuous(breaks = seq(0,100,20), labels = paste(seq(0,100,20), "%")) +
        coord_flip() +
        theme_minimal() +
        montheme        +
        guides(
          color = "none",
          fill  = "none"
        ) 
    } # Fin Choix graphiques
    
    return(p)
    
  } # Fin graph
  
  
  ### HTML                   ----
  
  if(view_html == TRUE | view == "table"| table_type == "mix"){
    
    if(!is.null(var_col)){
      
      if(table_type == "row"){
        html_dt = table_auto_row
      } else if(table_type == "col"){
        html_dt = table_auto_col
      } else { 
        html_dt = table_auto_eff
      }
      
      
      if(table_type != "mix"){
        
        if(use_test %in% c("chi2", "chi2_noponder")){
          nb_col = length(select(html_dt, -c(Var, Levels, chi2_pvalue, chi2_ddl, chi2_warn)))
        } else if (use_test == "fisher"){
          nb_col = length(select(html_dt, -c(Var, Levels, fisher_pvalue)))
        } else {
          nb_col = length(select(html_dt, -c(Var, Levels)))
        }
        
        titre = var_col
        
        show_tab = html_dt                       |> 
          filter(is.na(Var) == FALSE) |> 
          group_by(Var)               |> 
          gt()                        |>
          # Titre ligne du haut
          tab_spanner(
            label = html(titre),
            columns = c(3: (3+nb_col-1))) |>
          # Alignement du texte
          cols_align(
            align = "center",
            columns = c(3:(length(html_dt)))
          )                           |>
          cols_align(
            align = "right",
            columns = 2)              |>
          # Centrer les titres des colonnes en hauteur
          tab_style(
            style = list(
              cell_text(align = "center", v_align = "middle") # v_align pour la hauteur
            ),
            locations = cells_column_labels()
          )                           |> 
          tab_style(
            style = cell_fill(color = "#f5ffed"),
            locations = cells_row_groups()
          )                           |>
          # Supprimer les bordures 
          tab_style(
            style = list(
              cell_borders(
                sides = "top",          # Supprimer la bordure en bas
                color = "black",        # Couleur invisible
                weight = px(2)          # Aucune épaisseur
              )
            ),
            locations = cells_row_groups()
          )                            |> 
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_column_spanners()
          )
        
      } else {
        
        nb_col = ncol(dt_mix)
        show_tab = dt_mix |> 
          rename(Effectifs = Eff,
                 `Fréquences` = Pct,
                 `Fréquences cumulées` = PctCum) |> 
          gt() |>
          # Titre ligne du haut
          tab_spanner(
            label = html("Combinaison des variables"),
            columns = c(1: (nb_col-3))) |>
          # Alignement du texte
          cols_align(
            align = "center",
            columns = c(1:nb_col))      |>
          # Centrer les titres des colonnes en hauteur
          tab_style(
            style = list(
              cell_text(align = "center", v_align = "bottom") # v_align pour la hauteur
            ),
            locations = cells_column_labels()
          )                           |> 
          tab_style(
            style = cell_fill(color = "#f5ffed"),
            locations = cells_column_labels()
          )                           |>
          tab_style(
            style = cell_fill(color = "#d3e8c3"),
            locations = cells_column_spanners()
          )                           |>
          tab_style(
            style = cell_fill(color = "#d3e8c3"),
            locations = cells_column_labels(
              columns = c((nb_col-2) : nb_col)
            ))                           |>
          tab_style(
            style = cell_fill(color = "#f5ffed"),
            locations = cells_body(
              columns = c((nb_col-2) : nb_col)
            ))                           |>
          
          # Supprimer les bordures 
          tab_style(
            style = list(
              cell_borders(
                sides = "bottom",          # Supprimer la bordure en bas
                color = "black",           # Couleur invisible
                weight = px(2)             # épaisseur
              )
            ),
            locations = cells_column_labels()
          )                            |> 
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_column_labels()
          )|> 
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_column_spanners()
          )
        
      } 
      
    } else { # Si pas de variable en colonnes
      
      if(table_type == "mix"){
        
        nb_col = ncol(dt_mix)
        show_tab = dt_mix |> 
          rename(Effectifs = Eff,
                 `Fréquences` = Pct,
                 `Fréquences cumulées` = PctCum) |> 
          gt() |>
          # Titre ligne du haut
          tab_spanner(
            label = html("Combinaison des variables"),
            columns = c(1: (nb_col-3))) |>
          # Alignement du texte
          cols_align(
            align = "center",
            columns = c(1:nb_col))      |>
          # Centrer les titres des colonnes en hauteur
          tab_style(
            style = list(
              cell_text(align = "center", v_align = "bottom") # v_align pour la hauteur
            ),
            locations = cells_column_labels()
          )                           |> 
          tab_style(
            style = cell_fill(color = "#f5ffed"),
            locations = cells_column_labels()
          )                           |>
          tab_style(
            style = cell_fill(color = "#d3e8c3"),
            locations = cells_column_spanners()
          )                           |>
          tab_style(
            style = cell_fill(color = "#d3e8c3"),
            locations = cells_column_labels(
              columns = c((nb_col-2) : nb_col)
            ))                           |>
          tab_style(
            style = cell_fill(color = "#f5ffed"),
            locations = cells_body(
              columns = c((nb_col-2) : nb_col)
            ))                           |>
          
          # Supprimer les bordures 
          tab_style(
            style = list(
              cell_borders(
                sides = "bottom",          # Supprimer la bordure en bas
                color = "black",           # Couleur invisible
                weight = px(2)             # épaisseur
              )
            ),
            locations = cells_column_labels()
          )                            |> 
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_column_labels()
          )|> 
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_column_spanners()
          )          
        
        
      } else {
        
        show_tab = table_auto_univar             |> 
          rename(`Fréquences` = Freq,
                 `Fréquences cumulées` = FreqCum) |> 
          filter(is.na(Var) == FALSE) |> 
          group_by(Var)               |> 
          gt()                        |>
          
          # Alignement du texte
          cols_align(
            align = "center",
            columns = c(3:(length(table_auto_univar)))
          )                           |>
          cols_align(
            align = "right",
            columns = 2)              |>
          
          # Centrer les titres des colonnes en hauteur
          tab_style(
            style = list(
              cell_text(align = "center", v_align = "middle") # v_align pour la hauteur
            ),
            locations = cells_column_labels()
          )                           |> 
          tab_style(
            style = cell_fill(color = "#f5ffed"),
            locations = cells_row_groups()
          )                           |>
          # Supprimer les bordures 
          tab_style(
            style = list(
              cell_borders(
                sides = "top",          # Supprimer la bordure en bas
                color = "black",  # Couleur invisible
                weight = px(2)          # Aucune épaisseur
              )
            ),
            locations = cells_row_groups()
          )  
        
        
      } # fin table univar
    } # fin sans colonne
    
    return(show_tab)
    
  } # fin html
  
  
} # Fin Fonction

##################

