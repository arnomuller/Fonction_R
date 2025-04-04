####################################
####   FONCTION table_auto()    ####
####################################


## PACKAGES ----

# library(tidyverse)
# library(openxlsx)
# library(haven)
# library(gt)
# library(survey)


## MESSAGES ----

print("MAJ : 04/04/2025")
print("")
print("Remplace l'option chi2_test, par use_test : pour choisir le test du chi2 ou le test de fisher")
print("Correction Chi² pondérés selon Rao-Scott")
print("Ajout de exclude : pour ne pas tenir compte de certaines modalités, par ex : 99, 88")
print("Ajout de la table 'mix' : équivalent du proc format list de SAS : pour les combinaisons de modalités issues de différentes variables")
print("Correction des pourcentages pour la ligne Ensemble des pourcentages lignes")
print("")
print("Pour plus d'info : https://github.com/arnomuller/Fonction_R/tree/main/table_auto")
print("En cas de soucis, vous pouvez me contacter : arno.muller@ined.fr")


## FONCTION ----

# Lancer la fonction suivante pour pourvoir l'appeler dans vos prochains scripts :

table_auto <- function(data,                    # Un data.frame
                       vars,                    # Un vecteur avec les noms des variables d'intérêts
                       var_col        = NULL,   # Variable à croiser avec celles du vecteur
                       var_weight     = NULL,   # Variable de pondération, sinon = NULL
                       weight_norm    = FALSE,  # Normaliser la pondération
                       table_type     = "all",  # Type de table : "all", "eff", "row", "col", "mix"
                       useNA          = TRUE,   # TRUE/FALSE : Ajout des valeurs manquantes
                       exclude        = NULL,   # Un vecteur avec les noms des modalités à exclure des tables
                       use_test       = "chi2", # Type de test : "chi2", "fisher", "chi2_noponder", "no"
                       arrondi        = 2,      # Nombre de chiffres après la virgule
                       use_labels     = "no",   # Utiliser les labels : "no", "yes", "both"
                       add_blank_rows = TRUE,   # TRUE/FALSE : Ajout d'une ligne vide entre les variables
                       eff_in_name    = TRUE,   # TRUE/FALSE : Ajout des effectifs dans les noms des modalités
                       excel_export   = FALSE,  # TRUE/FALSE : Création d'un fichier excel et son chemin
                       excel_filepath = "./table_auto.xlsx", # Chemin vers le fichier excel
                       view_html      = TRUE){
  
  ### OPTIONS                ----
  # Ecriture scientifique
  options(scipen=9999)
  
  ### GESTION LIBRARY        ----
  # Liste des packages à charger
  packages <- c("tidyverse", "openxlsx", "haven", "gt", "survey")
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  
  
  ### GESTION DES ERREURS    ----
  
  # Vérification du parametre table_type
  if (table_type != "eff" && table_type != "row" && table_type != "col" && table_type != "all" && table_type != "mix") {
    stop("Erreur : table_type doit être 'eff', 'row', 'col', 'all' ou 'mix' ")
  }
  
  # Vérification du parametre use_labels
  if (use_labels != "no" && use_labels != "yes" && use_labels != "both") {
    stop("Erreur : use_labels doit être 'no', 'yes', ou 'both' ")
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
  
  # Vérification de parametre eff_in_name
  if (eff_in_name != TRUE && eff_in_name != FALSE) {
    stop("Erreur : eff_in_name doit être TRUE ou FALSE")
  }
  
  # Vérification de parametre excel_export
  if (excel_export != TRUE && excel_export != FALSE) {
    stop("Erreur : excel_export doit être TRUE ou FALSE")
  }
  
  # Vérification de parametre weight_norm
  if (weight_norm != TRUE && weight_norm != FALSE) {
    stop("Erreur : weight_norm doit être TRUE ou FALSE")
  }
  
  
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
  } else if (use_labels == "yes"){
    dt <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars,var_col)),ponderation) %>% 
      mutate(across(where(~ !is.null(attr(.x, "labels"))), haven::as_factor)) %>% 
      mutate(across(where(~ is.null(attr(.x, "labels"))) & !matches("ponderation"), as.factor))
    
    
  } else if (use_labels == "both"){
    dt <- data %>% 
      mutate(ponderation = ponder_calc) %>% 
      select(any_of(c(vars,var_col)),ponderation) %>% 
      mutate(across(!matches("ponderation"), ~case_when(
        is.na(.) ~ NA,
        is.null(attributes(.)$labels) ~ as.factor(.),
        TRUE ~ paste0(as.factor(.)," : ",as_factor(.)))
      ))
  }
  
  
  
  
  
  ### BOUCLES                ----
  
  
  # Création table
  desc_uni <- data.frame()
  desc_bi_eff <- data.frame()
  desc_bi_row <- data.frame()
  desc_bi_col <- data.frame()
  
  for (i in c(1:length(vars))) {
    
    # print(i)
    
    ### Création du tri à plat  ----
    
    tabuni <- dt %>% 
      group_by(get(vars[i])) %>% 
      summarise(ENSEMBLE = round(sum(ponderation),arrondi)) %>% 
      rename(Levels = 1) %>% 
      mutate(Levels = if_else(is.na(Levels), "Val.Manq.", Levels),
             Levels = factor(Levels, 
                             levels = c(with(dt,names(table(get(vars[i])))),"Val.Manq."))) %>% 
      mutate(Var = vars[i]) %>%  
      select(Var,Levels,ENSEMBLE)
    
    
    # Gestion des NA
    if(useNA == FALSE){
      tabuni <- tabuni %>% 
        filter(Levels != "Val.Manq." | is.na(Levels) == TRUE )
    }
    
    # Exclure des modalités des calculs
    if (is.null(exclude) == FALSE) {
      tabuni <- tabuni |> 
        filter(!Levels %in% exclude)
    }
    
    
    tabuni = tabuni %>% 
      mutate(Freq = (ENSEMBLE*100)/sum(ENSEMBLE),
             FreqCum = cumsum(Freq)) |>  
      mutate(Freq = round(Freq,arrondi),
             FreqCum = round(FreqCum,arrondi)) 
    
    desc_uni <- rbind(desc_uni,tabuni)
    
    
    # Ajout de ligne blanche entre les variables
    if(add_blank_rows == TRUE){
      desc_uni <- rbind(desc_uni, rep(NA, ncol(desc_uni)))
    }
    
    
    ### Création tri croisé si var_col
    if(is.null(var_col) == FALSE){
      
      
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
      
      
      
      
      
      ##### Test de significativité  ------------
      
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
            test = survey::svychisq(~ var1 + var2, tab_test, statistic="F")
            
            ddl = round(test$parameter[1],1)
            
            # Warning si Pct > 20
            warning = as.data.frame(test$expected) |> 
              pivot_longer(1:ncol(test$expected))  |> 
              mutate(n5 = ifelse(value <= 5, 1,0),
                     n5 = ifelse(value < 1, 1000,n5)) |> 
              summarise(Eff = sum(n5),
                        Pct = sum(n5)*100/n())
            
            
            # Save message 
            message_test = "Chi2 pondéré avec correction de Rao-Scott"
            
          }
          
          # Fisher 
        } else if (use_test == "fisher"){
          
          tab_test = xtabs(~var1+var2,data=tempo)
          test = fisher.test(tab_test, simulate.p.value=TRUE)
          message_test = "Le test de fisher est calculé sur des données non pondérées"
          
          # Chi2 non pondéré sur n'importe quelles données  
        } else if (use_test == "chi2_noponder"){
          
          tab_test = xtabs(~var1+var2,data=tempo)
          test = chisq.test(tab_test)
          ddl = test$parameter
          
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
            chi2_pvalue = round(test$p.value,3),
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
            chi2_pvalue = round(test$p.value,3),
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
            chi2_pvalue = round(test$p.value,3),
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
  
  
  
  ############################################
  # AJOUT PREMIERE LIGNE ----
  
  
  ###### UNIVAR 
  first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
    summarise(ENSEMBLE = round(sum(ponderation),arrondi)) %>% 
    mutate(Freq = 100,
           FreqCum = 100) %>% 
    mutate(Var =" ",
           Levels = "ENSEMBLE") %>%  
    select(Var, Levels,everything())
  
  
  
  if(add_blank_rows == FALSE){
    desc_uni <- first_row %>% bind_rows(desc_uni) %>% 
      rename(Total = ENSEMBLE) 
  } else {
    desc_uni <- first_row %>% rbind(NA) %>% bind_rows(desc_uni) %>% 
      rename(Total = ENSEMBLE)
  }
  
  assign("table_auto_univar", desc_uni, envir = .GlobalEnv)
  
  
  ###### BIVAR 
  
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
      
      if(eff_in_name == TRUE){
        
        desc_bi_eff <- desc_bi_eff %>% 
          bind_cols(select(desc_uni,Total)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",Total,")"))) %>% 
          select(-Total)
        
      }
      
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
      
      if(eff_in_name == TRUE){
        desc_bi_row <- desc_bi_row %>% 
          bind_cols(select(desc_uni,Total)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",Total,")"))) %>% 
          select(-Total)
      }
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
      
      
      
      if(eff_in_name == TRUE){
        
        desc_bi_col <- desc_bi_col %>% 
          bind_cols(select(desc_uni,Total)) %>% 
          mutate(Levels = if_else(is.na(Levels),
                                  NA,
                                  paste0(Levels," (n = ",Total,")"))) %>% 
          select(-Total)
        
      }
      
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
  

  
  
  ### EXPORT                 ----
  
  # Export
  if(excel_export == TRUE){
    
    if(is.null(var_col) == TRUE){
      
      if(!table_type %in% c("mix","all")){
        wb <- createWorkbook()
        addWorksheet(wb,"univar")
        writeData(wb, "univar", desc_uni)
        saveWorkbook(wb, excel_filepath, overwrite = TRUE)
        
      } else {
        
        wb <- createWorkbook()
        addWorksheet(wb,"univar")
        writeData(wb, "univar", desc_uni)
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
        writeData(wb, "univar", desc_uni)
        
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
  
  
  ### HTML                   ----
  
  if(view_html == TRUE){
    
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
        
        html_dt                       |> 
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
        dt_mix |> 
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
        dt_mix |> 
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
        
        table_auto_univar             |> 
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
  } # fin html
}

