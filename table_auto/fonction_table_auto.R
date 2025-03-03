####################################
####   FONCTION table_auto()    ####
####################################


## PACKAGES ----

# library(tidyverse)
# library(openxlsx)


## FONCTION ----

# Lancer la fonction suivante pour pourvoir l'appeler dans vos prochains scripts :

table_auto <- function(data,                    # Un data.frame
                       vars,                    # Un vecteur avec les noms des variables d'intérêts
                       var_col        = NULL,   # Variable à croiser avec celles du vecteur
                       var_weight     = NULL,   # Variable de pondération, sinon = NULL
                       weight_norm    = FALSE,  # Normaliser la 
                       table_type     = "all",  # Type de table : "all", "eff", "row", "col"
                       useNA          = TRUE,   # TRUE/FALSE : Ajout des valeurs manquantes
                       chi2_test      = TRUE,   # TRUE/FALSE : Ajout du test du Chi²
                       arrondi        = 2,      # Nombre de chiffres après la virgule
                       use_labels     = "no",   # Utiliser les labels : "no", "yes", "both"
                       add_blank_rows = TRUE,   # TRUE/FALSE : Ajout d'une ligne vide entre les variables
                       eff_in_name    = TRUE,   # TRUE/FALSE : Ajout des effectifs dans les noms des modalités
                       excel_export   = FALSE,  # TRUE/FALSE : Création d'un fichier excel et son chemin
                       excel_filepath = "./table_auto.xlsx", # Chemin vers le fichier excel
                       view_html      = TRUE    # TRUE/FALSE : Affiche la table en HTML
){
  
  ### OPTIONS                ----
  # Ecriture scientifique
  options(scipen=9999)
  
  ### GESTION LIBRARY        ----
  # Liste des packages à charger
  packages <- c("tidyverse", "openxlsx", "haven")
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
  if (table_type != "eff" && table_type != "row" && table_type != "col" && table_type != "all") {
    stop("Erreur : table_type doit être 'eff', 'row', 'col' ou 'all' ")
  }
  
  # Vérification du parametre use_labels
  if (use_labels != "no" && use_labels != "yes" && use_labels != "both") {
    stop("Erreur : use_labels doit être 'no', 'yes', ou 'both' ")
  }
  
  # Vérification de parametre useNA
  if (useNA != TRUE && useNA != FALSE) {
    stop("Erreur : useNA doit être TRUE ou FALSE")
  }
  
  # Vérification de parametre chi2_test
  if (chi2_test != TRUE && chi2_test != FALSE) {
    stop("Erreur : chi2_test doit être TRUE ou FALSE")
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
  
  
  # Variable pondération :
  if(is.null(var_weight) == T){
    ponder_calc <- rep(1,nrow(data))
  }else if (weight_norm == FALSE){
    ponder_calc <- with(data,get(var_weight))
  }else {
    ponder_calc <- (with(data,get(var_weight))*nrow(data))/sum(with(data,get(var_weight)))
  }
  
  
  # Données et labels:
  
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
    
    ### Création du tri à plat
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
      
      
      # CHI²
      if(chi2_test == TRUE){
        # Sauvegarde la table du chi² et on capture le message d'erreur (s'il y en a)
        
        chi2 <<- chisq.test(xtabs(ponder_calc~
                                    get(colnames(dt[vars[i]]))+
                                    get(var_col),
                                  data=dt,
                                  addNA = useNA))
        
        msg_chi2 <- tryCatch({
          
          chisq.test(xtabs(ponder_calc~
                             get(colnames(dt[vars[i]]))+
                             get(var_col),
                           data=dt,
                           addNA = useNA))
          
        }, warning = function(warn) {
          warn_message <<- conditionMessage(warn)
          return(warn_message)
        })
        
      }
      
      
      # La table :
      
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
      
      
      if(table_type %in% c("eff","all")){
        tab_eff <- tab_commune %>% 
          mutate(ENSEMBLE = round(ENSEMBLE,arrondi)) |> 
          pivot_wider(names_from = Groupe, values_from = ENSEMBLE)  %>% 
          mutate(Var = vars[i]) %>%  
          select(Var,Levels,everything()) %>% 
          left_join(select(desc_uni, Var,Levels, ENSEMBLE), by = c("Var","Levels"))
        
        # Variables du Chi²
        if(chi2_test == TRUE){
          tab_eff <- tab_eff %>% mutate(
            pvalue = round(chi2$p.value,3),
            ddl = chi2$parameter,
            chi2_warn = "OK")
          # S'il y a un warning dans chisq.test
          if (inherits(msg_chi2, "character")) { tab_eff <- tab_eff %>% mutate(chi2_warn = "Pas_OK")}
        }
        
        # On ajoute la boucle à la table générale
        desc_bi_eff <- rbind(desc_bi_eff,tab_eff)
        
        # Ajout de ligne blanche entre les variables
        if(add_blank_rows == TRUE){
          desc_bi_eff <- rbind(desc_bi_eff, rep(NA, ncol(desc_bi_eff)))
        } # Fin Ligne blanche
        
      } # FIN eff
      
      
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
        if(chi2_test == TRUE){
          tab_row <- tab_row %>% mutate(
            pvalue = round(chi2$p.value,3),
            ddl = chi2$parameter,
            chi2_warn = "OK")
          # S'il y a un warning dans chisq.test
          if (inherits(msg_chi2, "character")) { tab_row <- tab_row %>% mutate(chi2_warn = "Pas_OK")}
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
        if(chi2_test == TRUE){
          tab_col <- tab_col %>% mutate(
            pvalue = round(chi2$p.value,3),
            ddl = chi2$parameter,
            chi2_warn = "OK")
          # S'il y a un warning dans chisq.test
          if (inherits(msg_chi2, "character")) { tab_col <- tab_col %>% mutate(chi2_warn = "Pas_OK")}
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
  
  
  
  # AJOUT PREMIERE LIGNE
  
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
  
  
  if(is.null(var_col) == FALSE){
    if(table_type %in% c("eff","all")){
      
      first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
        group_by(get(var_col)) %>% 
        summarise(Eff = round(sum(ponderation),arrondi)) %>% 
        rename(Groupe = 1) %>% 
        mutate(Groupe = if_else(is.na(Groupe), "Val.Manq.", as.character(Groupe))) %>% 
        mutate(ENSEMBLE = sum(Eff),
               Var =" ",
               Levels = "ENSEMBLE") %>%  
        pivot_wider(names_from = Groupe, values_from = Eff) %>% 
        select(-ENSEMBLE,Var, Levels,everything(),ENSEMBLE)
      
      
      if(useNA == FALSE){
        if(any(is.na(names(with(dt,table(get(var_col),useNA = "ifany")))))){
          first_row <- first_row %>% 
            select(-Val.Manq.)
        }}
      
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
    
    
    if(table_type %in% c("row","all")){
      
      first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
        group_by(get(var_col)) %>% 
        summarise(Eff = sum(ponderation)) %>% 
        mutate(Pct = round((Eff*100)/sum(Eff),arrondi)) %>% 
        select(-Eff) %>% 
        rename(Groupe = 1) %>% 
        mutate(Groupe = if_else(is.na(Groupe), "Val.Manq.", as.character(Groupe))) %>% 
        mutate(ENSEMBLE = sum(Pct),
               Var =" ",
               Levels = "ENSEMBLE") %>%  
        pivot_wider(names_from = Groupe, values_from = Pct) %>% 
        select(-ENSEMBLE,Var, Levels,everything(),ENSEMBLE)
      
      if(useNA == FALSE){
        if(any(is.na(names(with(dt,table(get(var_col),useNA = "ifany")))))){
          first_row <- first_row %>% 
            select(-Val.Manq.)
        }}
      
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
    
    if(table_type %in% c("col","all")){
      
      first_row <- dt %>% mutate(ponderation = ponder_calc) %>% 
        group_by(get(var_col)) %>% 
        summarise(Eff = 100) %>% 
        rename(Groupe = 1) %>% 
        mutate(Groupe = if_else(is.na(Groupe), "Val.Manq.", as.character(Groupe))) %>% 
        mutate(ENSEMBLE = 100,
               Var =" ",
               Levels = "ENSEMBLE") %>%  
        pivot_wider(names_from = Groupe, values_from = Eff) %>% 
        select(-ENSEMBLE,Var, Levels,everything(),ENSEMBLE)
      
      
      if(useNA == FALSE){
        if(any(is.na(names(with(dt,table(get(var_col),useNA = "ifany")))))){
          first_row <- first_row %>% 
            select(-Val.Manq.)
        }}
      
      
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
      
      
    }# Fin col
  } # Fin bivar
  
  
  
  ### EXPORT                 ----
  
  # Export
  if(excel_export == TRUE){
    
    if(is.null(var_col) == TRUE){
      
      wb <- createWorkbook()
      addWorksheet(wb,"univar")
      writeData(wb, "univar", desc_uni)
      saveWorkbook(wb, excel_filepath, overwrite = TRUE)
      
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
      }else{
        addWorksheet(wb,"univar")
        writeData(wb, "univar", desc_uni)
        
        obj_names <- c("desc_bi_eff", "desc_bi_row", "desc_bi_col")
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
      
      
      if(chi2_test == TRUE){
        nb_col = length(select(html_dt, -c(Var, Levels, pvalue, ddl, chi2_warn)))
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
      
    }
  }
}

