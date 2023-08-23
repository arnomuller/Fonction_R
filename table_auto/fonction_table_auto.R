####################################
####   FONCTION table_auto()    ####
####################################


## PARAMETRES ----

# donnees     : Un data.frame
# vars        : Un vecteur avec les noms de variables de donnee
# var_crois   : Le nom d'une variable, ex : "sexe"
# table_type  : "eff", "pct_ligne", "pct_col"
# ponder      : Le nom d'une variable, ex : "poids"
# val.manq    : "oui" ou "non"
# arrondi     : Un chiffre
# sautdeligne : "oui" ou "non"
# export_XLS  : "oui" ou "non"


## PACKAGES ----

library(tidyverse)
library(questionr)
library(openxlsx)


## FONCTION ----

# Lancer la fonction suivante pour pourvoir l'appeler dans vos prochains scripts :

table_auto <- function(donnees, 
                       vars, 
                       var_crois   = NULL, 
                       table_type  = "eff",
                       ponder      = NULL, 
                       val.manq    = "oui",
                       arrondi     = 2, 
                       sautdeligne = "oui",
                       export_XLS  = "non"){
  
  ### OPTIONS ----
  
  options(scipen=9999)
  
  ### GESTION LIBRARY ----
  
  # Liste des packages à charger
  packages <- c("tidyverse", "questionr", "openxlsx")
  
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  
  
  ### GESTION DES ERREURS ----
  
  # Vérification du parametre table_type
  if (table_type != "eff" && table_type != "pct_ligne" && table_type != "pct_col") {
    stop("Erreur : table_type doit être 'eff', 'pct_ligne', ou 'pct_col' ")
  }
  
  # Vérification de parametre val.manq
  if (val.manq != "oui" && val.manq != "non") {
    stop("Erreur : val.manq doit être 'oui' ou 'non'")
  }
  
  # Vérification de parametre sautdeligne
  if (sautdeligne != "oui" && sautdeligne != "non") {
    stop("Erreur : sautdeligne doit être 'oui' ou 'non'")
  }
  
  # Vérification de parametre export_XLS
  if (export_XLS != "oui" && export_XLS != "non") {
    stop("Erreur : export_XLS doit être 'oui' ou 'non'")
  }
  
  
  ### GESTION DES PARAMETRES ----
  
  # Données : 
  dt <- donnees
  
  # Variable à croiser : 
  if(is.null(var_crois) == T){
    var_crois_OK <- "NON"
  } else{
    var_crois_OK <- "OUI"
  }
  
  # Variable pondération : 
  if(is.null(ponder) == T){
    ponder_calc <- rep(1,nrow(dt))
  } else{
    ponder_calc <- with(dt,get(ponder))
  }
  
  # Valeurs manquantes
  if(val.manq == "oui"){
    NA_oupas <- "always"
  } else if(val.manq == "non"){
    NA_oupas <- "no"
  }
  
  # Choix du type de pourcentage
  if(table_type == "pct_ligne"){
    pct_type <- 1
  } else if(table_type == "pct_col"){
    pct_type <- 2
  }
  
  
  ### CREATION DE LA TABLE EMPILEE ----
  
  # TABLE DES EFFECTIFS ----
  if(table_type == "eff"){
    
    # Création table
    desc_grp <- data.frame()
    desc_T <- data.frame()
    
    for (i in c(1:length(vars))) {
      
      ### Création du tri à plat
      desc_T <- rbind(desc_T,
                      as.data.frame(wtd.table(dt[,vars[i]], 
                                              weights = ponder_calc, 
                                              useNA=NA_oupas)) %>% 
                        rename(Levels = Var1,
                               ENSEMBLE = Freq) %>%       
                        mutate(Levels = as.character(Levels),
                               Levels = if_else(is.na(Levels), "Val.Manq.", Levels)) %>% 
                        mutate(Var = colnames(dt[vars[i]])) %>%  
                        select(Var,Levels,ENSEMBLE))
      
      # Ajout de ligne blanche entre les variables
      if(sautdeligne == "oui"){
        desc_T <- rbind(desc_T, rep(NA, ncol(desc_T)))
      }
      
      
      ### Création tri croisé si var_crois_OK
      if(var_crois_OK == "OUI"){
        
        
        tab <- wtd.table(dt[,vars[i]], 
                         with(dt,get(var_crois)),
                         weights = ponder_calc, 
                         useNA=NA_oupas)
        
        
        
        desc_grp <- rbind(desc_grp,
                          as.data.frame(tab) %>% 
                            pivot_wider(names_from = Var2, values_from = Freq) %>% 
                            rename(Levels = Var1) %>% 
                            mutate(Levels = as.character(Levels),
                                   Levels = if_else(is.na(Levels), "Val.Manq.", Levels)) %>% 
                            mutate(Var = colnames(dt[vars[i]]))    %>%
                            mutate(pvalue = chisq.test(
                              xtabs(ponder_calc~ 
                                      get(colnames(dt[vars[i]]))+
                                      get(var_crois),
                                    data=dt))$p.value)              %>% 
                            mutate(ddl = chisq.test(
                              xtabs(ponder_calc~ 
                                      get(colnames(dt[vars[i]]))+
                                      get(var_crois),
                                    data=dt))$parameter[[1]])       %>%
                            mutate(danger = 0) %>% 
                            select(Var,Levels, everything()))
        
        
        result <- tryCatch({
          chisq.test(
            xtabs(ponder_calc~ 
                    get(colnames(dt[vars[i]]))+
                    get(var_crois),
                  data=dt))
        }, warning = function(warn) {
          warn_message <- conditionMessage(warn)
          return(warn_message)
        })
        
        # Est-ce qu'il y a un warning dans chisq.test
        if (inherits(result, "character")) {
          desc_grp <- desc_grp %>% 
            mutate(danger = if_else(Var == vars[i], 1, danger))
          
        } 
        
        
        # Ajout de ligne blanche entre les variables
        if(sautdeligne == "oui"){
          desc_grp <- rbind(desc_grp, rep(NA, ncol(desc_grp)))
        }
      }
    }
    
    
    
    ## Création base finale propre pour tris croisés
    if(var_crois_OK == "OUI"){
      
      desc_grp <- desc_grp %>% 
        bind_cols(select(desc_T, "ENSEMBLE")) 
      
      tabdesc = as.data.frame(t(
        c("", "ENSEMBLE", with(dt, round(wtd.table(get(var_crois), weights = ponder_calc, useNA=NA_oupas),arrondi)),NA, NA, NA,sum(ponder_calc))))  %>%
        `colnames<-`(colnames(desc_grp))
      
      if(sautdeligne == "oui"){
        tabdesc <- rbind(tabdesc, rep(NA, ncol(tabdesc)))
      }
      
      tabdesc <- tabdesc %>% 
        rbind(desc_grp) %>% 
        select(Var,Levels, ENSEMBLE, everything()) %>% 
        mutate_at(colnames(.)[-c(1,2, ncol(.))], ~round(as.numeric(.),arrondi)) %>% 
        mutate(pvalue = if_else(pvalue == "0", "0,0000", as.character(round(as.numeric(pvalue),4))))
      
      
      
      ## Création base finale propre pour trier à plat
    } else {
      
      desc_T <- desc_T %>% 
        mutate_at(colnames(.)[-c(1,2)], ~round(as.numeric(.),arrondi)) 
      
      tabdesc = as.data.frame(t(
        c("", "ENSEMBLE" ,sum(ponder_calc)))) %>%
        `colnames<-`(colnames(desc_T))
      
      if(sautdeligne == "oui"){
        tabdesc <- rbind(tabdesc, rep(NA, ncol(tabdesc)))
      }
      
      tabdesc <- tabdesc %>% 
        rbind(desc_T)
    }
    
  }
  
  
  
  
  
  
  if(table_type %in% c("pct_ligne","pct_col")){
    
    desc_grp <- data.frame()
    desc_T <- data.frame()
    
    
    for (i in c(1:length(vars))) {
      
      
      ### Création du tri à plat
      desc_T <- rbind(desc_T,             
                      as.data.frame(prop.table(
                        wtd.table(dt[,vars[i]],  
                                  weights = ponder_calc, 
                                  useNA=NA_oupas))*100) %>% 
                        rename(Levels = Var1,
                               ENSEMBLE = Freq) %>%      
                        mutate(Levels = as.character(Levels),
                               Levels = if_else(is.na(Levels), "Val.Manq.", Levels)) %>% 
                        mutate(Var = colnames(dt[vars[i]])) %>% 
                        select(Var,Levels,ENSEMBLE))     
      
      
      # Ajout de ligne blanche entre les variables
      if(sautdeligne == "oui"){
        desc_T <- rbind(desc_T, rep(NA, ncol(desc_T)))
      }
      
      
      ### Création tri croisé si var_crois_OK
      if(var_crois_OK == "OUI"){
        
        
        tab <- prop.table(wtd.table(dt[,vars[i]], 
                                    with(dt,get(var_crois)),
                                    weights = ponder_calc, 
                                    useNA=NA_oupas),pct_type)*100
        
        
        desc_grp <- rbind(desc_grp,
                          as.data.frame(tab) %>% 
                            pivot_wider(names_from = Var2, values_from = Freq) %>% 
                            rename(Levels = Var1) %>% 
                            mutate(Levels = as.character(Levels),
                                   Levels = if_else(is.na(Levels), "Val.Manq.", Levels)) %>% 
                            mutate_all(~ifelse(is.nan(.), 0, .)) %>% 
                            mutate(Var = colnames(dt[vars[i]]))    %>% 
                            mutate(pvalue = chisq.test(
                              xtabs(ponder_calc~ 
                                      get(colnames(dt[vars[i]]))+
                                      get(var_crois),
                                    data=dt))$p.value)    %>%
                            mutate(ddl = chisq.test(
                              xtabs(ponder_calc~ 
                                      get(colnames(dt[vars[i]]))+
                                      get(var_crois),
                                    data=dt))$parameter[[1]]) %>%
                            mutate(danger = 0) %>%     
                            select(Var,Levels, everything()))
        
        
        result <- tryCatch({
          chisq.test(
            xtabs(ponder_calc~ 
                    get(colnames(dt[vars[i]]))+
                    get(var_crois),
                  data=dt))
        }, warning = function(warn) {
          warn_message <- conditionMessage(warn)
          return(warn_message)
        })
        
        # Est-ce qu'il y a un warning dans chisq.test
        if (inherits(result, "character")) {
          desc_grp <- desc_grp %>% 
            mutate(danger = if_else(Var == vars[i], 1, danger))
          
        } 
        
        # Ajout de ligne blanche entre les variables
        if(sautdeligne == "oui"){
          desc_grp <- rbind(desc_grp, rep(NA, ncol(desc_grp)))
        }
      }
    }
    
    
    ## Création base finale propre pour trier croisé
    if(var_crois_OK == "OUI"){
      
      # POUR POURCENTAGE LIGNE
      if(pct_type == 1){
        desc_grp <- desc_grp %>% 
          bind_cols(desc_T %>% 
                      mutate(ENSEMBLE= if_else(ENSEMBLE == 0, ENSEMBLE, 100)) %>% 
                      select( "ENSEMBLE")) 
        
        tabdesc = as.data.frame(t(
          c("", "ENSEMBLE", round(prop.table(with(dt, wtd.table(get(var_crois), weights = ponder_calc, useNA=NA_oupas)))*100,arrondi),
            NA,NA,NA,100)))  %>%
          `colnames<-`(colnames(desc_grp))
        
        if(sautdeligne == "oui"){
          tabdesc <- rbind(tabdesc, rep(NA, ncol(tabdesc)))
        }
        
        tabdesc <- tabdesc %>% 
          rbind(desc_grp) %>% 
          select(Var,Levels,ENSEMBLE, everything()) %>% 
          mutate_at(colnames(.)[-c(1,2, ncol(.))], ~round(as.numeric(.),arrondi)) %>% 
          mutate(pvalue = if_else(pvalue == "0", "0,0000", as.character(round(as.numeric(pvalue),4))))
        
        # POUR POURCENTAGE COLONNES
      } else if(pct_type == 2){
        
        desc_grp <- desc_grp %>% 
          bind_cols(desc_T %>% 
                      select( "ENSEMBLE"))
        
        
        tabdesc = as.data.frame(t(
          c("", "ENSEMBLE", rep(100,length(with(dt,names(table(get(var_crois), useNA = NA_oupas))))),
            NA,NA,NA,100)))  %>%
          `colnames<-`(colnames(desc_grp))
        
        if(sautdeligne == "oui"){
          tabdesc <- rbind(tabdesc, rep(NA, ncol(tabdesc)))
        }
        
        tabdesc <- tabdesc %>% 
          rbind(desc_grp) %>% 
          select(Var,Levels,ENSEMBLE, everything()) %>% 
          mutate_at(colnames(.)[-c(1,2, ncol(.))], ~round(as.numeric(.),arrondi)) %>% 
          mutate(pvalue = if_else(pvalue == "0", "0,0000", as.character(round(as.numeric(pvalue),4))))
      }
      
      
    } else {
      # TRI A PLAT POURCENTAGE
      desc_T <- desc_T %>% 
        mutate_at(colnames(.)[-c(1,2)], ~round(as.numeric(.),arrondi)) 
      
      tabdesc = as.data.frame(t(
        c("", "ENSEMBLE" ,100))) %>%
        `colnames<-`(colnames(desc_T))
      
      if(sautdeligne == "oui"){
        tabdesc <- rbind(tabdesc, rep(NA, ncol(tabdesc)))
      }
      
      tabdesc <- tabdesc %>% 
        rbind(desc_T)
    }
  }
  
  # Enregistrer le data.frame dans l'environnement global
  assign("tabdesc", tabdesc, envir = .GlobalEnv)
  
  
  # Export
  if(export_XLS == "oui"){
    # Spécifiez le nom du fichier Excel de destination
    file_path <- "./table_empilee.xlsx"
    # Créez un objet workbook
    wb <- createWorkbook()
    
    # Ajoutez chaque data.frame en tant que feuille à l'objet workbook
    addWorksheet(wb, table_type)
    writeData(wb, table_type, tabdesc)
    # Enregistrez l'objet workbook dans un fichier Excel
    saveWorkbook(wb, file_path, overwrite = TRUE)
  }
  
}










