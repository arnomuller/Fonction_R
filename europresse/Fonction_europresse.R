####################################
####   FONCTION europresse()    ####
####################################


## PARAMETRES ----

# html        : Le chemin vers un fichier HTML issue d'Europresse
# name        : Le nom du data.frame en sortie
# min_nchar   : Filtre sur le nombre de caractères dans les textes
# suppr_doubl : "oui" ou "non", pour supprimer les doublons
# seuil_doubl : Le seuil de "sévérité" pour la suppression



# Fonction spécifique pour les dates manquantes
get_second_element <- function(node, xpath) {
  found_elements <- xml_find_all(node, xpath)
  if (length(found_elements) >= 2) {
    return(found_elements[[2]] %>% xml_text(trim = TRUE))
  } else {
    return(NA_character_)
  }
}



# La vraie fonction : 
europresse <- function(html,
                       name = "dt_europresse",
                       min_nchar = NULL,
                       suppr_doubl = "oui",
                       seuil_doubl = 50){
  
  ### OPTIONS ----
  
  options(scipen=9999)
  
  # Packages nécessaires
  load.lib <- c("xml2", "stringr", "stringdist", "stringi","lubridate", "dplyr", "tidyr","purrr") 
  # Installation des manquants
  install.lib <- load.lib[!load.lib %in% installed.packages()]
  for (lib in install.lib) install.packages(lib,dependencies=TRUE) 
  # On charge les packages
  sapply(load.lib,require,character=TRUE)
  
  
  
  # Vérification de parametre suppr_doubl
  if (suppr_doubl != "oui" && suppr_doubl != "non") {
    stop("Erreur : suppr_doubl doit être 'oui' ou 'non'")
  }
  
  
  
  ### IMPORT du corpus ----
  
  # Lire le fichier HTML
  doc <- read_html(html)
  # Sélectionner les articles
  articles <- xml_find_all(doc, "//article")
  
  
  ### CREATION DE VARIABLES ----
  
  ## JOURNAL
  journal <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, ".//header/div[@class='rdp__DocPublicationName']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp
  })
  
  journal_manquant <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, "./header/div[@class='sm-margin-bottom']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp
  })
  
  
  ## AUTEUR
  auteur <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, "./header/div[@class='docAuthors']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp
  })
  
  auteur_manquant <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, "./header/p[@class='sm-margin-bottomNews']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp
  })
  
  
  ## TITRE 
  titre <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, "./header/div[@class='titreArticle']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp
  })
  
  
  ## DATE
  date <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, ".//div[@class='publiC-lblNodoc']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp <- substr(tmp, 6, 13)
  })
  # On met la date au bon format
  date <- as.Date(date, "%Y%m%d") 
  
  # Dates Manquantes
  # Peut-être stocké à la fin
  date_manquant <- map_chr(articles, get_second_element, xpath = "./header/div[@class='sm-margin-bottom']")
  
  
  ## TEXTE
  texte <- map_chr(articles, ~ {
    tmp <- xml_find_first(.x, ".//div[@class='DocText clearfix']") %>%
      xml_text(trim = TRUE)
    if (is.null(tmp)) tmp <- NA_character_
    tmp
  })
  
  
  
  #### CREATION DES DONNEES ----
  dt_europresse <- data.frame(Journal = journal,
                              Titre = titre,
                              Date = date,
                              Date_manq = date_manquant,
                              Auteur = auteur,
                              Texte = texte)  
  
  
  #### NETTOYAGE DES DONNEES ----
  
  ## JOURNAUX MANQUANTS
  dt_europresse <- dt_europresse %>% 
    mutate(Journal = ifelse(is.na(Journal), journal_manquant, Journal)) %>% 
    mutate(Journal = gsub("\n", "", Journal)) %>% 
    mutate(Journal = gsub(" ", "", Journal))
  
  
  ## AUTEURS MANQUANTS
  dt_europresse <- dt_europresse %>% 
    mutate(Auteur = ifelse(is.na(Auteur), auteur_manquant, Auteur)) 
  
  
  ## DATES MANQUANTES
  dt_europresse <- dt_europresse %>% 
    # J'enlève les caractères spéciaux pour pouvoir utiliser
    # la fonction separate()
    mutate(Date_manq = str_replace(Date_manq, "é", "e"),
           Date_manq = str_replace(Date_manq, "û", "u")) %>% 
    # separate() permet d'isoler le jour, le mois, l'année, et le nombre de mots
    # pour les dates manquantes, dans 5 variables
    separate(Date_manq, c("jour", "mois", "annee", "lettre", "mots")) %>% 
    # Attention : pour les sources anglaise, le mois et le jour sont inversés
    # Je les inverse dans jour2 et mois2
    mutate(jour2 = if_else(substr(mois,1,1) %in% c(0:9), mois, jour),
           mois2 = if_else(substr(jour,1,1) %in% c(0:9), mois, jour)) %>% 
    # Je supprime les variables inutiles
    select(-c(lettre,mots, jour, mois)) %>% 
    # Je crée une nouvelle variable de date en collant l'année, le mois, le jour
    mutate(date2 = paste(annee, mois2, jour2),
           # Je dois réecrire février et aôut correctement
           date2 = str_replace(date2, "fevrier", "février"),
           date2 = str_replace(date2, "aout", "août"),
           # Je transforme en format date.
           date2 = as_date(ymd(paste(date2)))) %>% 
    # Je remplace la date quand elle est manquante 
    mutate(Date = as_date(ifelse(is.na(Date), date2, Date)))  %>% 
    # Je garde les variables d'intérêts
    select(-c(jour2, mois2, annee, date2)) %>% 
    # Je trie dans l'ordre croissant de parution
    arrange(Date) %>% 
    mutate(Annee = as.numeric(format(Date, "%Y"))) %>% 
    mutate(Longueur = nchar(Texte) ) %>% 
    select(Annee, Date, Journal, Auteur, Titre, Texte, Longueur)
  
  
  
  
  # Minimum de caractères
  if(is.null(min_nchar) == F){
    dt_europresse <- dt_europresse %>%  
      filter(nchar(Texte) > min_nchar) 
  } 
  
  
  
  #### GESTION DES DOUBLONS ----
  
  
  if(suppr_doubl == "oui"){
    
    dt_europresse <- dt_europresse %>%
      mutate(extrait_debut = str_sub(Texte, 50, 150), 
             extrait_fin = str_sub(Texte, -150, -50)) 
    
    ### DEBUT DU TEXTE
    
    # Calcul des paires de distance
    # C'est ici qu'a lieu le calcul de distance entre tous les textes.
    dist <- stringdistmatrix(dt_europresse$extrait_debut) 
    ## Conversion en matrice 
    m <- as.matrix(dist)
    # Dans la matrice, on met 1000 comme valeur pour toutes les valeurs en dessous 
    # de la diagonale, pour éviter d'avoir deux fois la même mesure
    m[lower.tri(m)] <- 1000 
    # Dans la matrice, on met 1000 comme valeur pour la diagonale 
    # pour ne pas enlever un texte parce qu'il ressemble à lui-même...
    diag(m) <- 1000 
    
    # Sélection des paires proches
    # On regarde les positions pour lesquelles l'indice de dissimilarité est 
    # inférieure à 50. C'est ici donc qu'on fixe le seuil et qu'on peut le changer !  
    indices <- which(m < seuil_doubl, arr.ind = TRUE) 
    
    # Pour vérifier
    verif_dbt <- data.frame(row_text1 = indices[,1],
                            text1 = dt_europresse$extrait_debut[indices[,1]],
                            row_text2 = indices[,2],
                            text2 = dt_europresse$extrait_debut[indices[,2]],
                            doubl_type = "Début")
    
    # On supprime
    dt_europresse <- dt_europresse %>% slice(-indices[,2])
    
    
    
    #### FIN DU TEXTE
    
    
    ## Calcul des paires de distance
    dist <- stringdistmatrix(dt_europresse$extrait_fin)
    
    ## Conversion en matrice 
    m <- as.matrix(dist)
    m[lower.tri(m)] <- 1000 
    diag(m) <- 1000
    
    ## Sélection des paires proches
    indices <- which(m < seuil_doubl, arr.ind = TRUE)
    
    # Pour vérifier
    verif_fin <- data.frame(row_text1 = indices[,1],
                            text1 = dt_europresse$extrait_fin[indices[,1]],
                            row_text2 = indices[,2],
                            text2 = dt_europresse$extrait_fin[indices[,2]],
                            doubl_type = "Fin")
    
    ## Suppression des articles proches
    dt_europresse <- dt_europresse %>% slice(-indices[,2])
    
    
    
    ### SAUVEGARDE DE LA VERIF'
    
    verif_doubl <- verif_dbt %>% 
      bind_rows(verif_fin)
    
    # Enregistrer le data.frame dans l'environnement global
    assign("verif_doubl", verif_doubl, envir = .GlobalEnv)
    
    
    
    dt_europresse <- dt_europresse %>% 
      select(-c(extrait_debut,extrait_fin)) 
    
    
  } 
  
  
  
  # Enregistrer le data.frame dans l'environnement global
  assign(name, dt_europresse, envir = .GlobalEnv)
}





















