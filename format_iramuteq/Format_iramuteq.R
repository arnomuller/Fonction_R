

# Fonction pour nettoyer les espaces et les "_"
nettoyer_noms <- function(nom) {
  # Vérifier s'il y a des espaces ou des "_" dans le nom
  if (any(sapply(nom, function(x) grepl(" |_", x)))) {
    warning("Des espaces ou des '_' ont été supprimés lors du nettoyage.")
  }
  
  # Supprimer les espaces et les "_"
  nom_nettoye <- gsub(" ", "", nom)
  nom_nettoye <- gsub("_", "", nom_nettoye)
  return(nom_nettoye)
}




# Créer la fonction pour exporter les articles en .txt
format_iramuteq <- function(dataframe, 
                            nom_fichier = "corpus_iramuteq.txt",
                            var_texte,
                            vars_metadonnees) {
  # Vérifier si les noms de colonnes spécifiés existent dans le dataframe
  if (!all(vars_metadonnees %in% colnames(dataframe))) {
    stop("Certains noms de colonnes spécifiées dans vars_metadonnees n'existent pas dans le dataframe.")
  }
  
  
  
  # Nettoyer les noms de vars_metadonnees
  vars_prefixe_nettoyes <- sapply(vars_metadonnees, nettoyer_noms)
  
  # Nettoyer les valeurs correspondant aux colonnes de vars_metadonnees dans le dataframe
  dataframe[, vars_metadonnees] <- lapply(dataframe[, vars_metadonnees], nettoyer_noms)
  
  # Nettoyer les * du texte
  dataframe$Texte <- gsub("\\*", "", dataframe$Texte)
  
  # Ouvrir le fichier en mode écriture
  con <- file(nom_fichier, "w")
  
  # Parcourir le dataframe
  for (i in 1:nrow(dataframe)) {
    # Créer le préfixe avec les variables nettoyées séparées par "_"
    prefixe <- paste0("*", paste( vars_prefixe_nettoyes, dataframe[i, vars_metadonnees], sep = "_", collapse = " *"), " ")
    
    # Écrire les variables en préfixe spécifiées par l'utilisateur
    ligne_prefixe <- paste0("**** ", prefixe, "\n")
    writeLines(ligne_prefixe, con)
    
    # Écrire le texte de l'article
    writeLines(dataframe[i, "Texte"], con)
    
    # Ajouter une ligne vide pour séparer les articles
    writeLines("\n", con)
  }
  
  # Fermer le fichier
  close(con)
  
  cat("Exportation terminée.\n")
}













