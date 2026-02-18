########################################
######       SAUVEGARDE            #####
########################################





#################################



ined_export = function(
    graph = NULL,
    fichier = "graphique",
    format = "moyen"
){
  
  ### GESTION LIBRARY           ----
  # Liste des packages à charger
  packages <- c("tidyverse", "magick")
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  
  largeur = case_when(
    format == "petit" ~ 9.23,
    format == "moyen" ~ (9.23*2)+0.51,
    format == "grand" ~ (9.23*2)+0.51
  )
  hauteur = case_when(
    format == "petit" ~ 8.5,
    format == "moyen" ~ 9.54, # Pourquoi ?
    format == "grand" ~ 21.75
  )
  
  extension = ".pdf"
  ggsave(plot = graph, 
         filename = paste0(fichier,extension), 
         device = cairo_pdf,
         height = hauteur, 
         width = largeur,
         units = "cm")
  
  ggsave(plot = p, 
         filename = paste0(fichier,".svg"), 
         device = "svg",
         height = hauteur, 
         width = largeur,
         units = "cm")
  
  message("Deux fichiers (svg + pdf) exportés ici : ", normalizePath(paste0(fichier)))
  
  # Lecture avec magick
  img <- image_read_pdf(paste0(fichier,extension))
  suppressMessages(
    invisible(capture.output(print(img)))
  )
  
  
}










