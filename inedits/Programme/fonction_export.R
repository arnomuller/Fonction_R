########################################
######       SAUVEGARDE            #####
########################################



min_version = "2.2.2"
# Si le package n'est pas installé du tout
if (!requireNamespace("svglite", quietly = TRUE)) {
  message("Le package 'svglite' n'est pas installé. Installation en cours...")
  install.packages("svglite")
  return(invisible(TRUE))
}

# Vérifier la version installée
installed_ver <- as.character(utils::packageVersion("svglite"))

if (utils::compareVersion(installed_ver, min_version) < 0) {
  message(
    sprintf("Version installée de svglite (%s) < %s. Mise à jour en cours...",
            installed_ver, min_version)
  )
  install.packages(get("svglite"))
  return(invisible(TRUE))
}

rm(min_version,installed_ver)

#################################



ined_export = function(
    graph = NULL,
    format = "moyen",
    nomfichier = "graphique"
){
  
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
  
  extension = ".svg"
  ggsave(graph, filename = paste0(nomfichier,extension), 
         height = hauteur, 
         width = largeur,
         units = "cm", 
         fix_text_size = F)
  
  
}










