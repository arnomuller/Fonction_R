import_sas_label <- function(data_file, catalog_file){
  
  ### GESTION LIBRARY ----
  
  # Liste des packages à charger
  packages <- c("haven")
  
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  
  
  
  ### Import des données ----
  
  # Je crée un data.frame
  as.data.frame(
    # Je vais appliquer une fonction à toutes les colonnes de la liste issue
    # du read_sas avec le catalog.
    lapply(as.list(read_sas(data_file,
                            catalog_file = catalog_file)), 
           function(col) {
             # Pour toutes les variables qui ont un label $
             # qui ont autant de label que de valeurs possibles
             if (any(class(col) == "haven_labelled") & 
                 length(unique(col)) == length(attributes(col)$labels)) {
               
               # On transforme en factor
               return(as_factor(col))
               
               
               # ATTENTION : les NA sont recodés en "", il ne faut donc pas le 
               # compter parmis les modalités de la variable
             } else if(length(unique(col)) == length(attributes(col)$labels)+1){
               
               #Ne s'applique qu'au variable character
               if(any(class(col) == "character")){
                 if(any(unique(col) == "")){
                   # Si effectivement on a le bon nombre de modalités et de 
                   # labels alors on transforme en factor
                   return(as_factor(col))
                 } else {
                   # Sinon on touche pas
                   return(col)
                 }
               } else {
                 # Sinon on touche pas
                 return(col)
               }
               
             } else {
               # Sinon on touche pas
               return(col)
             }
           }))
  
  
}