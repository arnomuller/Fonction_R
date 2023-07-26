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
             # Pour toutes les variables qui ont un label et
             # qui ont quasiment autant de label que de valeurs possibles
             if (any(class(col) == "haven_labelled")){
               
               
               # ATTENTION : les NA sont recodés en "", il ne faut donc pas le 
               # compter parmis les modalités de la variable
               # De même les NSP sont souvent 99 ou 88 etc.
               
               # Je choisis de faire la différence entre le nb de modalité
               # et le nombre de label < 5 pour ces cas là.
               # A réflechir
               
               if (length(unique(col)) - length(attributes(col)$labels) < 5){
                 # On transforme en factor
                 return(as_factor(col))

                 
               } else {
                 
                 # Sinon on touche pas
                 return(col)
               } 
             }else {
               # Sinon on touche pas
               return(col)
               
             }
             
         
           }))
  
  
}
