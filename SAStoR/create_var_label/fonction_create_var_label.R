create_var_label <- function(donnees, varlabel){
  
  ## Paramètres
  
  variable <- with(donnees, get(varlabel))
  varlabels_attr    <- attributes(variable)
  var_labels_values <- as.numeric(varlabels_attr$labels)
  var_labels_names  <- names(varlabels_attr$labels)
  new_var_name      <- varlabels_attr$format.sas
  var_bounds <- c(var_labels_values, max(as.numeric(variable))+1)
  
  newvar <- cut(as.numeric(variable), 
                breaks = var_bounds, 
                right = F,
                labels = var_labels_names)
  
  
  donnees = donnees %>% 
    mutate(newvar = newvar) %>% 
    `colnames<-`(c(colnames(.[1:ncol(.)-1]),new_var_name))
  
}