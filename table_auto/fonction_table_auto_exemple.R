#################################
####   EXEMPLE UTILISATION   ####
####      table_auto()       ####
#################################


## On appelle la fonction depuis github
# source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/table_auto/fonction_table_auto.R")


## Import données
library(questionr)
data("hdv2003")

## Définition des paramètres

# Pour vars : 
# On s'intéresse aux variables 10 à 19
mes_vars <- colnames(hdv2003)[10:19]
mes_vars

# On peut définir des modalités qui ne doivent pas être utilisées
# Exemple avec une modalité
junk = c("NSP ou NVPR") 
# Exemple avec trois modalités
junk = c("NSP ou NVPR", "Cadre", "Rejet")


## Utilisation de la fonction 

table_auto(hdv2003,                    # Base de données
           mes_vars,                   # Un vecteur avec les noms des variables d'intérêts
           var_col        = "qualif",  # Variable à croiser avec celles du vecteur
           table_type     = "all",     # Type de table : "all", "eff", "row", "col", ou "mix"
           var_weight     = "poids",   # Variable de pondération, sinon = NULL
           weight_norm    = FALSE,     # TRUE/FALSE : Normaliser la pondération
           useNA          = FALSE,     # TRUE/FALSE : Ajout des valeurs manquantes
           exclude        = junk,      # exclure des modalités
           use_test       = "chi2",    # Type de test : "chi2", "fisher", "chi2_noponder", "no"
           arrondi        = 2,         # Nombre de chiffres après la virgule
           use_labels     = "no",      # Utiliser les labels : "no", "yes", "both"
           add_blank_rows = TRUE,      # TRUE/FALSE : Ajout d'une ligne vide entre les variables
           eff_in_name    = "yes",     # Ajout des effectifs dans les noms des modalités : "yes","noponder", "no"
           excel_export   = FALSE,     # TRUE/FALSE : Création d'un fichier excel puis son chemin
           excel_filepath = "./table_auto.xlsx", # Seulement si excel_export = TRUE
           view_html      = TRUE)      # TRUE/FALSE : Afficher la table en HTML
