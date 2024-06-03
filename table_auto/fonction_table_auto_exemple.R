#################################
####   EXEMPLE UTILISATION   ####
####      table_auto()       ####
#################################


## On appelle la fonction depuis github
source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/table_auto/fonction_table_auto.R")


## Import données
library(questionr)
data("hdv2003")

## Définition des paramètres

# On s'intéresse aux variables 10 à 19
vars      <- colnames(hdv2003)[10:19]
vars


# On lance la fonction
table_auto(hdv2003,                  # Base de données
           vars,                     # Un vecteur avec les noms des variables d'intérêts
           var_col        = "sexe",  # Variable à croiser avec celles du vecteur
           table_type     = "all",   # Type de table : "all", "eff", "row", "col"
           var_weight     = "poids", # Variable de pondération, sinon = NULL
           useNA          = TRUE,    # TRUE/FALSE : Ajout des valeurs manquantes
           chi2.test      = TRUE,    # TRUE/FALSE : Ajout du test du Chi²
           arrondi        = 3,       # Nombre de chiffres après la virgule
           add_blank_rows = TRUE,    # TRUE/FALSE : Ajout d'une ligne vide entre les variables
           eff_in_name    = TRUE,    # TRUE/FALSE : Ajout des effectifs dans les noms des modalités
           excel_export   = TRUE,    # TRUE/FALSE : Création d'un fichier excel et son chemin
           excel_filepath = "./table_auto.xlsx")   
