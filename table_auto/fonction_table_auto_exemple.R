#################################
####   EXEMPLE UTILISATION   ####
####      table_auto()       ####
#################################


## On appelle la fonction depuis github
source("https://raw.githubusercontent.com/arnomuller/table_auto/main/fonction_table_auto.R")


## Import données
library(questionr)
data("hdv2003")

## Définition des paramètres

# On s'intéresse aux variables 10 à 19
vars      <- colnames(hdv2003)[10:19]
vars

# On les croise avec la variable sexe
var_crois <- "sexe"
# Si pas de croisement écrire NULL
var_crois <- NULL

# Variable de pondération
ponder    <- "poids"
# Si pas de pondération :
ponder    <- NULL



# On lance la fonction
table_auto(hdv2003,               # Base de données
           vars,                  # Un vecteur avec les noms des variables d'intérêts
           var_crois   = "sexe",  # Variable à croiser avec celles du vecteur
           table_type  = "eff",   # Type de table : "eff", "pct_ligne", "pct_col"
           ponder      = "poids", # Variable de pondération, sinon = NULL
           val.manq    = "non",   # Ajout des valeurs manquantes
           arrondi     = 3,       # Nombre de chiffres après la virgule
           sautdeligne = "oui",   # Sauté des lignes entre les variables
           export_XLS  = "oui")   # Création d'un fichier excel : tableau_empilé
