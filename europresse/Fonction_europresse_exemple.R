#################################
####   EXEMPLE UTILISATION   ####
####      table_auto()       ####
#################################


## On appelle la fonction depuis github
source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/europresse/fonction_europresse.R")


# Exemple de fonctionnement
europresse(html = "D:/INED_Formation/Europresse/DATA/biblioeuropresse20240221115914.HTML",
           name = "dt_europresse",
           min_nchar = 500,
           suppr_doubl = "non",
           seuil_doubl = 50)


# html        : Le chemin vers un fichier HTML issue d'Europresse
# name        : Le nom du data.frame en sortie
# min_nchar   : Filtre sur le nombre de caractères dans les textes
# suppr_doubl : "oui" ou "non", pour supprimer les doublons
# seuil_doubl : Le seuil de "sévérité" pour la suppression


