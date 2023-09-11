#### GLOBAL
# MULLER Arno


### Contexte  ----


# Cette application a été crée dans le cadre du projet VirageDOM de l'Ined, 
# mais peut-être utilisée sur d'autres bases de données.



### BUT : ----

# Faciliter l'utilisation des Analyses des Correspondances Multiples,
# notamment pour des personnes n'ayant pas l'habitude d'utiliser R.



### Fonctionnalité : ----

# - Imports des donées (CSV ou SAS avec les labels)
# - Créations de sous-populations et exports des tables
# - ACM :
#     - Visualisation rapide (graphiques et tables) : Repris d'Anton Perdoncin
#     - Passerelle sans codage vers la fonction explor() de Julien Barnier, qui le fait bien mieux.
# - Classification :
#     - Choix du nombre de catégories (Dendrogramme), distance de Ward uniquement.
#     - Description rapide de la variable "cluster" créee avec des variables au choix.
#     - Export des données avec la variables "cluster" en format CSV ou SAS



### Composition des scripts ----

# Le projet de Shiny doit obligatoirement contenir 3 scripts qui fonctionnent en
# interaction entre eux.

# - global.R : Explication de l'appli + Appel des packages + Choix de certaines options
# - ui.R     : Création de l'interface utilisateur
# - server.R : Calcules et création des graphiques. Pour plus de clarté, j'ai divisé le server dans 4 scripts :
#       - server           : pour le démarrage du Shiny
#       - ServeurTableau.R : pour la création de la sous-population voulue et l'affichage de la table
#       - ServeurACM.R     : pour l'ACM + représentation simple
#       - ServeurCAH.R     : pour la catégorisation + l'export de la base avec la variable.

# Ces 4 scripts auraient pu être regroupé dans un unique server.R

# De même les scripts global, ui et server aurait pu être regroupé dans un même script, appelé app.R
# Shiny permet différents style d'organisation du travail.



### Lancer l'application ----

# Si vous êtes dans un projet contenant les différents programmes mentionnés dans la partie
# précédente alors : CLIQUER SUR LE BOUTON "Run App" EN-HAUT A DROITE.
# Néanmoins cette solution ne permet pas de lancer explor()


# Sinon le script fonction.R, permet de créer la fonction VirageACM()
# Il suffit de lancer la fonction, pour faire apparaitre le Shiny dans toutes ses fonctions.




### Précautions ----

# Si au moment de cliquer sur le bouton Valider l'ACM, l'application s'interrompt, 
# c'est possiblement car une des variables ne contient qu'une unique modalité.



### Ajouts futurs ----

# - Gestion des ACP
# - Choix d'autres classifications que la CAH avec distance de ward
# - Embellir les graphiques de l'ACM avec ggplot : Fait
# - Enlever le noms des variables dans les étiquettes des modalités dans le graphique
# des ACM (pour le moment c'est nécessaire pour gérer les variables ayant les même noms
# de modalités) : Fait
# - Un onglet pour faire des graphiques simples avec une ou deux variables, pour explorer.



### Packages utilisées ----


# Shiny :
library(shiny)         # Pour Shiny
library(shinyWidgets)  # Ajout d'options notamment SelectPicker
library(shinythemes)   # Changer le theme du Shiny
library(shinyBS)       # Ajout Bootstrap pour Shiny
library(shinyjs)       # Ajout d'option à Shiny, notamment la création d'un Hub qui appelle différent Shiny
library(fresh)         # Personnaliser l'interface CSS
#library(spsComps)


# Import de données
library(openxlsx)     # Pour importer et écrire des données dans d'autres formats (Excel)
library(haven)         # Pour importer et écrire des données dans d'autres formats (SAS)
library(labelled)      # Pour transformer les label SAS en factor


# ACM :
library(FactoMineR)    # Pour faire des ACM et créer un objet pour explor()
library(factoextra)    # Embellir les graphiques de FactoMineR (peut-être pas utilisé dans la VF)
library(GDAtools)      # Outils pour ACM : faire des ACM en ggplot2
library(explor)        # Pour manipuler les ACM dans un Shiny interactif 
library(ade4)          # Autre package pour faire des ACM, on l'utilise pour faire des classifications
# Attention : si on indique pas le nombre de npc = Inf dans FactoMineR, alors les classifications seront différentes.
library(fastcluster)   # Accélère les clusters


# Manipulation de données :

library(tidyverse)     # Fonctions de manipulation de données
library(sortable)      # Pour réordonner les modalités
library(tidyr)         # NEW POUR TABLE PIVOT

# Tables
library(DT)            # Afficher des tables au format HTML

# Graphiques
library(ggplot2)       # Graphiques du tidyverse
library(colourpicker)  # Shiny : sélection manuelle des couleurs
library(RColorBrewer)  # Création de palette de couleurs
#library(plotly)        # Interactif
library(ggrepel)       # Eviter que les étiquettes des graphiques ne se superpose
library(corrplot)      # Graphique des sur/sous-représentations
library(ggiraph)
library(ggExtra)       # Permet plus d'options sur ggplot2

# Autres :
library(questionr)     # Plusieurs options utiles (pas forcément utilisé dans la VF)
# A AJOUTER :
# Sauvegarde :
library(rmarkdown)
library(knitr)
library(shinyFiles)
library(here)
library(flextable)
library(officer)

## Import SAS

source("fonction_import_sas_label.R")



### Options ----

# Problème d'accents
options(encoding = 'UTF-8')

# Pour importer des données plus importantes (ici 80Mo)
options(shiny.maxRequestSize = 80*1024^2)

# Pour enlever les messages de Dplyr
options(dplyr.summarise.inform = FALSE)


options(scipen=999)
# Pour supprimer la notation scientifique des tables notamment.


jscode <- "shinyjs.closeWindow = function() { window.close(); }"
# Pour permettre la création d'un "hub" de Shiny permettant de lancer successivement des Shiny différents
# Dans notre cas, on supprime le Hub en faisant une passerelle directement de l'application 1 (la mienne),
# vers la fonction explor() de Julien Barnier.



### Limite : ----



### Idées de développement ----

# - ACM : ind.sup, quanti.sup
# - Valeurs Manquante : algorithme missMDA
# - Bouton Chi² qui affiche la significativité, et une explication
# - Variable Quantitative : PCA




