pays <- c("France", "Allemagne", "Espagne", "Italie")

# Créer des données aléatoires pour chaque pays
donnees <- data.frame(
  Année = rep(2000:2020, length(pays)),
  Pays = rep(pays, each = 21),
  Indicateur = runif(length(pays) * 21, min = 0, max = 100)
)

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/spag_plot/spag_plot.R")

spag_plot(donnees,             # Base de données au format long
          var_x = "Année",     # Variable en X (numérique)
          var_y = "Indicateur",# Variable en Y (numérique)
          var_group= "Pays",   # Variable de groupe (factor)
          ordre =  "alpha",    # Ordre des plots ("alpha", valeur num de var_x, un vecteur)
          decroiss = "oui",    # Ordre décroissant de l'ordre choisi au dessus
          titre   = "TITRE",   # Choix du titre
          titre_x = "Années",  # Choix du titre de l'axe X
          titre_y   ="Taux",   # Choix du titre de l'axe Y
          source    = "",      # Source des données
          interval = 1,        # Echelle de X
          n_col = 2,           # Nombre de colonnes pour les graphiques
          alignement_x = 5 )   # Nombre d'étiquettes à superposer avant de revenir sur l'axe.






