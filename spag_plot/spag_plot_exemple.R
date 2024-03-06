pays <- c("France", "Allemagne", "Espagne", "Italie")

# Créer des données aléatoires pour chaque pays
donnees <- data.frame(
  Année = rep(2000:2020, length(pays)),
  Pays = rep(pays, each = 21),
  Indicateur = runif(length(pays) * 21, min = 0, max = 100)
)

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/spag_plot/spag_plot.R")

spag_plot(donnees,                # Base de données au format long
          var_x = "Année",        # Variable en X (numérique)
          var_y = "Indicateur",   # Variable en Y (numérique)
          var_group= "Pays",      # Variable de groupe (factor)
          ordre =  "alpha",       # Ordre des plots ("alpha", valeur num de var_x, un vecteur)
          decroiss = "oui",       # Ordre décroissant de l'ordre choisi au dessus
          titre   = "TITRE",      # Choix du titre
          titre_x = "Années",     # Choix du titre de l'axe X
          titre_y   ="Taux",      # Choix du titre de l'axe Y
          source    = "",         # Source des données
          interval = 5,           # Echelle de X
          n_col = 2,              # Nombre de colonnes pour les graphiques
          alignement_x = 1,       # Nombre d'étiquettes à superposer avant de revenir sur l'axe.
          col_line = "#C24168",   # Couleur de la ligne principale
          lwd_line = 1.4,         # Epaisseur de la ligne principale
          transp_line = 1,        # Transparence de la ligne principale
          type_line = "solid",    # Type de ligne : solid, dashed, dotdash, longdash, twodash
          col_line_bg = "grey",   # Couleur lignes secondaires
          lwd_line_bg = 0.5,      # Epaisseur lignes secondaires
          transp_line_bg = 0.8,   # Transparence lignes secondaires
          type_line_bg = "dashed")# Type lignes secondaires   





