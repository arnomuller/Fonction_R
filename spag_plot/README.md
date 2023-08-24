# spag_plot()

Une fonction pour simplifier la gestion de l'effet spaghettis (nombre trop important de courbes sur un même graphique).


## Pistes pour plus tard :

- options pour les couleurs
- options pour choisir d'afficher un ou plusieurs groupes au choix au lieu de tous.


## Données d'exemples


```{r filename="Import des données d'exemples", warning=FALSE, message=FALSE}

pays <- c("France", "Allemagne", "Espagne", "Italie")

# Créer des données aléatoires pour chaque pays
donnees <- data.frame(
  Année = rep(2000:2020, length(pays)),
  Pays = rep(pays, each = 21),
  Indicateur = runif(length(pays) * 21, min = 0, max = 100)
)
```




# Mise en oeuvre
 
## Fonctionnement

On utilise la fonction `table_auto()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. 
Une fois la fonction chargée, il suffira de la lancer dans R en renseignant les variables, et les options voulues.

## Paramètre de la fonction

**donnees**     : Une base de données                             
**var_x**       : Une variable continue pour l'axe X    
**var_y**       : Une variable continue pour l'axe Y   
**var_group**   : Une variable catégorielle         
**ordre**       : Ordre des graphiques    
- Soit "alpha" : Ordre alphabétique selon var_group                                
- Soit une valeur numérique comprise dans var_x : Classe à la date de var_x
- Soit un vecteur avec les noms des groupes dans l'ordre voulu : Ordre du vecteur

**decroiss**    : "oui" ou "non", Ordre décroissant ou croissant de ordre   
**titre**       : Choix du titre du graphique    
**titre_x**     : Choix du titre de l'axe X    
**titre_y**     : Choix du titre de l'axe Y   
**source**      : Nom de la source   
**interval**    : Echelle de l'axe X  
**n_col**       : Nombre de colonnes sur lesquelles apparaissent les graphiques   
**alignement_x**: Position des étiquettes sur l'axe X (à documenter, voir n.dodge) 



## Import de la fonction `table_auto()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  

```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/spag_plot/spag_plot.R")

```


## Création des graphiques


```{r filename="Activation de la fonction", warning=FALSE, message=FALSE}

spag_plot(donnees,             # Base de données au format long
          var_x = "Année",     # Variable en X (numérique)
          var_y = "Indicateur",# Variable en Y (numérique)
          var_group= "Pays",   # Variable de groupe (factor)
          ordre =  2020,       # Ordre des plots ("alpha", valeur num de var_x, un vecteur)
          decroiss = "oui",    # Ordre décroissant de l'ordre choisi au dessus
          titre   = "TITRE",   # Choix du titre
          titre_x = "Années",  # Choix du titre de l'axe X
          titre_y   ="Taux",   # Choix du titre de l'axe Y
          source    = "",      # Source des données
          interval = 20,       # Echelle de X
          n_col = 2,           # Nombre de colonnes pour les graphiques
		  alignement_x = 1)    # Position sur l'axe X
```
