# spag_plot()

Une fonction pour simplifier la gestion de l'effet spaghettis (nombre trop important de courbes sur un même graphique).


## Pistes pour plus tard :

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

**donnees**        : Une base de données                             
**var_x**          : Une variable continue pour l'axe X    
**var_y**          : Une variable continue pour l'axe Y   
**var_group**      : Une variable catégorielle         
**ordre**          : Ordre des graphiques    
- Soit "alpha" : Ordre alphabétique selon var_group                                
- Soit une valeur numérique comprise dans var_x : Classe à la date de var_x  
- Soit un vecteur avec les noms des groupes dans l'ordre voulu : Ordre du vecteur  

**decroiss**       : "oui" ou "non", Ordre décroissant ou croissant de ordre    
**titre**          : Choix du titre du graphique     
**titre_x**        : Choix du titre de l'axe X    
**titre_y**        : Choix du titre de l'axe Y   
**source**         : Nom de la source    
**interval_x**     : Echelle de l'axe X   
**graduation_y**   : Montrer les graduations sur l'axe Y : TRUE/FALSE   
**n_col**          : Nombre de colonnes sur lesquelles apparaissent les graphiques    
**alignement_x**   : Position des étiquettes sur l'axe X (à documenter, voir n.dodge)   
- Une valeur supérieur ou égale à 1 : Nombre d'étiquettes à superposer avant de revenir sur l'axe.   

**col_line**       : Couleur de la ligne principale   
**lwd_line**       : Epaisseur de la ligne principale  
**transp_line**    : Transparence de la ligne principale  
**type_line**      : Type de ligne : solid, dashed, dotted, dotdash, longdash, twodash  
**col_line_bg**    : Couleur lignes secondaires  
**lwd_line_bg**    : Epaisseur lignes secondaires  
**transp_line_bg** : Transparence lignes secondaires  
**type_line_bg**   : Type lignes secondaires   




## Import de la fonction `table_auto()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  

```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/spag_plot/spag_plot.R")

```


## Création des graphiques


```{r filename="Activation de la fonction", warning=FALSE, message=FALSE}

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
          interval_x = 5,         # Echelle de X
          graduation_y = FALSE,   # Graduation sur l'axe Y : TRUE/FALSE 
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

```
