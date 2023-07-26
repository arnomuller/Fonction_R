# create_var_label()

Une fonction pour créer une nouvelle variable catégorielle à partir d'un label appliqué à des intervalles de valeurs quantitatives.



## Fonctionnement

On utilise la fonction `create_var_label()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. 
Une fois la fonction chargée, il suffira de la lancer dans R en renseignant le jeu de donnée et la variable voulue.

Sur SAS, certains labels peuvent s'appliquer à des intervalles de valeurs pour les variables numériques.
Ce fonctionnement n'existe pas sur R, chaque label doit avoir son propre nom.

Pour résoudre ce problème, je crée une nouvelle variable qui prend le nom du label SAS, et qui découpe la variable continue aux mêmes intervalles.

**ATTENTION !**
Si les NSP sont recodés en 99 ou 999, ils seront ajoutés à la dernière catégorie.



## Paramètre de la fonction

**donnees**    : Une base de données                             
**varlabel**   : Une variable continue avec un label qui s'applique sur des intervalles   


## Import de la fonction `create_var_label()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  

```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/SAStoR/create_var_label/fonction_create_var_label.R")

```


## Données d'exemples

L'exemple ci-joint est basé sur une extraction de la 1ère enquête ERFI


## Mise en oeuvre


```{r }

library(haven)
library(dplyr)

erfi <- read_sas("erfi_extrait2.sas7bdat", catalog_file = "formats.sas7bcat")
str(erfi$MA_AGER)

erfi <- create_var_label(erfi, "MA_AGER")
```




## Pour plus tard

- Gestion des 88, 99, etc.
- Ne pas simplement faire une copie du jeu de donnée mais appliqué directement la fonction à une variable 

Exemple de développement :

```{r }

erfi$newvar <- create_var_label(erfi, "MA_AGER")

```

