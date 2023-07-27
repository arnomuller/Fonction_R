# import_sas_label()

Une fonction pour importer facilement des données SAS (.sas7bdat) dans R, avec des labels issus d'un catalogue (.sas7bcat).



## Fonctionnement

On utilise la fonction `import_sas_label()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. 
Une fois la fonction chargée, il suffira de la lancer dans R en renseignant le chemin d'accès vers les données et celui vers le catalogue.

La fonction renvoie alors un data.frame qui applique les labels aux variables, quand cela est pertinent.

**ATTENTION !**
Il n'applique pas les labels aux variables quantitatives qui ont des labels par intervalles, pour éviter d'écraser la variable numérique.
Cependant la fonction `create_var_label()`  permet de régler ce problème. (voir : <https://github.com/arnomuller/Fonction_R/tree/main/SAStoR/create_var_label>).




## Paramètre de la fonction

**data_file**    : Chemin d'accès vers une base de données au format .sas7bdat.                          
**catalog_file** : Chemin d'accès vers un catalogue de labels au format .sas7bcat.      
**blanc_as_NA**  : TRUE ou FALSE, permet de transformer les blancs ("") en NA. 


## Import de la fonction `import_sas_label()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  

```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/SAStoR/import_sas_label/fonction_import_sas_label.R")

```


## Données d'exemples

L'exemple ci-joint est basé sur une extraction de la 1ère enquête ERFI


## Mise en oeuvre


```{r }

erfi <- import_sas_label(data_file = "erfi_extrait2.sas7bdat", 
                         catalog_file = "formats.sas7bcat",
						 blanc_as_NA = TRUE)
table(erfi$MB_STOC)
table(erfi$MA_AGER)

```




## Pour plus tard

- Potentiel problème quand un label est utilisé pour recoder une variable et qu'il regroupe 2 modalités sous la même étiquette.
	- Piste : changer la manière de détecter les labels qui s'appliquer sur plusieurs modalités (par intervalle), mais ouvre le problème de détection des 99 et des 88 etc.

