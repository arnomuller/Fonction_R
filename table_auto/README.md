# Introduction

Il existe un large éventail de package R consacré à la gestion et la mise en forme de table et de tris croisés (`GT`, `flextable`, ...). S'ils sont souvent de bonne qualités, leur coût d'entrée peut être un peu élevé pour les utilisateurs.rices occasionnelles de R.

table_auto() se veut une fonction facile d'utilisation permettant de compiler des tris uni- ou bivariées sur un grand nombre de variables, d'utiliser une pondération et les exporter dans un document Excel.

Pour une introduction aux tables et à la pondération, voir la [fiche](https://mthevenin.github.io/assistoolsms/R/assist/posts/weight_norm/weight_norm.html)

## Données d'exemples

Pour illustrer la mise en oeuvre de la fonction, on utilise les données d'exemples *hdv2003* du packages `questionr`. Ces données possèdent à la fois des variables sur des carastéristiques socio-démographiques, des pratiques et des goûts, et propose une variable de pondération : *poids*.

```{r filename="Import des données d'exemples", warning=FALSE, message=FALSE}

library(questionr)
data("hdv2003")

```


## Objectifs :

-   Obtenir une table automatique avec les tris à plat d'un grand nombre de variables
-   Croiser ces variables avec une variable choisie, par exemple le sexe ou la qualification et obtenir :
    -   Les effectifs
    -   Les pourcentages lignes
    -   Les poucentages colonnes
    -   Ajout d'un test du chi²
-   Utiliser des pondérations
-   Garder ou non les valeurs manquantes
-   Exporter le tableau obtenu  


# Mise en oeuvre
 
## Fonctionnement

On utilise la fonction `table_auto()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. Une fois la fonction chargée, il suffira de la lancer en renseignant les variables, et les options voulues (présence de valeurs manquantes, pondérations, exportation)

## Paramètre de la fonction

**data**          : Une base de données                            
**vars**          : Un vecteur avec des noms de variables         
**var_col**       :    
- Si NULL (vide)  : Tris à plat                                    
- Si une variable : Tris croisés

**table_type**     :  
-   Effectifs           : "eff"      
-   Pourcentage ligne   : "row"  
-   Pourcentage colonne : "col"  
-	Tout                : "all"
                  
**var_weight**     : Le nom d'une variable de pondération dans data                     
**useNA**          : TRUE ou FALSE, garder ou non les valeurs manquantes      
**chi2.test**      : TRUE ou FALSE, ajouter une p.value du test du chi²        
**arrondi**        : Nombre de chiffre après la virgule                           
**add_blank_rows** : TRUE ou FALSE, insérer une ligne vide entre chaque variable     
**eff_in_name**    : TRUE ou FALSE, ajouter les effectifs dans les noms des modalités   
**excel_export**   : TRUE ou FALSE, création d'un fichier excel.        
**excel_filepath** : Chemin et nom du fichier excel (défaut : "table_auto.xlsx")        

```{r}
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
```


## Import de la fonction `table_auto()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  


```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/table_auto/fonction_table_auto.R")

```


## Choix des variables

On définit les variables à croiser :

```{r filename="Choix des variables"}

vars      <- c("relig","trav.imp","trav.satisf","hard.rock",
               "lecture.bd","peche.chasse","cuisine",
               "bricol","cinema","sport")

```



## Création du tableau empilé

En lançant le code suivant, on crée les data.frames **table_auto_(table_type)**.

```{r filename="Activation de la fonction", warning=FALSE, message=FALSE}

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

```
