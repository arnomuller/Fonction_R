# Introduction

Il existe de nombreux packages R consacrés à la gestion et la mise en forme de tables et de tris croisés (`GT`, `flextable`, ...). S'ils sont souvent de bonne qualité, leurs coûts d'entrée peut être un peu élevé pour les utilisateurs.rices occasionnelles de R.

table_auto() se veut une fonction facile d'utilisation permettant de compiler des tris uni- ou bivariées sur un grand nombre de variables, d'utiliser une pondération et les exporter dans un document Excel.

Pour une introduction aux tables et à la pondération, voir la [fiche](https://mthevenin.github.io/assistoolsms/R/assist/posts/weight_norm/weight_norm.html)

## Données d'exemples

Pour illustrer la mise en oeuvre de la fonction, on utilise les données d'exemples *hdv2003* du package `questionr`. Ces données possèdent à la fois des variables sur des caractéristiques socio-démographiques, des pratiques et des goûts, et propose une variable de pondération : *poids*.

```{r filename="Import des données d'exemples", warning=FALSE, message=FALSE}

library(questionr)
data("hdv2003")

```


## Objectifs :

-   Obtenir une table automatique avec les tris à plat d'un grand nombre de variables  
-   Croiser ces variables avec une variable choisie, par exemple le sexe ou la qualification et obtenir :  
    -   Les effectifs  
    -   Les pourcentages lignes  
    -   Les pourcentages colonnes  
	-	Quelques indicateurs pour les variables numériques (moyennes, quantiles, etc.)  
-	Obtenir une table de la combinaison des réponses possibles aux variables présentes  
-   Utiliser des pondérations (et les normaliser si besoin)  
-   Garder ou non les valeurs manquantes  
-   Exclure des modalités choisies  
-	Obtenir les résultats d'un test (chi² ou fisher)  
-	Conserver les labels pour les données issues de SAS  
-	Afficher la table en HTML  
-   Exporter le tableau obtenu au format .xlsx  


# Mise en oeuvre
 
## Fonctionnement

On utilise la fonction `table_auto()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. 
Une fois la fonction chargée, il suffit de la lancer en renseignant les variables, et les options voulues (présence de valeurs manquantes, pondérations, exportation)

## Paramètre de la fonction

**data**          : Une base de données                            
**vars**          : Un vecteur avec des noms de variables    
**vars_num**      : Un vecteur avec des noms de variables numériques         
**var_col**       :    
- Si NULL (vide)  : Tris à plat                                    
- Si une variable : Tris croisés

**table_type**     :  
-   Effectifs        		    : "eff"      
-   Pourcentage ligne  		    : "row"  
-   Pourcentage colonne 		: "col"  
-	Tout                        : "all"
-	La combinaison des réponses : "mix"  
                  
**var_weight**     : Le nom d'une variable de pondération dans data      
**weight_norm**	   : TRUE ou FALSE, normaliser la pondération     
**useNA**          : TRUE ou FALSE, garder ou non les valeurs manquantes    
**exclude**        : Un vecteur avec les noms des modalités à exclure des tables    
**use_test**       :   
-	Pas de test 	 : "no"  
-	Chi²  	  	     : "chi2"  
-	Chi² non pondéré : "chi2_noponder"  
-	Fisher 			 : "fisher"  

**arrondi**        : Nombre de chiffres après la virgule      
**use_labels**     : Utiliser les labels :    
- "no"    : n'affiche pas les labels   
- "yes"   : utilise les labels si présents   
- "both"  : concatène la valeur numérique et le label         
              
**add_blank_rows** : TRUE ou FALSE, insérer une ligne vide entre chaque variable     
**eff_in_name**    : Ajouter les effectifs dans les noms des modalités   
- "no"        : n'affiche pas les effectifs   
- "yes"       : affiche les effectifs   
- "noponder"  : affiche les effectifs non-pondérés     

**excel_export**   : TRUE ou FALSE, création d'un fichier excel.        
**excel_filepath** : Chemin et nom du fichier excel (défaut : "table_auto.xlsx")    
**view_html**      : TRUE ou FALSE, affiche la table choisie dans table_type en HTML       



## Import de la fonction `table_auto()`

Pour l'instant, la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  


```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/table_auto/fonction_table_auto.R")

```


## Choix des variables

On définit les variables à croiser :

```{r filename="Choix des variables"}

mes_vars <- c("relig","trav.imp","trav.satisf","hard.rock",
               "lecture.bd","peche.chasse","cuisine",
               "bricol","cinema","sport")

```

Ainsi que les variables numériques :  

```{r filename="Choix des variables"}

vars_num  <- c("age","heures.tv")

```


## Suppression de certaines modalités

On peut définir des modalités qui ne doivent pas être utilisées (le total changera donc d'une variable à l'autre).  

```{r filename="Choix des modalités à exclure"}
# Exemple avec une modalité
junk = c("NSP ou NVPR") 
# Exemple avec trois modalités
junk = c("NSP ou NVPR", "Cadre", "Rejet")
```


## Création du tableau empilé

En lançant le code suivant, on crée les data.frames **table_auto_(table_type)**.

```{r filename="Activation de la fonction", warning=FALSE, message=FALSE}

table_auto(hdv2003,                     # Base de données
           vars           = mes_vars,   # Un vecteur avec les noms des variables d'intérêts
           vars_num       = vars_num,   # Un vecteur avec les noms des variables d'intérêts numériques
           var_col        = "qualif",   # Variable à croiser avec celles du vecteur
           table_type     = "all",      # Type de table : "all", "eff", "row", "col", ou "mix"
           var_weight     = "poids",    # Variable de pondération, sinon = NULL
           weight_norm    = FALSE,      # TRUE/FALSE : Normaliser la pondération
           useNA          = FALSE,      # TRUE/FALSE : Ajout des valeurs manquantes
           exclude        = junk,       # exclure des modalités
           use_test       = "chi2",     # Type de test : "chi2", "fisher", "chi2_noponder", "no"
           arrondi        = 2,          # Nombre de chiffres après la virgule
           use_labels     = "no",       # Utiliser les labels : "no", "yes", "both"
           add_blank_rows = TRUE,       # TRUE/FALSE : Ajout d'une ligne vide entre les variables
           eff_in_name    = "noponder", # Ajout des effectifs dans les noms des modalités : "yes","noponder", "no"
           excel_export   = FALSE,      # TRUE/FALSE : Création d'un fichier excel puis son chemin
           excel_filepath = "./table_auto.xlsx", # Seulement si excel_export = TRUE
           view_html      = TRUE)       # TRUE/FALSE : Afficher la table en HTML

```
