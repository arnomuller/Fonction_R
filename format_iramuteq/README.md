# format_iramuteq()

Une fonction pour transformer un data.frame avec une variable de texte, en un document **.txt** adapté à l'analyse textuelle sur Iramuteq.



## Données d'exemples

```{r}
# Définir les catégories du journal
categories_journal <- c("Journal A", "Journal B", "Journal C", "Journal D", "Journal E")

# Définir les années
annees <- sample(2010:2022, 100, replace = TRUE)

# Phrases d'exemple
phrases <- c(
  "Ceci est une phrase intéressante.",
  "Le temps est magnifique aujourd'hui.",
  "Le chat noir se promène dans le jardin.",
  "L'équipe locale a remporté le match hier soir.",
  "Les vacances approchent à grands pas.",
  "La science progresse rapidement.",
  "La musique adoucit les mœurs.",
  "Les montagnes offrent des paysages à couper le souffle.",
  "La cuisine française est réputée dans le monde entier.",
  "Le film que j'ai vu hier était vraiment captivant."
)

# Générer des phrases aléatoires pour chaque entrée
set.seed(123) # Pour reproduire les mêmes résultats
texte <- sample(phrases, 100, replace = TRUE)

# Créer un échantillon aléatoire des catégories de journal pour chaque entrée
journal <- sample(categories_journal, 100, replace = TRUE)

# Créer le data frame contenant les données
donnees <- data.frame(Journal = journal, Annee = annees, Texte = texte)
```


# Mise en oeuvre
 
## Fonctionnement

On utilise la fonction `format_iramuteq()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. 
Une fois la fonction chargée, il suffira de la lancer dans R en renseignant les variables, et les options voulues.


## Import de la fonction `table_auto()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  

```{r filename="Import de la fonction depuis Github"}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/format_iramuteq/format_iramuteq.R")

```




## Paramètres de la fonction

**articles**         : Une base de données avec une variable texte et des métadonnées (années, source, etc.)                    
**nom_fichier**      : Le nom du fichier .txt en sortie    
**var_texte**        : Le nom de la variable de texte (entre "")  
**vars_métadonnees** : Un vecteur avec les variables de métadonnées           



## Création du corpus 

```{r}

vars_metadonnees <- c("CJournal", "Annee", "Journal_2")
format_iramuteq(dataframe = donnees, 
                nom_fichier = "corpus_iramuteq.txt", 
				var_texte = "Texte", 
				vars_metadonnees = c("Journal", "Annee"))

```


## Attention :


Le format utilisé dans Iramuteq se présentent sous le format : 

\*\*\*\* \*NomVar1_Moda1 \*NomVar2_Moda1 

**Texte 1**

\*\*\*\* \*NomVar1_Moda2 \*NomVar2_Moda1 

**Texte 2**

Chaque texte est séparé du précédent par une ligne étoilé qui contient les métadonnées, qui sont elles même séparées par une étoile.
Iramuteq impose donc quelques limitations dans les noms de variables (métadonnées) et les valeurs qu'elles peuvent prendre, ainsi que pour certains symboles dans les textes.



1) Il faut supprimer les espaces et les "_" dans les noms des variables métadonnées   
2) Il faut supprimer les espaces et les "_" dans les valeurs des variables métadonnées  
3) Il faut supprimer les \* présentent dans les textes.

Chacun de ces 3 points sont pris en compte dans la fonction `format_iramuteq()` mais informera l'utilisateur.rice des modifications par un **warning**.



