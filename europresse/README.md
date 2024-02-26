# europresse()

Une fonction pour importer un document HTML issue d'Europresse dans R et le mettre en forme de data.frame pour son exploitation.

Variables récupérées :
- Journal  
- Titre  
- Date  
- Année  
- Auteur  
- Texte  
- Longueur  


# Mise en oeuvre
 
## Fonctionnement

On utilise la fonction `europresse()` qui ne fait partie d'aucun package, il faut donc la charger dans un premier temps dans R. 
Une fois la fonction chargée, il suffira de la lancer dans R en renseignant les variables, et les options voulues.


## Import de la fonction ` europresse ()`

Pour l'instant la fonction ne se trouve pas dans un package, il faut donc la charger dans l'environnement global de R depuis GitHub en utilisant le code suivant.  

```{r}

source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/europresse/Fonction_europresse.R")

```




## Paramètres de la fonction

html        : Le chemin vers un fichier HTML issue d'Europresse
name        : Le nom du data.frame en sortie
min_nchar   : Filtre sur le nombre de caractères dans les textes
suppr_doubl : "oui" ou "non", pour supprimer les doublons
seuil_doubl : Le seuil de "sévérité" pour la suppression





## Création du corpus 

```{r}
europresse(html = "D:/INED_Formation/Europresse/DATA/biblioeuropresse20240221115914.HTML",
           name = "dt_europresse",
           min_nchar = 500,
           suppr_doubl = "non",
           seuil_doubl = 50)
```


## Attention :

- Il est possible que les variables contiennent des NA si le balisage du HTML change.
- Voir la fonction format_iramuteq(), pour aller plus loin sans R.



