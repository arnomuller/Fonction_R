
## DONNEES EXEMPLES : 

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


## IMPORT FONCTION
source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/format_iramuteq/format_iramuteq.R")
       

## UTILISATION FONCTION

format_iramuteq(dataframe = donnees, 
                nom_fichier = "corpus_iramuteq.txt", 
                var_texte = "Texte", 
                vars_metadonnees = c("Journal", "Annee"))




