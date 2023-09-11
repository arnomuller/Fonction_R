#### Server ACM

# Dans ce script, je continue la partie server et je crée les objets nécessaires à l'ACM
# Je présente quelques outils pour explorer l'ACM, repris d'Anton Perdoncin : https://quanti.hypotheses.org/1871


### TEXTE INTRODUCTION ----


n_row_acm <- reactive({ 
  nrow(filter_data())
  })

n_row_acm_rapport <- reactive({ 
  nrow(data_acm)
})


output$recap_activ <- renderPrint({
  validate(need(filter_data(), ""))
  cat("La taille de la population sélectionnée est de :", isolate(n_row_acm()),"individus. \n")
})




### SELECTION DES VARIABLES ACTIVES ET SUPPLEMENTAIRES ----


# Selection des variables actives
output$acm_main_act <- renderUI({
  
  validate(need(input$target_upload, 'Importer des données'))
  
  tagList(
    fluidRow(
      column(4, selectizeInput('var_act',
                               label=paste0("Choix des variables actives"),
                               # Choix parmis les noms de variables de data
                               choices=c("",nomcol_data_reac()),
                               # Plusieurs options : 
                               options = list(`actions-box` = TRUE, placeholder = 'Pas de variables actives'), 
                               multiple = TRUE, # Si TRUE alors on peut choisir plusieurs variables.
                               width = 450)),
      column(1, actionButton("fill_act", "Bis", style = 'margin-top:23px')),
      
      #Sélection de pondération
      column(4, offset = 1,
             selectizeInput('var_ponder',
                               label=paste0("Choix de la variable de pondération"),
                               # Choix parmis les noms de variables de data
                               choices=c("",nomcol_data_reac()),
                               # Plusieurs options : 
                               options = list(`actions-box` = TRUE, placeholder = 'Pas de pondération'), 
                               multiple = FALSE, 
                               width = 450)),
      column(1, actionButton("fill_ponder", "Bis", style = 'margin-top:23px'))
      )
  )})



# Selection des variables supplémentaires

# La sélection ne se fait que quand des variables ont ête prises en actives
# On enlève les variables choisies en active des choix possibles en supplémentaire.

observeEvent(input$var_act,{
  if(length(input$var_act) ==""){ 
    
    output$acm_main_sup <- renderUI({  
      data()
      tagList(
        fluidRow(
          column(4,
                 selectizeInput('var_sup',
                                label=paste0("Choix des variables supplémentaires"),
                                choices=c("",nomcol_data_reac()),
                                options = list(`actions-box` = TRUE, placeholder = 'Pas de variables supplémentaires'),
                                multiple = TRUE,
                                width = 450)),
          column(1, actionButton("fill_sup", "Bis", style = 'margin-top:23px'))
          ))})
    
  }else{
    # Si des variables ont été choisies en actives :
    output$acm_main_sup <- renderUI({  
      data()
      tagList(
        fluidRow(
          column(4,
                 selectizeInput('var_sup',
                                label=paste0("Choix des variables supplémentaires"),
                                choices = c("",nomcol_data_reac()[!(nomcol_data_reac() %in% input$var_act)]), # on enlève les actives
                                options = list(`actions-box` = TRUE, placeholder = 'Pas de variables supplémentaires'),
                                multiple = TRUE,
                                width = 450)),
          column(1, actionButton("fill_sup", "Bis", style = 'margin-top:23px'))
          ))})
  }
})




#### BOUTON RECUPERER LES VARIABLES SELECTIONNEES

previousSelections <- reactiveValues(var_act = character(0), var_sup = character(0), var_ponder = character(0))


observeEvent(input$fill_act, {
  validate(need(input$target_upload, ''))
  validate(need(input$acmOK, ''))
  
  updateSelectizeInput(session, "var_act", selected = previousSelections$var_act)
})

observeEvent(input$fill_sup, {
  validate(need(input$target_upload, ''))
  validate(need(input$acmOK, ''))
  
  updateSelectizeInput(session, "var_sup", selected = previousSelections$var_sup)
})


observeEvent(input$fill_ponder, {
  validate(need(input$target_upload, ''))
  validate(need(input$acmOK, ''))
  
  updateSelectizeInput(session, "var_ponder", selected = previousSelections$var_ponder)
})



observeEvent(input$var_act, {
  validate(need(input$target_upload, ''))
  previousSelections$var_act <- input$var_act
})

observeEvent(input$var_sup, {
  validate(need(input$target_upload, ''))
  previousSelections$var_sup <- input$var_sup
})

observeEvent(input$var_ponder, {
  validate(need(input$target_upload, ''))
  previousSelections$var_ponder <- input$var_ponder
})





########### CLIQUER SUR LE BOUTON ACM ----


observeEvent(input$acmOK, {
  
  
  showModal(modalDialog("Le chargement de l'ACM peut prendre quelques secondes", footer=NULL))
  #showNotification("Le chargement de l'ACM peut prendre quelques secondes", duration = 15, type = "warning")
  
  n_acmOK <<- input$acmOK[1]
  indic_cah <<- "Non"
  
  # if(exists("ncluster") == TRUE){
  #   rm("ncluster", "arbre", "inertie") 
  # }
  
  ncluster <<- NULL
  arbre <<- NULL
  inertie <<- NULL
  meth_dist <<- NULL
  
  
  ### CREATION DE LA TABLE SOUS-POP

  if(length(input$var_souspop) >= 1){
    # Pour le rapport automatisé :
    # Je garde une base temporaire avec les variables sur lesquelles on a fait une selection
    tempo <<- filter_data() %>%
      select(all_of(input$var_souspop))

    # Je créer une table avec les variables et les modalités de sélection de population.
    tableau_modalite <<- data.frame("Variable" = input$var_souspop) %>%
      mutate(Modalite = NA)

    for (i in c(1:ncol(tempo))) {
      tableau_modalite[i,]$Modalite <<- paste(names(table(tempo[,i])), collapse = ", ")
    }
  } else {
    
    tableau_modalite <<- data.frame("Variable" = "Pas de sélection", "Modalite" = "Pas de sélection" )
    
  }
  
  ### CREATION DE L'OBJET FACTOMINER ----
  
  # On ne continue que si on a des variables actives
  validate(need(input$var_act,'')) 
  
  # Copie des variables utilisées (on pourra enlever à l'avenir)
  var_sup <<- input$var_sup
  var_act <<- input$var_act
  var_ponder <<- input$var_ponder
  
  # On ne garde que les variables choisies en actives et supplementaires 
  
  
  
  ####################
  if("cluster" %in% nomreac$nomcol) {
    
    # cluster_data <- filter_data() %>%
    #   select(-cluster)
    
    data_init2 <<- v$data %>%
      select(-"cluster")
    
  } else {
    # cluster_data <<- filter_data()
    data_init2 <<-  v$data
  }
  ####################
  
  data_acm <<- filter_data() %>% 
    select(autoacm_id,all_of(input$var_sup), all_of(input$var_act))
  
  # On créer un vecteur avec les poids quand il y en a :
  if(var_ponder != ""){
    ponder <<- as.numeric(as.character(as_vector(filter_data() %>% select(all_of(input$var_ponder)))))
  }
  
  
  
  # On fait l'ACM :
  
  # Si on a des variables supplementaires :
  if(is.null(var_sup) == FALSE){
    
    # Si on a une pondération
    if(var_ponder != ""){
      # Comme on a placé les var sup en 1er, on a juste à indiquer leurs numéros dans quali.sup
      res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE , ncp = Inf, 
                      quali.sup = c(1:length(input$var_sup)),
                      row.w = ponder)
    } 
    # Sans pondération
    else{
      res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE , ncp = Inf, 
                      quali.sup = c(1:length(input$var_sup)))
    }
  } 
  
  # Sans variables supplementaires
  else {
    # Si on a une pondération
    if(var_ponder != ""){
      res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE, ncp = Inf,
                      row.w = ponder)
    } 
    # Sans pondération
    else{
      res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE, ncp = Inf)
    }
  }
  ############################  
  
  
  
  
  
  ############################  
  ### CREATIONS D'OBJETS POUR TABLES ET GRAPHIQUES ----
  
  # Pour les sauts d'inertie : ----
  variances <<- as.data.frame(round(res_mca$eig,2)) %>%
    #rownames_to_column() %>%                                  # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
    mutate(rowname = rownames(.)) %>% 
    slice(1:10) %>%                                           # conserver que les infos des 10 premiers axes
    mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
    select(-rowname) %>%                                      # on enlève cette colonne dont on n'a plus besoin
    rename(`Valeurs propres` = eigenvalue) %>%
    rename(`% de variance` = `percentage of variance`) %>%    # on renomme les autres colonnes
    rename(`% cumulé de variance` = `cumulative percentage of variance`) %>% 
    mutate(Axes = fct_relevel(Axes, paste("Axe", 1:10)))      # pour que l'ordre de 1 à 10 soit bien respecté dans les graphiques
  
  
  # Pour les différents indicateurs de l'ACM : ----
  frequences <- gather(data_acm[,2:ncol(data_acm)], variables, modalites) %>%       # étendre le jeu de données par variable et modalité
    # compter le nombre de couples "variable/modalité" unique (donc le nombre d'individus par modalité du jeu de données)
    count(variables, modalites) %>%                              
    group_by(variables) %>% 
    mutate(pourcentage = round(100 * n / nrow(data_acm), 1)) %>% # calculer des pourcentages pour chaque groupe de variable
    ungroup() %>% 
    select(variables, modalites, n, pourcentage) %>% 
    rename(moda = modalites) %>% 
    # On ajoute le nom de la variable à l'avant pour les cas de variables avec les mêmes noms de modalités
    mutate(modalites = ifelse(is.na(moda)== F, paste0(variables,"_",moda), paste0(variables,".",moda )))
  
  ###############
  # Modalités actives ----
  
  # Coordonnées (modalités actives)
  
  coordonnees <- as.data.frame(round(res_mca$var$coord, 2)) %>% # récupérer les coordonnées des modalités actives et arrondir à deux décimales (c'est bien suffisant)
    rename_all(tolower) %>%                                     # tout en minuscules
    rename_all(~ str_replace(., " ", "")) %>%                   # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1" 
    rename_all(~ str_c(., "coord", sep = "_")) %>%              # ajouter le suffixe _coord à chaque nom de variable. On obtient ainsi par exemple "dim1_coord"
    mutate(modalites = rownames(.))                             # récupérer les noms des modalités, stockées dans le nom des lignes de res_mca$var$coord
  
  # Contributions (modalités actives) 
  contributions <- as.data.frame(round(res_mca$var$contrib, 2))  %>% 
    rename_all(tolower) %>% 
    rename_all(~ str_replace(., " ", "")) %>% 
    rename_all(~ str_c(., "contrib", sep = "_")) %>%            # idem sauf qu'ici on obtient "dim1_contrib"
    mutate(modalites = rownames(.))
  
  # Cosinus carrés (modalités actives) 
  cos2 <- as.data.frame(round(res_mca$var$cos2, 2)) %>% 
    rename_all(tolower) %>%
    rename_all(~ str_replace(., " ", "")) %>% 
    rename_all(~ str_c(., "cos2", sep = "_")) %>% # idem avec "cos2" 
    mutate(modalites = rownames(.))
  
  # vtest (modalités actives) 
  vtest <- as.data.frame(round(res_mca$var$v.test, 2)) %>% 
    rename_all(tolower) %>%
    rename_all(~ str_replace(., " ", "")) %>% 
    rename_all(~ str_c(., "vtest", sep = "_")) %>% # idem avec vtest
    mutate(modalites = rownames(.))
  
  
  # Compilation des indicateurs
  
  resultats_act <- frequences %>% 
    right_join(coordonnees) %>% 
    right_join(contributions) %>% 
    right_join(cos2) %>% 
    right_join(vtest) %>%                        # fusionner les jeux de données ; la clé de fusion (implicite) est la variable "modalites", qui est commune à tous. 
    mutate(type = "Variable active") %>%         # ajout d'une colonne contenant la chaîne de caractères "Variable active" (pour pouvoir distinguer plus tard avec les variables supplémentaires)
    select(type, variables, modalites, n, pourcentage,
           contains("dim1_"), contains("dim2_"),
           contains("dim3_"), contains("dim4_")) # conserver et réorganiser les variables pertinentes axe par axe, on garde que 4 axes
  
  # Problème des variables avec les mêmes modalités
  meme_moda <- resultats_act %>% 
    filter(is.na(variables) == T) %>% 
    select(- c(variables, n, pourcentage)) %>% 
    left_join(frequences, by = c("modalites"  = "moda")) %>% 
    mutate(modalites = paste0(variables,"_",modalites)) %>% # On ajoute le nom de la variable devant la modalité pour éviter les noms doublons
    select( -modalites.y)
  
  # On enlève les modalités en doubles et on ajoute leurs corrections
  resultats_actives <<- resultats_act %>% 
    filter(is.na(variables) == F) %>% 
    rbind(meme_moda)
  
  
  ################
  # Modalités supplémentaires ----
  
  # Si on a des variables supplementaires :
  if(is.null(var_sup) == FALSE){
    
    # Coordonnées (modalités supplémentaires) 
    coordonnees_sup <- as.data.frame(round(res_mca$quali.sup$coord, 2)) %>% # la démarche est la même que supra, mais avec le sous-objet quali.sup qui stocke les informations sur les variables qualitatives supplémentaires
      rename_all(tolower) %>%
      rename_all(~ str_replace(., " ", "")) %>% 
      rename_all(~ str_c(., "coord", sep = "_")) %>% 
      mutate(modalites = rownames(.))
    
    # Cosinus carrés (modalités supplémentaires) 
    cos2_sup <- as.data.frame(round(res_mca$quali.sup$cos2, 2)) %>% 
      rename_all(tolower) %>%
      rename_all(~ str_replace(., " ", "")) %>% 
      rename_all(~ str_c(., "cos2", sep = "_")) %>% 
      mutate(modalites = rownames(.))
    
    # vtest (modalités supplémentaires) 
    vtest_sup <- as.data.frame(round(res_mca$quali.sup$v.test, 2)) %>% 
      rename_all(tolower) %>%
      rename_all(~ str_replace(., " ", "")) %>% 
      rename_all(~ str_c(., "vtest", sep = "_")) %>% 
      mutate(modalites = rownames(.))
    
    # Assemblage du tableau des résultats (modalités actives) 
    
    resultats_sup <- frequences %>% 
      right_join(coordonnees_sup) %>% 
      right_join(cos2_sup) %>% 
      right_join(vtest_sup) %>% 
      mutate(type = "Variable supplementaire") %>% # comme supra pour le tableau des résultats des modalités actives : on distingue ici le type de variable.
      select(type, variables, modalites, n, pourcentage,
             contains("dim1_"), contains("dim2_"),
             contains("dim3_"), contains("dim4_")) 
    
    # Même problème de doublons
    meme_moda_sup <- resultats_sup %>% 
      filter(is.na(variables) == T) %>% 
      select(- c(variables, n, pourcentage)) %>% 
      left_join(frequences, by = c("modalites"  = "moda")) %>% 
      mutate(modalites = paste0(variables,"_",modalites)) %>% 
      select( -modalites.y)
    
    resultats_suplem <<- resultats_sup %>% 
      filter(is.na(variables) == F) %>% 
      rbind(meme_moda_sup)
    
  } # Fin Var Supp
  
  
  ###############
  # TABLEAU COMPLET  ----
  
  # Si on a des var supplémentaires
  if(is.null(var_sup) == FALSE){
    resultats_complet <<- bind_rows(resultats_actives, resultats_suplem)
  } 
  
  # Si on a que des variables actives
  if(is.null(var_sup) == TRUE){
    resultats_complet <<- resultats_actives
  }
  
  ############################ 
  
  
  
  
  ############################   
  ### FAIRE LES GRAPHIQUES ET LES TABLES ----
  
  # Table de saut d'inertie
  output$variances <- renderDataTable({
    datatable(variances %>% select(Axes, everything()))
  })
  
  # Graphique de saut d'inertie
  output$variances_graph <- renderPlot({
    ggplot(variances, aes(x = Axes)) +     # initialiser du graphique et de l'axe horizontal
      geom_bar(aes(y = `% de variance`),   # indiquer le type de graphique (barplot) et la variable à représenter sur l'axe vertical
               stat = "identity",
               fill = "lightgrey") +       # parce que j'aime bien le rouge
      xlab("") +                           # on enlève le label de l'axe des x, pas très utile
      ylab("% de variance") +              # renommer proprement le label de l'axe des y
      theme_minimal()                     
    
  })
  
  
  # Table de saut d'inertie, corrigé selon le critère de Benzécri
  variances_modif <<- round(modif.rate(res_mca)$modif,2)
  
  output$variances_Benz <- renderDataTable({
    datatable(variances_modif)
  })
  
  
  # Graphique de saut d'inertie, corrigé selon le critère de Benzécri
  output$variances_Benz_graph <- renderPlot({
    ggplot(variances_modif, aes(x = reorder(row.names(variances_modif), -mrate))) + # initialiser du graphique et de l'axe horizontal
      geom_bar(aes(y = modif.rate(res_mca)$modif$mrate),                            # indiquer le type de graphique (barplot) et la variable à représenter sur l'axe vertical
               stat = "identity", 
               fill = "red") +                                                      # choix couleur
      xlab("") +                                                                    # on enlève le label de l'axe des x, pas très utile
      ylab("% de variance") +                                                       # renommer proprement le label de l'axe des y
      theme_minimal()                                                              
    
  })
  
  
  # Graphique des variables actives
  
  output$plot_acm_act <- renderPlot({
    
    g_act <<- resultats_actives %>% 
      mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites))) %>%
      filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
               get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil) %>%   # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).
      
      ggplot(aes(x = get(paste0("dim",input$axe_X,"_coord")), y = get(paste0("dim",input$axe_Y,"_coord")), # initialisation du graphique
                 label = modalites,                                           # les labels des points sont les modalités
                 shape = variables,                                           # les formes des points dépendent des variables : à chaque variable son symbole
                 colour = variables,
                 size = 15)) +                                                
      
      geom_point() +                                                          # tracer les points
      geom_text_repel(size = input$taille_label, segment.alpha = 0.5) +       # tracer les labels, en utilisant cette fonction du package ggrepel qui permet de s'assurer qu'il n'y a pas de chevauchement. segment.alpha : transparence du petit tiret qui relie si besoin le libellé au point
      coord_fixed() +                                                         # pour que les échelles des axes soient identiques
      
      geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +  # ligne horizontale y = 0
      geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +  # ligne verticale x = 0
      
      xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
      ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical
      
      scale_shape_manual(name="", values = 0:20) +                            # sélection des types de symboles 
      
      guides(colour=guide_legend(title="", nrow = 2),                         # paramètres de la légende : pas de titre
             size = "none",                                                   # Pas de legende pour la taille et la forme
             shape = "none") + 
      
      theme_minimal(base_size = 18) +                                         # mise en forme globale du graphique ; theme_minimal est la plus "sobre" mais d'autres sont possibles... 
      
      theme(legend.position="bottom", legend.text = element_text(size = 20))  # pour que la légende se place sous le graphique.
    g_act
    
    
    
  })
  
  
  # Graphique des variables actives et supplémentaires
  
  output$plot_acm_complet <- renderPlot({
    
    resultats_complet %>% 
      mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites))) %>%
      filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
               get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil |
               is.na(get(paste0("dim",input$axe_X,"_contrib"))) == T |
               is.na(get(paste0("dim",input$axe_Y,"_contrib"))) == T) %>%    
      
      ggplot(aes(x = get(paste0("dim",input$axe_X,"_coord")), y = get(paste0("dim",input$axe_Y,"_coord")), 
                 label = modalites,
                 shape = variables,
                 colour = type,                                               # on distingue par des couleurs différentes les variables actives et supplémentaires
                 size = 15)) + 
      
      geom_point() +
      geom_text_repel(size = input$taille_label, segment.alpha = 0.5) +
      coord_fixed() +
      
      geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
      geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
      
      xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) +
      ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) +
      
      scale_shape_manual(name="", values = 0:20) +
      scale_color_manual(values = c("black", "red")) + 
      # scale_color_brewer(palette = "Set1") +
      # scale_color_grey() +
      # scale_color_brewer(palette = "Accent")
      
      guides(shape = "none",
             colour = guide_legend(title= "Type de variable",                 # titre de la légende distinguant actives et supplémentaires
                                   title.position = "top",
                                   nrow = 2),
             size = "none") + # toujours pas de légende pour les tailles de point
      
      theme_minimal(base_size = 18) +
      theme(legend.position="bottom", legend.text = element_text(size = 20))
    
  })
  
  
  
  
  
  
  
  
  
  # Graphique interactifs des variables
  
  
  
  output$plot_acm_interact <- renderGirafe({
    
    gg_point = resultats_complet %>% 
      mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites)),
             tooltip = c(paste0("Modalites = ", modalites, 
                                "\n Variables = ", variables, 
                                "\n Contrib1 = ", dim1_contrib, 
                                "\n Contrib2 = ", dim2_contrib, 
                                "\n Type = ", type))) %>% 
      filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
               get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil |
               is.na(get(paste0("dim",input$axe_X,"_contrib"))) == T |
               is.na(get(paste0("dim",input$axe_Y,"_contrib"))) == T) %>%
      ggplot() +
      geom_point_interactive(aes(x = get(paste0("dim",input$axe_X,"_coord")), 
                                 y = get(paste0("dim",input$axe_Y,"_coord")), 
                                 color = variables,
                                 shape = type,
                                 tooltip = tooltip, 
                                 data_id = modalites)) + 
      coord_fixed() +
      xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
      ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical
      theme_minimal()+ 
      theme(legend.position="none")
    # + theme(legend.position="bottom", legend.text = element_text(size = 12)) 
    
    girafe(ggobj = gg_point)
    
    
    
  })
  
  

  
  # Nuage des individus 
  
  coord_indiv <<- as.data.frame(res_mca$ind$coord) # récupérer les coordonnées des individus sur les deux premiers axes.
  
  
  output$plot_acm_interact_indiv <- renderGirafe({
    
    texte <- vector()
    text_id <- vector()
    text_sup <- vector()
    text_sup2 <- vector()
    text_act <- vector()
    text_act2 <- vector()
    
    for (k in c(1:nrow(data_acm))) {
      
      text_id[k] <- paste0("autoacm_id : ", data_acm[k,1], "\n\n" )
      for (i in var_sup) {
        text_sup[k] <- paste0(text_sup[k], "\n ", i, " : ", data_acm[k,i] )
      }
      
      text_sup[k] = substr(text_sup[k], 3, nchar(text_sup[k]))
      text_sup2[k] <- paste0(c("SUPPLEMENTAIRES : "), text_sup[k])
      
      for (j in var_act) {
        text_act[k] <- paste0(text_act[k], "\n ", j, " : ", data_acm[k,j] )
      }
      
      text_act[k] = substr(text_act[k], 3, nchar(text_act[k]))
      text_act2[k] <- paste0(c("\n\n ACTIVES : "), text_act[k])
      
      texte[k] <- paste0(text_id[k],text_sup2[k],text_act2[k])
      
    }
    
    
    gg_point_ind = coord_indiv %>% 
      cbind(data_acm) %>% 
      mutate(tooltip = texte) %>% 
      ggplot() +
      geom_point_interactive(aes(x = get(paste0("Dim ", input$axe_X)),
                                 y = get(paste0("Dim ", input$axe_Y)), 
                                 color = "#E41A1C",
                                 tooltip = tooltip
      )) + 
      coord_fixed() +
      xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
      ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical
      theme_minimal()+ 
      theme(legend.position="none")
    # + theme(legend.position="bottom", legend.text = element_text(size = 12)) 
    
    girafe(ggobj = gg_point_ind)
    
    
  })
  
  
  
  
  
  # Tables des indicateurs actifs
  
  output$resultat_acm_table_act <- renderDataTable({
    datatable(resultats_complet  %>% 
                filter(type == "Variable active")%>% 
                select(variables, modalites, n, pourcentage, 
                       contains(paste0("dim",input$axe_X,"_")), contains(paste0("dim",input$axe_Y,"_")))
    )
    
  })
  
  
  
  # Tables des indicateurs supplémentaires
  
  output$resultat_acm_table_sup <- renderDataTable({
    datatable(resultats_complet %>% 
                filter(type == "Variable supplementaire") %>% 
                select(variables, modalites, n, pourcentage, 
                       contains(paste0("dim",input$axe_X,"_")), contains(paste0("dim",input$axe_Y,"_"))) 
    )
    
  })
  
  
  ############################ 
  
  
  
  
  ############################   
  ### UI : CHOIX GRAPHIQUES & PARAMETRES
  
  
  output$affichage_choix_ACM <- renderUI({
    
    # On crée une liste pour les différents objets qu'on va représenter
    list_acm <- list("Saut d'inertie" = "variances" , 
                     "Saut d'inertie (Benzecri)" = "variances_Benz",
                     "ACM : variables actives" = "plot_acm_act",
                     "ACM : variables actives et supplémentatires" = "plot_acm_complet",
                     "ACM : graphique interactif" = "plot_acm_interact",
                     "ACM : individus" = "plot_acm_interact_indiv",
                     "Tables des variables actives" = "resultat_acm_table_act",
                     "Tables des variables supp." = "resultat_acm_table_sup")
    
    # On crée un bandeau avec les choix possibles
    
    
    output$warning_explor <- renderText({"    Attention : Cliquer sur 'ACM avec explor' ferme l'application en cours et ouvre l'interface d'explor()."})
    tagList(
      h5("Exploration de l'ACM"),
      column(12,
             wellPanel(
               # 1ere ligne dans le bandeau
               fluidRow(
                 # Choix de ce qu'on montre
                 column(5, selectInput(inputId="choix_MCA",
                                       label="Choix de la représentation : ",
                                       choices= list_acm,
                                       selected = list_acm[1])),
                 # Choix de l'axe X
                 column(2, offset=1, numericInput("axe_X", "Axe X :", 1, min = 1, max = 4)),
                 # Choix de l'axe Y
                 column(2, numericInput("axe_Y", "Axe Y :", 2, min = 1, max = 4)),
                 # Choix du seuil de contribution pour la représentation
                 column(2, numericInput("seuil", "Seuil contrib.:", 100 / nrow(res_mca$var$coord) , min = 0, max = 10))),
               
               # 2ème ligne dans le bandeau
               fluidRow(
                 column(7,
                        fluidRow(span(textOutput("warning_explor"), style="color:red")),
                        # fluidRow(helpText("Attention : Cliquer sur 'ACM avec explor' ferme l'application en cours et active la fonction explor().")),
                        fluidRow(column(4, offset=3, actionButton("explorACM", "ACM avec explor", class = "btn-danger")))),
                 column(2, offset = 1,
                        # Taille du graphique
                        sliderInput(inputId = "taille_graph",
                                    label = "Taille du graphique",                            
                                    min = 300, max = 3000, step = 100, value = 800)),
                 column(2,
                        # Taille des étiquette
                        sliderInput(inputId = "taille_label",
                                    label = "Taille des étiquettes",                            
                                    min = 2, max = 15, step = 1, value = 7)))
               
             )))
  }) # Fin renderUI
  
  removeModal()
  
  
}) # Fin Bouton ACM


############################ 




############################   
### UI : REPRESENTATION GRAPHIQUES ET TABLES


# Il faut choisir dans le bandeau le type de représentation
observeEvent(input$choix_MCA, { 
  
  # Pour saut d'inertie
  if (input$choix_MCA == "variances"){  
    
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(7,dataTableOutput(input$choix_MCA)),
        column(4, offset = 1,plotOutput("variances_graph"))
      )
    })
  }
  # Pour saut d'inertie Benzécri
  else if (input$choix_MCA == "variances_Benz"){
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(7,dataTableOutput(input$choix_MCA)),
        column(4, offset = 1,plotOutput("variances_Benz_graph"))
      )
    })
  }
  # Pour graphique ACM sans variables supplémentaires
  else if (input$choix_MCA == "plot_acm_complet" & is.null(var_sup) == T ){
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(12, plotOutput("plot_acm_act", height = input$taille_graph, width = input$taille_graph) )
      )
    })
  }
  # Pour la table des informations sur l'ACM
  else if (input$choix_MCA == "resultat_acm_table_act" ){
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(12, dataTableOutput(input$choix_MCA))
      )
    })
  }
  
  
  # Pour la table des informations sur l'ACM
  else if (input$choix_MCA == "resultat_acm_table_sup" ){
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(12, dataTableOutput(input$choix_MCA))
      )
    })
  }
  
  
  # Pour graphique ACM interactif
  else if (input$choix_MCA == "plot_acm_interact"){
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(12, girafeOutput("plot_acm_interact", height = input$taille_graph, width = input$taille_graph)) 
      )
    })
  }
  
  else if (input$choix_MCA == "plot_acm_interact_indiv"){
    output$affichage_ACM <- renderUI({
      fluidRow(
        column(12, girafeOutput("plot_acm_interact_indiv", height = input$taille_graph, width = input$taille_graph)) 
      )
    })
  }
  
  
  # Pour toutes les autres graphiques
  else{
    output$affichage_ACM <- renderUI({
      plotOutput(input$choix_MCA, height = input$taille_graph, width = input$taille_graph) 
    })
  }
  
  
})


















