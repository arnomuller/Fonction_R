#############
#### Server : Apprentis chercheurs
#############
# Ce code programme le server du programme shiny pour faire des 
# analyses exporatoires dans le cadres du projet pour les apprentis chercheurs




### Server ----

server = shinyServer(
  function(input, output, session) {
    
    
    
    # Import des données ----
    
    data <- reactive({
      inFile <<- input$target_upload
      if (is.null(inFile)) {
        return(NULL)
      }
      
      if (input$datatype == ".xlsx"){
        
        # Si ce qu'on importe n'est pas un xlsx, il ne se passe rien
        if (substr(inFile$datapath, nchar(inFile$datapath)-4, nchar(inFile$datapath)) == ".xlsx") {

          # On remplace les virgules par des points pour en faire des variables numeriques
          df <- data.frame(lapply(openxlsx::read.xlsx(inFile$datapath,1), 
                                  function(x) {gsub(",", ".", x)})) %>%
            
            # On crée un identifiant pour chaque ligne
            mutate(autoacm_id = rownames(.)) %>%
            select(autoacm_id, everything()) 
          nomcol_data <<- colnames(df)
          
          return(df)
        } else {
          return(NULL)
        }
        
      } else if (input$datatype == ".csv"){ # Même processus pour les fichiers csv.
        if (substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == ".csv") {
          
          tryCatch({
          
          df <- data.frame(lapply(read.csv(inFile$datapath, header = TRUE, sep = input$separator),
                                  function(x) {gsub(",", ".", x)})) %>%
            mutate(autoacm_id = rownames(.)) %>%
            select(autoacm_id, everything())
          
          
          nomcol_data <<- colnames(df)
          
          }, error = function(e) {
            return(NULL)
          })
          
          return(df)
        } else {
          return(NULL)
        }
        
      } else if (input$datatype == ".dta"){
        df <- as.data.frame(lapply(
          read_stata(inFile$datapath),
          as_factor)) %>%
            mutate(autoacm_id = rownames(.)) %>%
            select(autoacm_id, everything())
        nomcol_data <<- colnames(df)
        return(df)
        
      } else if (input$datatype == ".sas7bdat"){
        
        # Si pas de catalogue de labels :
        if (input$catalog == "Non"){
          
          if(substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == "bdat"){
          #df <- as.data.frame(read_sas(inFile$datapath))
          
            df <- as.data.frame(read_sas(inFile$datapath)) %>% 
              mutate(autoacm_id = rownames(.)) %>%
              select(autoacm_id, everything())
          
          nomcol_data <<- colnames(df)
          return(df)
          } else {
            return(NULL)
          }
          
        }
        
        # Si on a un catalogue de labels :
        if (input$catalog == "Oui"){
          # Import du fichier sas7bcat
          CataloginFile <<- input$catalog_upload
           if (is.null(CataloginFile))
             return(NULL)

          if(substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == "bdat"){
          # Import de la base avec les labels qui deviennent des modalités
            
            df <- import_sas_label(data_file = inFile$datapath, 
                                   catalog_file = CataloginFile$datapath,
                                   blanc_as_NA = FALSE)  %>% 
              mutate(autoacm_id = rownames(.)) %>%
              select(autoacm_id, everything())
            
          
          nomcol_data <<- colnames(df)
          return(df)
          
           } else {
             return(NULL)
           }
          
        }
      }
    }) # Fin Import
    
    
    # Warning si pas le bon format de données
    observeEvent(input$target_upload, {
      if(is.null(data()) == T ){
        
        ### AJOUT IF POUR LE TEMPS DE METTRE LE CATALOGUE AVEC SAS
        showModal(modalDialog(
          title = "Attention",
          "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
          easyClose = TRUE,
          footer = NULL))
        
      }
      
      
    })
    
    
    # On crée un objet "reactiveValues" qui est une sorte d'objet reactif, qui va pouvoir
    # contenir d'autres objets comme des dataframes.
    # L'avantage c'est qu'on peut le modifier dans différents observeEvent
    # au contraire du simple reactive, qui ne peut être crée et modifié que dans un
    # unique bloc de code.
    
    v <- reactiveValues(data = NULL)
    # Idem pour les noms de variables
    nomreac <- reactiveValues(nomcol = NULL)
    
    
    # 1ère modification : quand on importe les données v$data prend la valeur des
    # des données.
    
    
    # L'objet contenant les données se modifie quand : 
    # On importe les données
    observeEvent(input$target_upload, {
      data_init <<- data()
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
    })
    
    # On change le séparateur pour les csv
    observeEvent(input$separator, {
      validate(need(input$target_upload, 'Importer des données'))
      data_init <<- data()
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
    })
    
    
    # On active le catalogue
    observeEvent(input$catalog_upload, {
      validate(need(input$catalog_upload, 'Importer des données'))
      data_init <<- data()
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
    })
    
    # On change le type de données avec un message d'erreur si ce n'est pas le bon
    observeEvent(input$datatype, {
      validate(need(input$target_upload, 'Importer des données'))
      data_init <<- data()
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
      
      if(is.null(data()) == T ){
        
        showModal(modalDialog(
          title = "Attention",
          "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
          easyClose = TRUE,
          footer = NULL))
        
      }
    })
    
    
    # Changement de l'ordre des colonnes alphabetiquement
    observeEvent(input$col_alpha, {
      validate(need(input$target_upload, 'Importer des données'))
      

        if (input$col_alpha == TRUE) {
          #nomcol_data <<- order(colnames(data()))
          v$data <<- data() %>%
            select(order(colnames(data())))
          
          nomreac$nomcol <<- colnames(data() %>%
                                        select(order(colnames(data()))))
          
        } else{
          #nomcol_data <<- colnames(data())
          v$data <<- data()
          nomreac$nomcol <<- colnames(data())
        }

      
    })
    
    

    
    
    # On sauvegarde des objets réactifs qui renvoie les noms de variables.
    # Pour la base importée (= nomcol_data)
    nomcol_data_start <- reactive({
      nomcol_data
    })
    
    # Idem pour la base qui sera modifiée (utilisé dans les pickers)
    nomcol_data_reac <- reactive({
      colnames(v$data)
    })
    
    
    
    
    # Dimension de la table en entrée      
    n_col_start <- reactive({ ncol(data()) })
    n_row_start <- reactive({ nrow(data()) })
    
    # Affichage dimension table
    output$info_row <- renderPrint({
      validate(need(data(), ""))
      cat("Nombre d'individus :", isolate(n_row_start()))
    })
    output$info_col <- renderPrint({
      validate(need(data(), ""))
      cat("Nombre de variables :", isolate(n_col_start()))
    })
    
    
    
    
    # SUITE DU SERVER ----     
    
    # Pour la suite, on continue le server dans des scripts différents pour chacun des onglets.
    
    
    # 1) Variable ----  
    # Permet de modifier les variables
    source('ServeurVariable.R', local = TRUE)
    
    
    # 2) Sous Population ----  
    # Permet de faire une sous-population
    source('ServeurSousPopulation.R', local = TRUE)
    
    
    # 3) Table ----  
    # Permet d'observer les variables dans des tables
    source('ServeurTable.R', local = TRUE)
    
    
    # 4) ACM ----  
    source('ServeurACM.R', local = TRUE)
    
    
    # # 5) CAH ----  
     source('ServeurCAH.R', local = TRUE)
    
    
    
    
    # PASSERELLE VERS EXPLOR() ----
    
    
    # Appuyer sur le bouton explorACM, stoppe l'ACM et renvoie le signal explorACM
    # Ce signal sera reçu dans la fonction VirageACM (script fonction.R), pour ouvrir explor()
    
    observeEvent(input$explorACM, {
      js$closeWindow(); stopApp("explorACM")
    })
    
    
    
    
    # SAUVEGARDE ----
    
    
    # https://shiny.rstudio.com/articles/generating-reports.html
    # https://shiny.rstudio.com/gallery/download-knitr-reports.html
    # https://resources.symbolix.com.au/2020/10/28/downloadable-reports-shiny/
    
    # Création d'un rapport automatique
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      # Nom du rapport en sortie
      filename =     function() {
        paste0('report-',Sys.Date(),".", switch(
          input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      # Contenu du rapport
      content = function(file) {
        
        showModal(modalDialog("Création du rapport automatisé...", footer=NULL))
        on.exit(removeModal())
        # Copy the template report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list( name_data = inFile$name,
                        n_data = isolate(n_row_start()),
                        n_souspop = nrow(data_acm),
                        axe_X= input$axe_X,
                        axe_Y= input$axe_Y,
                        seuil= input$seuil,
                        taille_label= input$taille_label,
                        meth_dist = meth_dist,
                        ncluster = ncluster,
                        indic_cah = indic_cah,
                        is_html = input$report_format)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        out <- rmarkdown::render(tempReport,
                                 output_format = switch(
                                   input$report_format,
                                   PDF = pdf_document(), HTML = html_document(), Word = word_document()),
                                 output_file = file,
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )
        file.rename(out, file)
      }
    )
    
    
    
    # Reactif pour afficher la possibilité de sauvegarder les indicateur de l'ACM
    output$afficher_sauvegarde_indic <- reactive({
      if (input$acmOK >= 1) {
        "Oui"
      } else {
        "Non"
      }
      
    })
    outputOptions(output, "afficher_sauvegarde_indic", suspendWhenHidden=FALSE)
    
    
    # Bouton de téléchargement de la base des indicateurs
    output$save_indic <- downloadHandler(
      filename = function() {
        paste('autoacm-indicACM-', Sys.Date(), '.csv', sep = '')
      },
      content = function(file){
        write_csv(resultats_complet,file)
      }
    )
    
    
    
  })






