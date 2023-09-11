#########
#### UI : APPRENTIS CHERCHEURS
#########


# Ce code programme l'interface utilisateur du programme shiny pour faire des
# analyses exporatoires dans le cadres du projet pour les apprentis chercheurs
#

### Fonctionnement du code ----

# Si dans ce programme on paramètre l'interface, on pourra être amener à en programmer
# certains bouts, comme les sorties graphiques et les outils de séléction dans la partie 
# server, en même temps que les calculs
# Dans ce cas, j'utilise RenderUI pour indiquer au server qu'on code de l'interface
# et j'appellerai ces bouts de codes grâce à uiOutput() dans ce code.


### Interface Utilisateur ----

ui = shinyUI(
  fluidPage(
    
    
    
    # Choisir le theme : exemple : https://rstudio.github.io/shinythemes/
    
    # On crée un thème personnalisé à partir du package "fresh"
    use_theme(create_theme(
      theme = "default",
      # Wellpanel et sidebar panel par défaut blanc + bordure grise
      bs_vars_wells(
        bg = "#FFF",
        border =  "#E2E2E2"
        
      ),
      # Texte des tabs
      bs_vars_global(
        link_color = "#78B27A" #texte pas sélectionné du tabs
      ),
      # Tabulation
      bs_vars_pills(
        border_radius = "100px", # radius de l'arrondi du coin (0% = carrée)
        active_link_hover_color = "#FFF",
        active_link_hover_bg = "#78B27A"
      ),
      # Police des textes
      bs_vars_font(
        size_base = "11px",
        size_h4 = "15px",
        size_h5 = "14px",
        
      )
      
    )),
    
    # sidebarPanel pour Import, Sous-population et sauvegarde
    sidebarPanel(
      # Largeur du panel
      width = 3,
      # Bordure en blanc
      style = "border: white",
      
      # Import du logo
      tags$figure(
        align = "center",
        tags$img(
          src = "chameau.png",
          width = "50%",
        )
      ),
      br(),
      wellPanel(
        style = "background: #E2E2E2",
        h4("Import des données :"),
        # IMPORT DES DONNEES
        # Création de bouton CSV ou Excel, pour l'import des données
        radioButtons("datatype","Format des données à importer : ",choices = c(".csv",".xlsx",".sas7bdat",".dta"), selected=".csv",inline=TRUE),
        
        conditionalPanel(condition="input.datatype=='.csv'",
                         # Comme d'une base à l'autre le séparateur du CSV peut changer, on propose un choix avec radioButtons() :
                         radioButtons("separator","Séparateur des colonnes :",choices = c(";",",",":"), selected=";",inline=TRUE)),
        
        
        # Si les données sont en SAS
        conditionalPanel(condition="input.datatype=='.sas7bdat'",
                         # On crée un bouton pour savoir si l'util possede un catalogue des labels
                         radioButtons("catalog","Avez-vous un catalogue de labels (format .sas7bcat) ?",
                                      choices = c("Oui","Non"), selected="Non",inline=TRUE)),
        
        
        fileInput('target_upload', 'Choix de la base de donnée',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    '.csv',
                    '.xlsx',
                    '.sas7bdat',
                    '.dta'),
                  buttonLabel = "Parcourir...",
                  placeholder = "Pas de base sélectionnée"),
        
        
        # Si l'util a dit avoir un catalogue de format, alors on lui permet de l'importer dans le signal catalog_upload
        conditionalPanel(condition="input.catalog=='Oui' && input.datatype=='.sas7bdat'",
                         fileInput('catalog_upload', 'Choix du catalogue de label SAS (.sas7bcat)',
                                   accept = '.sas7bcat',
                                   buttonLabel = "Parcourir...",
                                   placeholder = "Pas de catalogue sélectionné")),
        
        # DIMENSION DE LA BASE DE DONNEES
        textOutput("info_row"),
        textOutput("info_col"),
        
        # Possibilité de trier les colonnes alphabetiquement
        checkboxInput(inputId = "col_alpha", label = "Tri colonnes alphabétique", value = FALSE)
        
      ), # Fin Wellpanel
      
      br(),
      
      
      conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                       
                       # CHOIX DE LA SOUS-POPULATION
                       wellPanel(
                         style = "background: #F4F4F4",
                         h4("Choix de la sous-population"),
                         uiOutput("selection_variables"),
                         # Choix des modalités d'intérêt
                         conditionalPanel(condition="output.afficher_choix_moda == 'Oui'",
                                          h5("Filtrer les modalités d'intérêt dans les variables choisies :"),
                                          # On appelle la boucle qui permet de faire la selection des modalités
                                          # Creée dans ServeurTableau.R
                                          uiOutput("selection_modalites"),
                                          fluidRow(column(12, align = "center", 
                                                          actionButton("fill_modasouspop", "Bis"))),
                                          br()
                                          
                         ),
                         # Nombre d'individus
                         textOutput("info_row_filter")
                       ),
                       
                       br(),
                       
                       
                       
                       
                       wellPanel(
                         style = "background: #A3D6A4",
                         h4("Sauvegarde"),
                         
                         conditionalPanel(condition="output.afficher_sauvegarde_indic == 'Non'",
                                          helpText("Sauvegarder les résultats de l'ACM et/ou de la classification.")
                         ),
                         
                         conditionalPanel(condition="output.afficher_sauvegarde_indic == 'Oui'",
                                          
                                          helpText("Appuyer sur ce bouton pour créer un rapport avec la sélection de population, les paramètres et les résultats de l'ACM et de la CAH"),
                                          
                                          br(),
                                          
                                          radioButtons('report_format', 'Document format', c('Word', 'HTML', 'PDF'),
                                                       inline = TRUE),
                                          fluidRow(column(12, align = "center",
                                                          downloadButton("report", "Rapport"))),
                                          br(),
                                          
                                          helpText("Sauvegarder les indicateurs de l'ACM :"),
                                          fluidRow(column(12, align = "center",downloadButton("save_indic", "Indicateurs ACM")))
                         )
                         
                       ) # Fin sauvegarde
      ) # Fin conditional
      
      
      
      
    ), # Fin SidebarPanel
    
    
    # AIRE CENTRALE
    mainPanel(
      width = 9,
      # Un panneau composé de plusieurs onglets
      tabsetPanel(
        id = "windows",  type = "pills",
        
        # Onglet qui affiche la base de données
        tabPanel("Base de données", value = "BDD",
                 br(),
                 
                 # Option de Zoom dans la table
                 conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                  fluidRow(
                                    column(2, offset = 10,
                                           sliderInput("zoom_tab", label = NULL, min = 50, 
                                                       max = 150, value = 80, post = "%", ticks = F)    
                                    )
                                  )),
                 # Affichage de la table générale
                 fluidRow(
                   uiOutput("view_tab")
                 ),
                 
                 conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                  br(),
                                  br(),
                                  helpText("Télécharger les données récodées et/ou filtrées"),
                                  fluidRow(column(12, align="center", id="buttons",
                                                  downloadButton('downLoadFilter',"Télécharger"))))
                 
                 
        ), # Fin BDD
        
        
        # Onglet qui permet de recoder ou réordonner des variables
        tabPanel("Variables", value = "Recod_Reord",
                 br(),
                 h4("Recoder ou réordonner les modalités des variables"),
                 helpText("- Recoder une variable permet de modifier les noms de modalités et de regrouper plusieurs modalités dans une même catégorie."),
                 helpText("- Réordonner une variable permet de changer l'ordre d'affichage des modalités (pour les tables ou les graphiques)"),
                 br(),
                 # Bouton pour choisir entre recoder ou réordonner
                 radioButtons("recod_or_reord","Voulez-vous :",choices = c("Recoder","Réordonner"), selected="Recoder",inline=TRUE),
                 # Voir ServeurVariable.R
                 
                 # Si on choisit recoder :
                 conditionalPanel(condition="input.recod_or_reord == 'Recoder'",
                                  # Choix de la variable à recoder
                                  uiOutput("choix_var_recod"),
                                  # Mise en forme du bouton GO en CSS
                                  tags$head(
                                    tags$style(
                                      HTML("
                                      #RecodeGO {
                                      background-color: #5E6FFF;
                                      color: white;
                                      border-color: #5E6FFF;
                                      }
                                      #RecodeGO:hover {
                                      background-color: darkblue;
                                      border-color: darkblue;
                                      }
                                           ")
                                    )
                                  ),
                                  # Bouton pour ouvrir l'interface de recodage
                                  fluidRow(column(4, align = "center", 
                                                  actionButton("RecodeGO", "GO !", class = "btn-success"))),
                                  br(),
                                  # Affichage d'une table de la variable à recoder
                                  fluidRow(column(12, align = "center",
                                                  tableOutput("table_recod_avant"))),
                                  br(),
                                  # Si une variable est sélectionnée, affichage de l'interface de recodage.
                                  conditionalPanel(condition = "input.var_recod != ''",
                                                   uiOutput("recodage")
                                  ),
                                  # Affichage des tables de la variable recodée avant/après avec textes explicatifs
                                  conditionalPanel(condition = "input.var_recod == ''",
                                                   fluidRow(column(12, align = "center",
                                                                   textOutput("texte_table_avant"))),
                                                   fluidRow(column(12, align = "center",
                                                                   uiOutput("aff_table_avant2")))),
                                  # Affichage de la table de la nouvelle variable
                                  conditionalPanel(condition = "input.var_recod == ''",
                                                   fluidRow(column(12, align = "center",
                                                                   textOutput("texte_table_apres"))),
                                                   fluidRow(column(12, align = "center",
                                                                   uiOutput("aff_table_apres"))))),
                 
                 # Si on choisit réordonner
                 conditionalPanel(condition="input.recod_or_reord == 'Réordonner'",
                                  # Choix de la variable à réordonner
                                  uiOutput("choix_var_reorder"),
                                  br(),
                                  # Interface pour réordonner
                                  uiOutput("reorder_ui"),
                                  # Bouton pour valider la réorganisation
                                  conditionalPanel(condition="input.var_reord != ''",
                                                   fluidRow(column(12, align = "center", 
                                                                   actionButton("ReorderOK", "Valider", class = "btn-success")))),
                                  # Affichage de la table réorganisée
                                  fluidRow(column(12, align = "center",
                                                  uiOutput("aff_table_apres_reord")))
                 ) # Fin Reorder
        ), # Fin Variable
        
        
        # Onglet d'exploration des tables
        tabPanel("Tables", value = "Tables",
                 br(),
                 # h4("Tableaux croisées"),
                 helpText("Dans cet onglet vous pouvez croiser jusqu'à trois variables."),
                 # Voir ServeurTable.R
                 
                 fluidRow(
                   # Affichage choix variables
                   column(5, wellPanel(
                     uiOutput("affichage_choix_var1"),
                     uiOutput("affichage_choix_var2"),
                     uiOutput("affichage_choix_var3"),
                     uiOutput("affichage_choix_var3_moda"))),
                   # Affichage image explicative
                   column(2, tags$figure(
                     align = "center",
                     tags$img(
                       src = "little_robot.svg",
                       width = "100%",
                     )
                   )),
                   # Affichage choix tables
                   column(5, 
                          uiOutput("affichage_choix_table_type")),
                 ),
                 br(),
                 
                 # Affichage de la table
                 fluidRow(column(12, align = "center",
                                 uiOutput("affichage_table")
                 )),
                 br(),
                 br(),
                 # Sauvegarde de la table
                 
                 conditionalPanel(condition="output.afficher_choix_souspop == 'Oui' && input.var_table1 != ''", 
                                  fluidRow(column(12, align="center", id="buttons",
                                                  downloadButton('savetable',"Télécharger la table"))))
                 
                 
                 
        )# Fin Tables
        
        ,
        # Onglet ACM
        tabPanel("ACM", value = "ACM",
                 br(),
                 helpText("Dans cet onglet vous pouvez créer votre analyse des correspondances multiples"),
                 #h4("Analyse des Correspondances Multiples"),
                 # On affiche une phrase avec le nombre de personnes dans la population
                 uiOutput("recap_activ"),
                 br(),
                 
                 conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                  # On sélectionne les variables active et supplémentaires
                                  h5("Sélection des variables actives et supplémentaires"),
                                  uiOutput("acm_main_act"),
                                  uiOutput("acm_main_sup"),
                                  
                                  # On affiche 2 boutons
                                  helpText("Cliquer sur 'Valider' une fois votre choix de variables fait."),
                                  fluidRow(column(5, offset = 2,
                                                  # Valider l'ACM
                                                  actionButton("acmOK", "Valider", class = "btn-success"))
                                  )
                                  
                 ),
                 
                 
                 
                 br(),
                 hr(), # Barre horizontale
                 
                 
                 # On affiche le bandeau de choix des représentations graphiques et tables
                 uiOutput("affichage_choix_ACM"),
                 
                 # On affiche la représentation demandée
                 uiOutput("affichage_ACM"),
                 
                 br(),
                 br())
        , # Fin ACM
        
        # Onglet CAH
        tabPanel("Classification", value = "CAH",
                 
                 br(),
                 helpText("Dans cet onglet vous pouvez créer une classification à partir des distances de Ward."),
                 
                 conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                  fluidRow(column(6, align = "center",
                                                  actionButton("cahOK", "Lancer la classification", class = "btn-success")))),
                 
                 br(),
                 br(),
                 
                 conditionalPanel(condition="output.lancer_cah == 'Oui'",    
                                  
                                  # On affiche le bandeau de choix de représentations
                                  uiOutput("affichage_choix_graph_cah"),
                                  # On affiche la représentation demandée
                                  uiOutput("affichage_graphique"),
                                  
                                  br(),
                                  br(),
                                  
                                  
                                  hr(),
                                  
                                  #### condipan
                                  
                                  h5("Description bivariée de la variable 'cluster' :"),
                                  # On affiche l'outil de selection de variable
                                  uiOutput("affichage_choix_table_cah"),
                                  # On affiche le tri croisé de "cluster" avec la variable voulue
                                  fluidRow(column(12, align="center", uiOutput("affichage_table_cah"))),
                                  
                                  br(),
                                  hr(),
                                  
                                  fluidRow(h5("Vous pouvez exporter les données avec la nouvelle variable de classe")),
                                  
                                  br(),
                                  
                                  fluidRow(column(12, align = "center",id="buttons",
                                                  downloadButton('downLoadCluster',"Télécharger avec variable de classes"))),
                                  
                                  br(),
                                  hr(),
                                  br()
                 )
                 ###########"
                 ,
                 conditionalPanel(condition="output.lancer_cah == 'Non'",
                                  
                                  fluidRow(
                                    column(12, align = "center",id="buttons",
                                           h5("ATTENTION : Vous avez changé la taille de l'échantillon, 
                                    veuillez retourner sur l'onglet ACM pour la créer sur ce nouvel échantillon.")))
                                  
                 ),
                 
                 conditionalPanel(condition="output.lancer_cah == 'Non_newACM'",
                                  
                                  fluidRow(
                                    column(12, align = "center",id="buttons",
                                           h5("ATTENTION : Vous avez changé l'ACM, 
                                    veuillez relancer la classification.")))
                                  
                 )
                 
                 
        ) # Fin CAH
        
        
      ) # Fin tabsetpanel
    ) # Fin mainpanel
  ) # Fin fluidpage
) # FIN UI











