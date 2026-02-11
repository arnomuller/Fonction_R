########################################
######           BATONS            #####
########################################



#################################


ined_baton = function(
    donnees = NULL,
    var_y = 1,
    var_x = 2,
    var_couleur = NULL,
    var_facet = NULL,
    facet_disposition = "lignes",
    
    etiquette = "oui",
    etiquette_min = -999,
    etiquette_arrondi = 0,
    
    var_x_ordre = "base",
    var_couleur_ordre = "base",
    palette = "categorie",
    
    legende_position = "bottom",
    legende_ncol = 1,
    legende_ordre = "colonne",
    
    titre = "",
    titre_x = "",
    titre_y = "",
    titre_legende = "",
    
    y_min = NULL,
    y_max = NULL,
    y_intervalle = NULL,
    y_pct = FALSE,


    
    var_x_angle = 0,
    bg_color = "#FFF",
    
    ncarac_titre = 50,
    ncarac_titre_x = 35,
    ncarac_titre_y = 35,
    ncarac_titre_legende = 30,
    ncarac_var_x = 25,
    ncarac_legende = 40,
    ncarac_facet = 40
    
){
  
  
  
  #############################
  ### OPTIONS                ----
  # Ecriture scientifique
  options(scipen=9999)
  options(OutDec= ",")
  
  ### GESTION LIBRARY        ----
  # Liste des packages à charger
  packages <- c("tidyverse","ggrepel")
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  #############################
  
  
  
  
  
  
  #######################################
  ## Données                  ----
  
  dt = donnees
  
  if(is.numeric(var_y) == F){val_pos = which(colnames(dt) == var_y)} else {val_pos = var_y}
  if(is.numeric(var_x) == F){varX_pos = which(colnames(dt) == var_x)} else {varX_pos = var_x}
  
  
  ## Couleur
  if(is.null(var_couleur)){
    var_couleur_pos = NULL
  } else{
    if(is.numeric(var_couleur) == F){
      var_couleur_pos = which(colnames(dt) == var_couleur)
    } else {var_couleur_pos = var_couleur}
  }
  
  ## Facet
  if(is.null(var_facet)){
    var_facet_pos = NULL
  } else{
    if(is.numeric(var_facet) == F){
      var_facet_pos = which(colnames(dt) == var_facet)
    } else {var_facet_pos = var_facet}
  }
  
  
  ############
  
  
  ############
  ## Ordre et nom des variables
  
  dt_plot = dt |> 
    rename(value = all_of(val_pos),
           varX = all_of(varX_pos))
  
  
  
  if(var_x_ordre == "base"){
    dt_plot = dt_plot |> 
      mutate(varX = str_wrap(varX, ncarac_var_x))|> 
      mutate(varX = factor(varX, levels = unique(as.character(varX))))
  } else if(var_x_ordre == "alpha"){
    dt_plot = dt_plot |> 
      mutate(varX = str_wrap(varX, ncarac_var_x))|> 
      mutate(varX = factor(varX, levels = names(table(as.character(varX)))))
  } else if(var_x_ordre == "croissant"){
    dt_plot = dt_plot |> 
      mutate(varX = str_wrap(varX, ncarac_var_x))|> 
      arrange(value)  |> 
      mutate(varX = factor(varX, levels = unique(as.character(varX))))
  } else if(var_x_ordre == "inverse"){
    dt_plot = dt_plot |> 
      mutate(varX = str_wrap(varX, ncarac_var_x))|> 
      mutate(varX = factor(varX, levels = rev(unique(as.character(varX)))))
  }
  
  
  
  
  if(is.null(var_couleur_pos) == FALSE) {
    dt_plot = dt_plot |>
      rename(couleur = all_of(var_couleur_pos)) |> 
      mutate(couleur = str_wrap(couleur, ncarac_legende))
    
    if(var_couleur_ordre == "base"){
      dt_plot = dt_plot |> 
        mutate(couleur = factor(couleur, levels = unique(as.character(couleur))))
    } else if(var_couleur_ordre == "alpha"){
      dt_plot = dt_plot |> 
        mutate(couleur = factor(couleur, levels = names(table(as.character(couleur)))))
    } else if(var_couleur_ordre == "inverse"){
      dt_plot = dt_plot |> 
        mutate(couleur = factor(couleur, levels = rev(unique(as.character(couleur)))))
    }
  }
  
  
  
  if(is.null(var_facet_pos) == FALSE) {
    dt_plot = dt_plot |>
      mutate(facet = get(colnames(dt_plot)[var_facet_pos])) |> 
      mutate(facet = str_wrap(facet, ncarac_facet))  |> 
      mutate(facet = factor(facet, levels = unique(facet)))

  } else {
    dt_plot = dt_plot |>
      mutate(facet = "Facet") 
  }
  

  
  if(etiquette %in% c("yes","oui",TRUE)){
    
    if(y_pct == T){
      dt_plot = dt_plot |>
        mutate(etiquette = ifelse(value > etiquette_min, paste0(round(value,etiquette_arrondi),"%"), NA))
    }else{
      dt_plot = dt_plot |>
        mutate(etiquette = ifelse(value > etiquette_min, round(value,etiquette_arrondi), NA))
    }
    
    
  } else if (etiquette %in% c("max","maxi")){
    
    if(y_pct == T){
      dt_plot = dt_plot |>
        group_by(varX,facet)  |> 
        mutate(etiquette = ifelse(value == max(value), paste0(round(value,etiquette_arrondi),"%"), NA))
    }else{
      dt_plot = dt_plot |>
        group_by(varX,facet)  |> 
        mutate(etiquette = ifelse(value == max(value), round(value,etiquette_arrondi), NA))
    }
    
    
  } else {
    dt_plot = dt_plot |>
      mutate(etiquette = NA)
  }
  
  
  #######################################
  
  
  #######################################
  ## Paramètres graphiques
  
  #police = "arial"
  
  color_text = "#41414d"
  color_quadri = "#a8a8b3"
  size_text = 8
  size_text_title = size_text
  size_title = size_text+3
  
  
  ############
  ## Couleurs
  
  
  pal_genre        = c("#469164","#e3a852","#8ac2bd","#4f4f4f")
  pal_categorie    = c("#a993b9","#ea6058","#8DD3C7","#FDB462","#8C6642","#B3DE69","#80B1D3","#f5aba9","#5b4087")
  pal_ordre_orange = c("#faeceb","#e8c1be","#e6aaa5","#ea6058","#ad312a","#7d241e","#450602") 
  pal_ordre_violet = c("#f6f0fa","#d2c1de","#a993b9","#775e8a","#57396e","#321647","#140321") 
  
  pal_oppose2       = c("#775e8a","#ea6058")
  pal_oppose3       = c("#775e8a","#e7e6e8","#ea6058")
  pal_oppose4       = c("#775e8a","#a993b9","#e6aaa5","#ea6058")
  pal_oppose5       = c("#775e8a","#a993b9","#e7e6e8","#e6aaa5","#ea6058")
  pal_oppose6       = c("#775e8a","#a993b9","#d2c1de","#e8c1be","#e6aaa5","#ea6058")
  pal_oppose7       = c("#775e8a","#a993b9","#d2c1de","#e7e6e8","#e8c1be","#e6aaa5","#ea6058")
  pal_oppose8       = c("#57396e","#775e8a","#a993b9","#d2c1de","#e8c1be","#e6aaa5","#ea6058","#ad312a")
  

  # # Modalités avec couleurs spéciales
  # asupr = "ne sait pas|999|888|ne souhaite pas répondre|nsp|nvpr|ne veut pas rép|non réponse|non-réponse|total|ensemble"
  # 
  # # Modalités classiques
  # moda_couleur = names(table(dt_plot$couleur))
  # moda_couleur_simple = moda_couleur[!str_detect(tolower(moda_couleur),asupr)]
  # n_moda_vrai = length(moda_couleur_simple)
  # 
  # # Les NSP et non-réponses
  # nsp = "ne sait pas|999|888|ne souhaite pas répondre|nsp|nvpr|ne veut pas rép|non réponse|non-réponse"
  # moda_nsp = c()
  # moda_nsp  = moda_couleur[str_detect(tolower(moda_couleur),nsp)]
  # length(moda_nsp)
  # 
  # # Les totaux
  # tot = "total|ensemble"
  # moda_tot = c()
  # moda_tot  = moda_couleur[str_detect(tolower(moda_couleur),tot)]
  # 
  # # Le vrai ordre des modalités
  # moda_couleur_vf = c(moda_tot,moda_couleur_simple,moda_nsp)
  
  
  
  if( is.null(var_couleur_pos) == F) {
    n_moda = length(names(table(dt_plot$couleur)))
    fill_color = case_when(
      palette %in% c("genre","sexe","dem01")     ~ pal_genre[1:n_moda],
      palette %in% c("genre_inv","sexe","dem01") ~ rev(pal_genre[1:n_moda]),
      palette %in% c("categorie","cat")          ~ pal_categorie[1:n_moda],
      palette %in% c("categorie_inv","cat_inv")  ~ rev(pal_categorie[1:n_moda]),
      palette == "orange"                        ~ pal_ordre_orange[1:n_moda],
      palette == "orange_inv"                    ~ rev(pal_ordre_orange[1:n_moda]),
      palette == "violet"                        ~ pal_ordre_violet[1:n_moda],
      palette == "violet_inv"                    ~ rev(pal_ordre_violet[1:n_moda]),
      (palette == "oppose" & n_moda == 2)        ~ pal_oppose2[1:n_moda],
      (palette == "oppose" & n_moda == 3)        ~ pal_oppose3[1:n_moda],
      (palette == "oppose" & n_moda == 4)        ~ pal_oppose4[1:n_moda],
      (palette == "oppose" & n_moda == 5)        ~ pal_oppose5[1:n_moda],
      (palette == "oppose" & n_moda == 6)        ~ pal_oppose6[1:n_moda],
      (palette == "oppose" & n_moda == 7)        ~ pal_oppose7[1:n_moda],
      (palette == "oppose" & n_moda == 8)        ~ pal_oppose8[1:n_moda],
      (palette == "oppose_inv" & n_moda == 2)    ~ rev(pal_oppose2[1:n_moda]),
      (palette == "oppose_inv" & n_moda == 3)    ~ rev(pal_oppose3[1:n_moda]),
      (palette == "oppose_inv" & n_moda == 4)    ~ rev(pal_oppose4[1:n_moda]),
      (palette == "oppose_inv" & n_moda == 5)    ~ rev(pal_oppose5[1:n_moda]),
      (palette == "oppose_inv" & n_moda == 6)    ~ rev(pal_oppose6[1:n_moda]),
      (palette == "oppose_inv" & n_moda == 7)    ~ rev(pal_oppose7[1:n_moda]),
      (palette == "oppose_inv" & n_moda == 8)    ~ rev(pal_oppose8[1:n_moda])
    )
  } else{
    fill_color = case_when(
      palette %in% c("genre","sexe")            ~ pal_genre[1],
      palette %in% c("genre_inv","sexe_inv")    ~ pal_genre[2],
      palette %in% c("categorie","cat")         ~ pal_categorie[1],
      palette %in% c("categorie_inv","cat_inv") ~ pal_categorie[2],
      palette %in% c("orange","orange_inv")     ~ pal_ordre_orange[4],
      palette %in% c("violet","violet_inv")     ~ pal_ordre_violet[4],
      palette %in% c("oppose")                  ~ pal_categorie[1],
      palette %in% c("oppose_inv")              ~ pal_categorie[2]
    )

  }
  
  
  
  
  
  
  #############
  # THEME
  
  bg_col = case_when(
    is.null(bg_color) ~ "#FFF",
    bg_color == "orange" ~ "#f2e2e1",
    bg_color == "violet" ~ "#f0edf5",
    TRUE ~ "#FFF"
  )
  
  # LEGENDE
  
  if(is.numeric(legende_position) & length(legende_position) == 2){
    theme_legend_pos = legende_position
    
  } else {
    theme_legend_pos = case_when(
      legende_position %in% c("haut","top") ~ "top",
      legende_position %in% c("bas","bottom") ~ "bottom",
      legende_position %in% c("droite","right") ~ "right",
      legende_position %in% c("gauche","left") ~ "left",
      legende_position %in% c("courbe", "none", "non","no") ~ "none",
      TRUE ~ "none"
    )
  }
  
  
  # LE THEME
  
  
  xaxis_hjust = case_when(
    var_x_angle == 90 ~ 1,
    var_x_angle < 90 & var_x_angle >= 10 ~ 1,
    var_x_angle < 10 & var_x_angle >= 0 ~ 0.5,
    TRUE ~ 0.5
  )
  
  xaxis_vjust = case_when(
    var_x_angle == 90 ~ 0.5,
    var_x_angle < 90 & var_x_angle >= 10 ~ 1,
    var_x_angle < 10 & var_x_angle >= 0 ~ 0.5,
    TRUE ~ 0.5
  )
  
  
  montheme = theme(
    
    plot.background = element_rect(fill = bg_col),
    panel.background = element_rect(fill = bg_col,
                                    colour = bg_col,
                                    linewidth = 0.5, 
                                    linetype = "solid"),
    
    
    strip.text.x = element_text(size = size_text_title, 
                                colour = "white",face = 2, 
                                angle = 0
                                #,family = police
                                ),
    strip.background.x = element_rect(fill = color_text,colour = NA),
    strip.text.y = element_text(size = size_text_title, 
                                colour = "white",face = 2
                                #,family = police
                                ),
    strip.background.y = element_rect(fill = color_text,colour = NA),
    strip.placement = "outside",
    
    
    legend.position= theme_legend_pos,
    legend.title = element_blank(),
    legend.text  = element_text(size = size_text, 
                                #family = police,
                                colour = color_text),
    legend.background = element_blank(),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    # legend.spacing.y  = unit(-14, "pt"),
    # legend.spacing.x  = unit(-14, "pt"),
    legend.margin   = margin(t = -8),
    plot.margin     = margin(t = 5, r = 5, b = 3, l = 2),
    
    
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_line(color = color_quadri,linewidth = 0.25,linetype = "dashed"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_line(color = color_quadri,linewidth = 0.25,linetype = "dashed"),
    
    axis.text.x = element_text(size = size_text, 
                               #family = police,
                               colour = color_text, 
                               angle = var_x_angle, 
                               vjust = xaxis_vjust, 
                               hjust = xaxis_hjust, 
                               face = 1),
    axis.text.y = element_text(size = size_text, 
                               #family = police,
                               colour = color_text),
    
    plot.title  = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.ticks = element_blank()
  )
  
  
  
  if(nchar(titre_legende) > 0){
    montheme = montheme + theme(legend.title = element_text(size = size_text_title, 
                                                            #family = police,
                                                            colour = color_text))
  }
  
  if(nchar(titre) > 0){
    montheme = montheme + theme(plot.title = element_text(size = size_title, 
                                                          #family = police,
                                                          colour = color_text))
  }
  
  if(nchar(titre_x) > 0){
    montheme = montheme + theme(axis.title.x = element_text(size = size_text_title, 
                                                            #family = police,
                                                            colour = color_text,
                                                            hjust = 1))
  }
  
  if(nchar(titre_y) > 0){
    montheme = montheme + theme(axis.title.y = element_text(size = size_text_title, 
                                                            #family = police,
                                                            colour = color_text))
  }
  
  #######################################
  
  
  
  #######################################
  ### Graphique ----
  
  if(is.null(var_couleur_pos)){
    dt_plot = dt_plot |> 
      mutate(couleur = "") 
  }
  
  

  p = dt_plot |> 
    ggplot(aes(x = varX, y = value, fill = couleur)) +
    guides(fill = "none") + 
    geom_col(color = "white",
             width = 0.75, 
             position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = fill_color)
  
  
  legend_en_ligne = ifelse(legende_ordre == "ligne", TRUE, FALSE)
  
  
  if(!is.null(var_couleur_pos)){
    
    p = p +
      guides(fill = guide_legend(ncol = legende_ncol, 
                                 reverse = F,
                                 byrow = legend_en_ligne,
                                 title.position = "top", 
                                 title.hjust = 0.5))
  } 
  
  
  if(is.null(var_facet_pos) == F){
    
    if (facet_disposition %in% c("ligne","lignes","row","rows")) {
      p = p +
        facet_grid(facet ~ ., scales = "free_y", space = "free_y", switch = "y") 
    } else if (facet_disposition %in% c("colonne","colonnes","column","columns")){
      p = p +
        facet_grid(. ~ facet, scales = "free_y", space = "free_y", switch = "y") 
    } else {
      p = p +
        facet_wrap(vars(facet))
    }
  }
  
  
  if(etiquette %in% c("yes","oui",TRUE, "max", "maxi")){
    p = p +
      geom_text_repel(aes(label = etiquette, y = value,
                          color = couleur),
                      direction = "y",
                      bg.color = "white", 
                      bg.r = 0.1 ,         
                      xlim = c(NA, NA),
                      ylim = c(NA, NA),
                      min.segment.length = Inf,
                      hjust = 0.5,
                      vjust = 1,
                      
                      size = size_text/3,
                      fontface = 2,
                      position = position_dodge(width = 0.75),
                      
      ) +
      scale_color_manual(values = fill_color) +
      guides(color = "none")
  }
  
  

  
  p = p + 
    geom_hline(yintercept=0) +
    labs(title = str_wrap(titre,ncarac_titre),
         x = str_wrap(titre_x, width = ncarac_titre_x),
         y = str_wrap(titre_y, width = ncarac_titre_y),
         fill = str_wrap(titre_legende, width = ncarac_titre_legende)) +
    montheme
  
  
  
  
  if(is.null(y_max) == F & is.null(y_min) == F){
    p = p + coord_cartesian(ylim = c(y_min, y_max) )
  } else if(is.null(y_max) == F & is.null(y_min) == T){
    p = p + coord_cartesian(ylim = c(0, y_max) )
  }
  
  
  if(y_pct == T) {
    
    if(is.null(y_intervalle) == F){
      p = p + scale_y_continuous(
        labels = scales::percent_format(scale = 1,decimal.mark = ","),
        breaks = scales::breaks_width(y_intervalle)) 
    } else {
      p = p + scale_y_continuous(
        labels = scales::percent_format(scale = 1,decimal.mark = ",")) 
    }
    
  } else {
    
    if(is.null(y_intervalle) == F){
      p = p + scale_y_continuous(
        breaks = scales::breaks_width(y_intervalle)) 
    } 
  }
  
  
  
  return(p)
  
}












