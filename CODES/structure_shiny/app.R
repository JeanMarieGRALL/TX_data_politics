library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(leaflet)
library(stringr)
library(data.table)
library(rgdal)
library(ggplot2)
library(plotly)
library(shinyWidgets)

# ----------------------------------------------------------
# Import des BBD --------------------------------------------
# ----------------------------------------------------------
communes = read_rds("data/communes_1er_tour_2022.rds")
resultats1erT = read_rds("data/resultats_1er_tour_2022.rds")
resultats2ndT = read_rds("data/resultats_T2_2022.rds")

# Pour les graphiques en centiles
centile_rfr_candidat = read_rds("data/centile_rfr_commune_candidat.rds")
centile_prixm2_candidat = read_rds("data/centile_prixm2_commune_candidat.rds")
centile_popu_candidat = read_rds("data/centile_popu_commune_candidat.rds")
centile_csp_moins_candidat = read_rds("data/centile_csp_moins_candidat.rds")
centile_csp_plus_candidat = read_rds("data/centile_csp_plus_candidat.rds")
centile_chomage_candidat = read_rds("data/centile_chomage_candidat.rds")

# Divers
score_candidats = read_rds("data/score_candidats.rds")

# ----------------------------------------------------------
# code -----------------------------------------------------
# ----------------------------------------------------------

header <- dashboardHeader(
    title = "Elections Présidentielles",
    tags$li(class = "dropdown",
            "DOMMARTIN Brieuc & GRALL Jean-Marie, Printemps 2022, Université de Technologie de Compiègne",
                style = "position:absolute;
                         top:0;
                         right:0;
                         padding: 15px;
                         z-index: 1000;
                         color: white;"
                )
)

sidebar <- dashboardSidebar(# SIDEBAR
    sidebarMenu(
        menuItem("Acceuil", tabName = "acceuil", icon = icon("home")), # Onglet acceuil
        menuItem("Premier tour 2022", icon = icon("calendar"), # Onglet 1er tour 2022
                 menuSubItem("Cartes des résultats", tabName = "1er_t_resultats"),
                 menuSubItem("Résultats par candidats", tabName = "1er_t_candidats")
        ),
        menuItem("Second tour 2022", icon = icon("calendar"), # Onglet 2nd tour 2022
                 menuSubItem("Cartes des résultats", tabName = "2nd_t_resultats"),
                 menuSubItem("Evolution 2017-2022", tabName = "2nd_t_evo")
        ),
        menuItem("Analyse statistique", icon = icon("chart-line"), # Onglet analyse statistique
                 menuSubItem("ACP sur les résultats", tabName = "stat_ACP"),
                 menuSubItem("Régression logistique", tabName = "stat_reg_log")
                              
                    
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem("acceuil", # Page d'acceuil
            h1("Objectifs de ce dashboard"),
            
            h3("Contexte"),
            div(HTML("Dans un contexte de développement des outils d'étude de données massives (<em>Big Data</em>), les données occupent une place de plus en plus importante lors des campagnes électorales. 
                     De plus, le partage de données publiques (<em>Open Data</em>) par l'état (<a href='https://www.data.gouv.fr' target='_blank'>www.data.gouv.fr</a>), 
                     offre la possibilité d'analyser les résultats d'élections de manière gratuite et transparente.")),
            
            div(HTML("Les élections présidentielles 2022 sont marquées par un second tour identique à celui de 2017 (Macron - Le Pen). 
                     Cependant, elles se déroulent après mandat d'Emmanuel Macron compliqué par la crise sanitaire et la crise des gilets jaunes.
                     De ce fait, la situation et l'opinion des français ont bien évoluées entre ces deux présidentielles.")),
            
            h3("Etude"),
            div(
              HTML("Ce dashboard présente une analyse socio-éconimique des résultats des élections présidentielles 2017 à travers les données open data à l'échelle des communes. 
              Nous allons également présenter une comparaison des deux dernières présidentielles (2017 et 2022) afin de mieux comprendre les changements d'opinion politique opérés en cinq ans.<br><br>"),
              imageOutput("logoUTC", inline = TRUE),
              HTML("<br><br>Cette étude à été réalisée dans le cadre d'une UV à projet à l'Université de Technologie de Compiègne. Projet encadré par Mme. Anne BELLON et réalisé par Brieuc DOMMARTIN et Jean-Marie GRALL."))
        ),
        tabItem("1er_t_resultats", # Page des resultats du 1er tour 2022
                title = "France : carte premier tour",
                valueBox(
                  value = textOutput("score_cand_avec_abstention_"),
                  subtitle = "Score du candidat (%), en prenant en compte l'abstention ainsi que les votes blancs et nuls",
                  icon = icon("chart-line"),
                  color = "green",
                  width = 12
                ),
                box(
                  width = 6,
                  selectInput("candidat_1er_t_resultats_name", "Candidat:",
                              choices=names(table(centile_rfr_candidat$candidat)))),
                box(
                  width = 6,
                  selectInput("candidat_1er_t_resultats_reg", "Région:",
                              choices=names(table(communes$region_name)))),
                box(
                  imageOutput("carte_france_png_res_1t"), 
                  title = "Résultat du candidat en France métropolitaine",
                  footer = "Echelle: Rapport (score du candidat dans la commune) / (score moyen du candidat en France)"
                  ),
                box(
                  leafletOutput("leaflet_region"),
                  title = "Zoom sur une région",
                  footer = "Echelle: Rapport (score du candidat dans la commune) / (score moyen du candidat en France)"
                  ),
        ),
        tabItem("1er_t_candidats", # Page des resultats du 1er tour 2022 - focus candidat
                valueBox(
                    value = textOutput("score_cand_sans_abstention"),
                    subtitle = "Score du candidat (%), sans prendre en compte l'abstention, les votes blancs et nuls",
                    icon = icon("chart-line"),
                    color = "light-blue",
                    width = 6
                ),
                valueBox(
                    value = textOutput("score_cand_avec_abstention"),
                    subtitle = "Score du candidat (%), en prenant en compte l'abstention, les votes blancs et nuls",
                    icon = icon("chart-line"),
                    color = "green",
                    width = 6
                ),
                box(
                    solidHeader = TRUE,
                    width = 3,
                    selectInput("candidat_1er_t_candidats", "Candidat:",
                                choices=names(table(centile_rfr_candidat$candidat)))
                ),
                box(
                    solidHeader = TRUE,
                    width = 3,
                    selectInput("variable_1er_t_candidats", "Variable:",
                                choices=c("Prix du mètre carré", "Taux de chômage", "Population de la commune", "Revenu médian", "CSP+", "CSP-"))
                ),
                box(
                    title = strong("Méthodologie"),
                    solidHeader = TRUE,
                    width = 6,
                    "Les plus de 30 000 communes françaises sont classées dans l'ordre croissant selon la variable choisie. Ensuite, nous découpons l'ensemble des communes en centiles: 100 groupes d'à peu près 300 communes chacun. Pour finir nous observons le score moyen du candidat choisi pour chaque centile."
                ),
                box(
                    footer = "Données: INSEE et Ministère de l'intérieur",
                    status = "info",
                    solidHeader = TRUE,
                    width = 9,
                    plotOutput("candidat_centile_commune")
                ),
                box(
                    title = strong("Description des variables"),
                    solidHeader = TRUE,
                    width = 3,
                    strong("Prix au mètre carré:"), "prix moyen d'un mètre carré (appartement ou maison) dans la commune.", br(), br(),
                    strong("Taux de chômage:"), "taux de chômage dans la commune (données 2017).", br(), br(),
                    strong("Population de la commune:"), "nombre d'habitants dans la commune (données 2019).", br(), br(),
                    strong("Revenu médian:"), "médiane des revenus déclarés dans la commune (données 2019).", br(), br(),
                    strong("CSP+:"), "pourcentage de cadres et de professions supérieures présents dans la commune (données 2017).", br(), br(),
                    strong("CSP-:"), "pourcentage d'ouvriers et d'employés présents dans la commune (données 2017)."
                )
        ),
        tabItem("2nd_t_resultats", # Page des resultats du 2nd tour 2022
                title = "Résultats du second tour présidentielles 2022",
                h1("Résultats du second tour 2022"),
                box(
                  width = 12,
                  "Le second tour des élections présidentielles 2022 est identique à celui de 2017. Cependant M. Macron termine un mandat compliqué,
                  marqué par une crise sanitaire et la crise des gilets jaunes."
                ),
                box(title = "Carte de la France métropolitaine: Macron - Le Pen",
                    imageOutput("carte_france_2t"),
                    width = 6,
                ),
                box(
                  leafletOutput("leaflet_region_2ndT"),
                  title = "Zoom sur une région",
                  width = 6,
                ),
                valueBox(
                  value = "18544",
                  subtitle = "Nombre de communes remportées par Marine Le Pen",
                  color = "blue",
                  width = 3
                ),
                valueBox(
                  value = "16701",
                  subtitle = "Nombre de communes remportées par Emmanuel Macron",
                  color = "red",
                  width = 3
                ),
                box(
                  width = 6,
                  selectInput("candidat_2nd_t_resultats_reg", "Région:",
                              choices=names(table(communes$region_name)))
                )
        ),
        tabItem("2nd_t_evo",  # Page de comparaison des 2nd tours 2017 et 2022
                title = "Evolution 2017 - 2022",
                h1("Evolution des résultats de second tour Macron - Le Pen entre 2017 et 2022"),
                switchInput(
                  inputId = "carte_2017ou2022",
                  onLabel = "2017",
                  offLabel = "2022"
                ),
                box(imageOutput("carte_2017ou2022_aff"),
                    title = "Score par commune pour Emmanuel Macron",
                    footer = "Rouge => Macron gagnant.  Bleu => Le Pen gagnante",
                    width = 6),
                box(imageOutput("carte_evo_17_22"),
                    title = "Différence de score des seconds tours 2017 et 2022 pour Marine Le Pen",
                    footer = "Bleu => meilleur score pour Le Pen en 2022 que en 2017. Rouge => moins bon score",
                    width = 6),
                box("Nous observons un gain de score dans presque toutes les communes métropolitaines pour Marine Le Pen. 
                    Ce gain est plus élevé dans les communes plus rurales comme le centre Bretagne le centre de la France ou le sud ouest."),
                valueBox(
                  value = "9143",
                  subtitle = "Nombre de communes perdues en 2017 mais remportées en 2022 par Marine Le Pen.",
                  color = "red",
                  width = 6
                )
        ),
        tabItem("stat_ACP",  # Page presentant notre ACP
                title = "ACP sur les résultats",
                h2("Analyse en Composantes Principale des communes"),
                box(width = 12,
                    "Le but de cette partie est de comprendre d’où viennent les disparités existantes entre les
                    communes. Le fonctionnement de l’Analyse en Composantes Principales (ACP) est relativement
                    simple, son but est de trouver une nouvelle base de représentation des données dans laquelle l’inertie expliquée (la variance)
                    par les premiers axes est la plus forte. Nous obtenons donc une nouvelle représentation des
                    données qui concentre l’information sur les premiers axes.
                    Vous avez ici la représentation des communes sur les deux premiers axes de l'ACP, concentrants 50% de la variance totale."
                    ),
                switchInput(
                  inputId = "switch_1er_ou_2nd_t",
                  onLabel = "Coloration en fonction des résultats du 1er tour",
                  offLabel = "Coloration en fonction des résultats du 2nd tour",
                  width = "auto"
                ),
                box(imageOutput("ACP_img"),
                    title = "Résultats de l'ACP: Communes coloriées par le candidat majoritaire",
                    width = 6
                    ),
                box(imageOutput("ACP_construction"),
                    title = "Corrélations variables/nouveaux axes",
                    footer = "Plus la corrélation est évlevée, plus la variables a participé à la construction de l'axe",
                    width = 6
                    ),
                  "
                *CSP3 : Nombre de cadres et professions intellectuelles supérieures actifs de 15 à 64 ans en 2017  //
                *CSP6 : Nombre d'ouvriers actifs de 15 à 64 ans en 2017  //
                *%_appartements : Pourcentage d'appartements présents dans la commune //
                *%_av_1919 : Pourcentage de logement construits avant 1919 //
                *%_a_2006 : Pourcentage de logement construits après 2006 //
                *%RFR_2019 : Révenu médian déclaré dans la commune
              "
        ),
        tabItem("stat_reg_log",  # Page presentant l'analyse a l'aide de la regression logistique
                title = "Analyse statistique à l'aide de la régression Logistique",
                h2("Analyse statistique à l'aide de la régression Logistique"),
                box(
                  width = 9,
                  "Pour comprendre et analyser les variables qui contribuaient le plus à la victoire d'un candidat dans une commune lors du 1er tour,
                  nous avons utilisé une régression logistique. En réalisant des analyses de variances, nous avons conservé les variables les plus significatives."
                ),
                box(width = 3,
                    selectInput("candidat_reg", "Candidat:",
                                choices=c("Emmanuel Macron", "Marine Le Pen", "Jean-Luc Mélenchon"))),
                box(
                  width = 9,
                  "Les odds ratios sont simplement les exponentielles des coefficients. Un odds
                  ratio proche de 1 signifie peu d’effet, un odds ratio largement supérieur à 1 correspond à une
                  augmentation du phénomène étudié et un odds ratio largement inférieur à 1 correspond à une
                  diminution du phénomène étudié. Cependant un odds ratio proche de 1 ne signifie pas que 
                  la variable n'explique pas le résulat. Elle peut en effet être corrélée avec des variables qui
                  explique le phénomène étudié."
                  ),
                valueBox(
                  value = textOutput("precision_modele"),
                  subtitle = "Précision du modèle (validation croisée à 10 plis)",
                  color = "red",
                  width = 3
                ),
              box(imageOutput("odds_ratio"),
                  title = "Odds ratios des variables les plus significatives",
                  width = 4,
                  height = "350"),
              box(imageOutput("graph1"),
                  title = "Probabilité de victoire du candidat en fonction de la variable",
                  width = 4,
                  height = "350"),
              box(imageOutput("graph2"),
                  title = "Probabilité de victoire du candidat en fonction de la variable",
                  width = 4,
                  height = "350"),
              
                "*CSP1 : Nombre d'agriculteurs exploitants actifs de 15 à 64 ans en 2017  //
                *CSP2 : Nombre d'artisans, commerçants, chefs d'entreprise actifs de 15 à 64 ans en 2017  //
                *CSP3 : Nombre de cadres et professions intellectuelles supérieures actifs de 15 à 64 ans en 2017  //
                *CSP4 : Nombre de professions intermédiaires actifs de 15 à 64 ans en 2017  //
                *CSP5 : Nombre d'employés actifs de 15 à 64 ans en 2017  //
                *CSP6 : Nombre d'ouvriers actifs de 15 à 64 ans en 2017  //
              "
    )
))

shinyApp(
    ui = dashboardPage(header,
                       sidebar,
                       body,
                       title = "Elections présidentielles 2022",
                       skin = "red"
                       ),
    
    server = function(input, output) { # Ici tout l'affichage des graphiques et des cartes interactives
      
        output$logoUTC <- renderImage({
          return(list(src = "figures/logo_UTC.png",
                      width = "300",
                      height = "auto",
                      contentType = "image/png",
                      alt = "logo_utc"))
        }, deleteFile = FALSE)
      
        output$score_cand_avec_abstention_ <- renderText({paste(round(resultats1erT[resultats1erT$candidat == input$candidat_1er_t_resultats_name,]$p_voix_France[1],1), "%")})
        output$score_cand_avec_abstention <- renderText({paste(score_candidats[score_candidats$candidat == input$candidat_1er_t_candidats,]$score_avec_abstention, "%")})
        output$score_cand_sans_abstention <- renderText({paste(score_candidats[score_candidats$candidat == input$candidat_1er_t_candidats,]$score_sans_abstention, "%")})
        
        output$candidat_centile_commune <- renderPlot({
            # Variables dispo : c("Prix du mètre carré", "Taux de chômage", "Population de la commune", "Revenu médian"))
            if(input$variable_1er_t_candidats == "Revenu médian"){
                ggplot(
                    data=centile_rfr_candidat[candidat == input$candidat_1er_t_candidats,],
                    aes(x=centile_num, y=prct_vote, fill=-prct_vote)) +
                    geom_bar(stat="identity") +
                    ggtitle("Score du candidat selon le centile de revenu médian des communes") +
                    xlab("Communes réparties en centile de revenu médian") +
                    ylab("Score en %") +
                    theme(
                        legend.position="none",
                        plot.title = element_text(size=16, face='bold', hjust=0.5),
                        plot.subtitle = element_text(color = "grey40", size=12),
                        plot.caption = element_text(color = "grey40", face = "italic", size=12)
                    )            }
            else if(input$variable_1er_t_candidats == "Prix du mètre carré"){
                ggplot(
                    data=centile_prixm2_candidat[candidat == input$candidat_1er_t_candidats,],
                    aes(x=centile_num, y=prct_vote, fill=-prct_vote)) +
                    geom_bar(stat="identity") +
                    ggtitle("Score du candidat selon le centile du prix au mètre carré des communes") +
                    xlab("Communes réparties en centile de prix au mètre carré") +
                    ylab("Score en %") +
                    theme(
                        legend.position="none",
                        plot.title = element_text(size=16, face='bold', hjust=0.5),
                        plot.subtitle = element_text(color = "grey40", size=12),
                        plot.caption = element_text(color = "grey40", face = "italic", size=12)
                    )            }
            else if(input$variable_1er_t_candidats == "Taux de chômage"){
                ggplot(
                    data=centile_chomage_candidat[candidat == input$candidat_1er_t_candidats,],
                    aes(x=centile_num, y=prct_vote, fill=-prct_vote)) +
                    geom_bar(stat="identity") +
                    ggtitle("Score du candidat selon le centile du taux de chômage des communes") +
                    xlab("Communes réparties en centile du taux de chômage") +
                    ylab("Score en %") +
                    theme(
                        legend.position="none",
                        plot.title = element_text(size=16, face='bold', hjust=0.5),
                        plot.subtitle = element_text(color = "grey40", size=12),
                        plot.caption = element_text(color = "grey40", face = "italic", size=12)
                    )            }
            else if(input$variable_1er_t_candidats == "Population de la commune"){
                ggplot(
                    data=centile_popu_candidat[candidat == input$candidat_1er_t_candidats,],
                    aes(x=centile_num, y=prct_vote, fill=-prct_vote)) +
                    geom_bar(stat="identity") +
                    ggtitle("Score du candidat selon le centile de population des communes") +
                    xlab("Communes réparties en centile de population") +
                    ylab("Score en %") +
                    theme(
                        legend.position="none",
                        plot.title = element_text(size=16, face='bold', hjust=0.5),
                        plot.subtitle = element_text(color = "grey40", size=12),
                        plot.caption = element_text(color = "grey40", face = "italic", size=12)
                    )            }
            else if(input$variable_1er_t_candidats == "CSP+"){
                ggplot(
                    data=centile_csp_plus_candidat[candidat == input$candidat_1er_t_candidats,],
                    aes(x=centile_num, y=prct_vote, fill=-prct_vote)) +
                    geom_bar(stat="identity") +
                    ggtitle("Score du candidat selon le centile du pourcentage de CSP+ présents dans les communes") +
                    xlab("Communes réparties en centile CSP+ présents dans la commune") +
                    ylab("Score en %") +
                    theme(
                        legend.position="none",
                        plot.title = element_text(size=16, face='bold', hjust=0.5),
                        plot.subtitle = element_text(color = "grey40", size=12),
                        plot.caption = element_text(color = "grey40", face = "italic", size=12)
                    )            }
            else if(input$variable_1er_t_candidats == "CSP-"){
                ggplot(
                    data=centile_csp_moins_candidat[candidat == input$candidat_1er_t_candidats,],
                    aes(x=centile_num, y=prct_vote, fill=-prct_vote)) +
                    geom_bar(stat="identity") +
                    ggtitle(label = "Score du candidat selon le centile de pourcentage de CSP- par commune") +
                    xlab("Communes réparties en centile CSP- présents dans la commune") +
                    ylab("Score en %") +
                theme(
                  legend.position="none",
                  plot.title = element_text(size=16, face='bold', hjust=0.5),
                  plot.subtitle = element_text(color = "grey40", size=12),
                  plot.caption = element_text(color = "grey40", face = "italic", size=12)
                )            }
        })
        
        output$carte_france_2t <- renderImage({
          return(list(src = "figures/second_tour_2022.png",
                      width = "100%",
                      height = "auto",
                      contentType = "image/png",
                      alt = "Alignment"))
        }, deleteFile = FALSE)
        
        output$carte_france_png_res_1t <- renderImage({
          name = input$candidat_1er_t_resultats_name
          name = str_replace_all(name," ","_")
          name = str_replace_all(name,"-","_")
          name = str_replace_all(name,"é","e")
          name = str_replace_all(name,"É","E")
          
          return(list(src = paste("figures/",name,".png", sep=""),
                      width = "100%",
                      height = "auto",
                      contentType = "image/png",
                      alt = "Alignment"))
          
        }, deleteFile = FALSE)
        
        observeEvent(input$carte_2017ou2022, {
          if (input$carte_2017ou2022) {
            output$carte_2017ou2022_aff <- renderImage({
              return(list(src = "figures/second_tour_2017.png",
                          width = "100%",
                          height = "auto",
                          contentType = "image/png",
                          alt = "Alignment"))
            }, deleteFile = FALSE) 
          } else {
            output$carte_2017ou2022_aff <- renderImage({
              return(list(src = "figures/second_tour_2022.png",
                          width = "100%",
                          height = "auto",
                          contentType = "image/png",
                          alt = "Alignment"))
            }, deleteFile = FALSE)                       
          }
          
        })

        
        output$carte_evo_17_22 <- renderImage({
          return(list(src = "figures/dif.png",
                      width = "100%",
                      height = "auto",
                      contentType = "image/png",
                      alt = "Alignment"))
        }, deleteFile = FALSE)
        
        output$leaflet_region <- renderLeaflet({
          name = input$candidat_1er_t_resultats_name # Candidat selectionne
          reg = input$candidat_1er_t_resultats_reg # Region selectionnee
          
          communes <- merge(communes, resultats1erT[candidat == name], by.x = "insee", by.y = "code_commune", all = FALSE)
          communes$rapport_commune_France_m = communes$rapport_commune_France
          communes$rapport_commune_France_m[communes$rapport_commune_France_m > 2] = 2
          communes <- communes[communes$region_name == reg,]
          
          pal <- colorQuantile(c("blue","white","red"), c(0,2), n = 100)
          
          leaflet(communes) %>%
            addPolygons( fillColor = ~pal(rapport_commune_France_m),
                         stroke = FALSE,
                         fillOpacity = 0.95,
                         smoothFactor = 0.3,
                         label = ~paste0("Commune : ",communes$libelle_commune, ", score : ", communes$p_voix),
                         labelOptions = labelOptions(noHide = F, textsize = "10px")) %>%
            addLegend(title = "Ratio", pal = colorNumeric(c("blue","white","red"), c(0,2)),
            values = communes$rapport_commune_France_m) %>%
            addTiles()
        })
        
        output$leaflet_region_2ndT <- renderLeaflet({
          reg = input$candidat_2nd_t_resultats_reg # Region selectionnee
          
          communes_selec <- communes[!is.na(communes$region_name) & (communes$region_name == reg),]
          communes_selec <- merge(communes_selec, resultats2ndT[, c("code_commune", "libelle_commune", "p_LePen", "p_Macron", "Vainqueur")], by.x = "insee", by.y = "code_commune", all = FALSE)
          communes_selec = communes_selec[!is.na(communes_selec$p_Macron),]
          communes_selec$p_macron_corrige = communes_selec$p_Macron
          communes_selec$p_macron_corrige[communes_selec$p_macron_corrige > 75] = 75
          communes_selec$p_macron_corrige[communes_selec$p_macron_corrige < 25] = 25
          
          
          pal <- colorQuantile(c("blue","white","red"), c(25, 75), n = 100)
          
          leaflet(communes_selec) %>%
            addPolygons( fillColor = ~pal(p_macron_corrige),
                         stroke = FALSE,
                         fillOpacity = 0.95,
                         smoothFactor = 0.3,
                         label = ~paste0("Commune : ", communes_selec$libelle_commune, ", score Macron: ", round(communes_selec$p_Macron, 1)),
                         labelOptions = labelOptions(noHide = F, textsize = "10px")) %>%
            addLegend(title = "Score Macron", pal = colorNumeric(c("blue","white","red"), c(25, 75)),
                      values = communes_selec$p_macron_corrige) %>%
            addTiles()
        })
        
        output$odds_ratio <- renderImage({
          cand = input$candidat_reg
          
          if (cand == "Jean-Luc Mélenchon"){
            return(list(src = "figures/MEL_ODDS.png",
                      width = "100%",
                      height = "auto",
                      contentType = "image/png",
                      alt = "Alignment"))
          }
          if (cand == "Emmanuel Macron"){
            return(list(src = "figures/MAC_ODDS.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
          if (cand == "Marine Le Pen"){
            return(list(src = "figures/LEP_ODDS.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
        }, deleteFile = FALSE)
        
        output$graph1 <- renderImage({
          cand = input$candidat_reg
          
          if (cand == "Jean-Luc Mélenchon"){
            return(list(src = "figures/MEL_HLM.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
          if (cand == "Emmanuel Macron"){
            return(list(src = "figures/MAC_RFR.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
          if (cand == "Marine Le Pen"){
            return(list(src = "figures/LEP_RFR.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
        }, deleteFile = FALSE)
        
        output$graph2 <- renderImage({
          cand = input$candidat_reg
          if (cand == "Jean-Luc Mélenchon"){
            return(list(src = "figures/MEL_CS3.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
          if (cand == "Emmanuel Macron"){
            return(list(src = "figures/MAC_chom.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
          if (cand == "Marine Le Pen"){
            return(list(src = "figures/LEP_CHOM.png",
                        width = "100%",
                        height = "auto",
                        contentType = "image/png",
                        alt = "Alignment"))
          }
        }, deleteFile = FALSE)
        
        output$precision_modele <- renderText({
          cand = input$candidat_reg
          if (cand == "Marine Le Pen" ){return("70.64%")}
          if (cand == "Emmanuel Macron" ){return("78.30%")}
          if (cand == "Jean-Luc Mélenchon" ){return("93.90%")}
        })
        
        output$ACP_construction <- renderImage({
          return(list(src = "figures/corr_axes_var.png",
                      width = "77%",
                      height = "auto",
                      contentType = "image/png",
                      alt = "Alignment"))
        }, deleteFile = FALSE)
        
        observeEvent(input$switch_1er_ou_2nd_t, {
          if (input$switch_1er_ou_2nd_t) {
            output$ACP_img <- renderImage({
              return(list(src = "figures/ACP_1er_t_2022.png",
                          width = "100%",
                          height = "auto",
                          contentType = "image/png",
                          alt = "Alignment"))
            }, deleteFile = FALSE) 
          } else {
            output$ACP_img <- renderImage({
              return(list(src = "figures/ACP_2nd_t_2022.png",
                          width = "100%",
                          height = "auto",
                          contentType = "image/png",
                          alt = "Alignment"))
            }, deleteFile = FALSE)                       
          }
          
        })
    }
)
