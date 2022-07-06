
setwd(dir = "C:/Users/brieu/Desktop/Cours/TX/GIT_TX_P22/CODES/cartes/codes_R")
Resultats <- readRDS("../fichiers_RDS/resultats_1er_tour_2022.rds")
Resultats$candidat <- str_replace_all(Resultats$candidat," ","_")

for (nom in names(table(Resultats$candidat))){

nom1 = nom

if (nom == "Éric_ZEMMOUR") { nom1 = "Eric_ZEMMOUR"}
if (nom == "Valérie_PÉCRESSE") { nom1 = "Valerie_PECRESSE"}
if (nom == "Jean-Luc_MÉLENCHON") { nom1 = "Jean_Luc_MELENCHON"}
  
  
communes <- readRDS("../fichiers_RDS/communes_1er_tour_2022.rds")
  
Resultats <- readRDS("../fichiers_RDS/resultats_1er_tour_2022.rds")
  
Resultats$candidat <- str_replace_all(Resultats$candidat," ","_")

communes <- merge(communes, Resultats[candidat == nom], by.x = "insee", by.y = "code_commune", all = FALSE)
communes$rapport_commune_France_m = communes$rapport_commune_France
communes$rapport_commune_France_m[communes$rapport_commune_France_m > 2] = 2


pal <- colorQuantile(c("blue","white","red"), c(0,2), n = 100)

carte <-  leaflet(communes) %>%
  addPolygons( fillColor = ~pal(rapport_commune_France_m),
               stroke = FALSE,
               fillOpacity = 0.8,
               smoothFactor = 0.3,
               label = ~paste0("rapport : ",communes$rapport_commune_France, ", commune : ", communes$libelle_commune),
               labelOptions = labelOptions(noHide = F, textsize = "10px")) %>%
  addLegend(pal = colorNumeric(c("blue","white","red"), c(0,2)), values = c(0,2)) %>%
  #addLegend(pal = pal, values = communes$rapport_commune_France, title = "rapport_commune_France") %>%
  addTiles()

saveWidget(carte, paste(nom1,".html", sep = ""), selfcontained = FALSE)
webshot(paste(nom1,".html", sep = ""), file = paste("../images_cartes/",nom1,".png",sep=""),
        cliprect = "viewport")
rm(list = ls())
gc()
}