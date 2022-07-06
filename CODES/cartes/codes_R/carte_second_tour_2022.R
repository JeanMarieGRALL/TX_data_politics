library(readr)
library(dplyr)
library(leaflet)
library(stringr)
library(data.table)
library(rgdal)

library(devtools)
library(leaflet)
library(htmlwidgets)
library(webshot)
library("mapview")

setwd(dir = "C:/Users/brieu/Desktop/Cours/TX/GIT_TX_P22/CODES/cartes/codes_R")

resultats_par_niveau_subcom_t2_france_entiere <- read_delim("C:/Users/brieu/Desktop/Cours/TX/DONNEES/presidentielles_2022/resultats-par-niveau-subcom-t2-france-entiere.csv", 
                                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)



Resultats <- resultats_par_niveau_subcom_t2_france_entiere[,c("Code du département","Code de la commune","Voix","...31", "Libellé de la commune", "Libellé du département")]

rm(resultats_par_niveau_subcom_t2_france_entiere)

Resultats <- rename(Resultats, 
                    Le_Pen = ...31, 
                    Macron = Voix,
                    code_commune = `Code de la commune`,
                    code_dep = `Code du département`,
                    libelle_commune = `Libellé de la commune`,
                    libelle_dep = `Libellé du département`
)

Resultats$code_commune = str_pad(Resultats$code_commune,3, "left", pad = "0")

Resultats$code_dep = str_pad(Resultats$code_dep,2, "left", pad = "0")

Resultats$code_commune = paste(Resultats$code_dep,Resultats$code_commune, sep = "")

Resultats$code_dep = NULL

Resultats$p_LePen = 100 * Resultats$Le_Pen / (Resultats$Le_Pen + Resultats$Macron)

Resultats$p_Macron = 100 * Resultats$Macron / (Resultats$Le_Pen + Resultats$Macron)

Resultats$Vainqueur = "Le_Pen"

Resultats$Vainqueur[Resultats$Macron > Resultats$Le_Pen] = "Macron"

saveRDS(Resultats, "../../../DATA/resulats_T2_2022.rds")
communes <- readOGR(dsn = "C:/Users/brieu/Desktop/Cours/TX/DONNEES/communes-20150101-100m-shp", layer = "communes-20150101-100m", GDAL1_integer64_policy = TRUE,encoding = "UTF-8", use_iconv = TRUE,verbose=FALSE)

communes <- spTransform(communes, CRS("+init=epsg:4326"))

communes <- merge(communes, Resultats, by.x = "insee", by.y = "code_commune", all = FALSE)


communes$p_Macron <- round(communes$p_Macron, 1)

# pal <- colorNumeric(c("blue","white","red"), communes$p_Macron)
# pal <- colorNumeric("magma", communes$p_Macron)
communes$p_Macron[communes$p_Macron > 75] = 75
communes$p_Macron[communes$p_Macron < 25] = 25
pal <- colorQuantile(c("blue","white","red"), c(25,75), n = 100)

carte <-  leaflet(communes) %>%
  addPolygons( fillColor = ~pal(p_Macron),
               stroke = FALSE,
               fillOpacity = 0.8,
               smoothFactor = 0.3,
               label = ~paste0("Poucentage de vote Macron : ", communes$p_Macron, ", Nom commune : ", communes$libelle_commune),
               labelOptions = labelOptions(noHide = F, textsize = "10px")) %>%
  addLegend(pal = colorNumeric(c("blue","white","red"), c(25,75)), values = c(25,75), title = "Score Macron") %>%
  # addLegend(pal = pal, values = communes$p_Macron, title = "Pourcentage vote Macron") %>%
  addTiles()

carte

saveWidget(carte, "second_tour_2022.html", selfcontained = FALSE)
webshot("second_tour_2022.html", file = "second_tour_2022.png",
        cliprect = "viewport")

communes <-  read_rds("../../DATA/Carte_2.rds")
communes_2017 <-  read_rds("../../DATA/Carte_2_2017.rds")
communes_2017$p_Macron_2017 = communes_2017$p_Macron
communes <- merge(communes, communes_2017[,c("insee", "p_Macron_2017")], by = "insee", all.x = T)
communes$p_Macron <- communes$p_Macron - communes$p_Macron_2017
communes$p_Macron[communes$p_Macron > 20] = 20
communes$p_Macron[communes$p_Macron < -20] = -20
pal <- colorQuantile(c("blue","white","red"), c(-20,20), n = 100)
carte = leaflet(communes) %>%
  addPolygons( fillColor = ~pal(p_Macron),
               stroke = FALSE,
               fillOpacity = 0.8,
               smoothFactor = 0.3,
               label = ~paste0("dif : ", communes$p_Macron, "%, commune : ", communes$libelle_commune),
               labelOptions = labelOptions(noHide = F, textsize = "10px")) %>%
  addLegend(pal = colorNumeric(c("blue","white","red"), c(0,100)), values = c(0,100)) %>%
  # addLegend(pal = pal, values = communes$p_Macron, title = "Pourcentage vote Macron") %>%
  addTiles()