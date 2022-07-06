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

communes <- readOGR(dsn = "C:/Users/brieu/Desktop/Cours/TX/DONNEES/communes-20150101-100m-shp", layer = "communes-20150101-100m", GDAL1_integer64_policy = TRUE,encoding = "UTF-8", use_iconv = TRUE,verbose=FALSE)

communes <- spTransform(communes, CRS("+init=epsg:4326"))

communes <- merge(communes, Resultats, by.x = "insee", by.y = "code_commune", all = FALSE)


communes$p_Macron <- round(communes$p_Macron, 1)

communes_2022 = communes


# 2017 --------------------------------------------------------------------

Presidentielle_2017_Resultats_Communes_Tour_2 <- read_delim("C:/Users/brieu/Desktop/Cours/TX/DONNEES/Presidentielles 2017/Presidentielle_2017_Resultats_Communes_Tour_2_c.csv", 
                                                            delim = ";", escape_double = FALSE, col_types = cols(`Code du département` = col_character(), 
                                                                                                                 `Code de la commune` = col_character(), 
                                                                                                                 Sexe...27 = col_character()), trim_ws = TRUE)
Resultats <- Presidentielle_2017_Resultats_Communes_Tour_2[,c("Code de la commune","Voix...30","Voix...23","Code du département", "Libellé de la commune")]

Resultats <- rename(Resultats, LePen = Voix...30, Macron = Voix...23, code_commune = `Code de la commune`, code_dep = `Code du département`, nom_commune = `Libellé de la commune`)


Resultats$p_LePen = 100 * Resultats$LePen / (Resultats$LePen + Resultats$Macron)

Resultats$p_Macron = 100 * Resultats$Macron / (Resultats$LePen + Resultats$Macron)

Resultats$code_commune = str_pad(Resultats$code_commune,3, "left", pad = "0")

Resultats$code_dep = str_pad(Resultats$code_dep,2, "left", pad = "0")

Resultats$code = paste(Resultats$code_dep,Resultats$code_commune, sep = "")

communes <- readOGR(dsn = "C:/Users/brieu/Desktop/Cours/TX/DONNEES/communes-20150101-100m-shp", layer = "communes-20150101-100m", GDAL1_integer64_policy = TRUE,encoding = "UTF-8", use_iconv = TRUE,verbose=FALSE)

communes <- spTransform(communes, CRS("+init=epsg:4326"))

communes <- merge(communes, Resultats, by.x = "insee", by.y = "code", all = FALSE)

communes$p_Macron <- round(communes$p_Macron, 1)


communes_2017 = communes



communes_2017$p_Macron_2017 = communes_2017$p_Macron
communes <- merge(communes_2022, communes_2017[,c("insee", "p_Macron_2017")], by = "insee", all.x = T)
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
              addLegend(pal = colorNumeric(c("blue","white","red"), c(-20,20)), values = c(-20,20), title = "Différence") %>%

  # addLegend(pal = pal, values = communes$p_Macron, title = "Pourcentage vote Macron") %>%
  addTiles()
carte
saveWidget(carte, "dif.html", selfcontained = FALSE)
webshot("dif.html", file = "dif.png",
        cliprect = "viewport")
