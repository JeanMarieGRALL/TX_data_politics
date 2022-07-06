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

# ouverture du fichier des résultats du premier tour 2022 -----------------


Resultats <- readRDS("../../../DATA/resultats_t1_P2022.rds")


# ajout de la variable p_voix qui correspond au % de voix du candidat dans chaque commune --------


Resultats_2 <- Resultats[,.(nb_voix = sum(voix)),code_commune]

Resultats <- merge(Resultats, Resultats_2, by = "code_commune")

Resultats$p_voix <- round (100 * Resultats$voix / Resultats$nb_voix, 1)

rm(Resultats_2)


# ajout de la variable écart à la moyenne ---------
# qui correspond a la différence en % du % du candiadat
# sur toute la France et son résulat dans la commune
# les % prennent en compte l'abstention, votes nuls et blancs

nb_votes = sum(Resultats$voix)

p_candidats <- Resultats[,.( voix_France = sum(voix)), candidat]

Resultats <- merge(Resultats, p_candidats, by = "candidat", all.x = T)

Resultats$p_voix_France = 100 * Resultats$voix_France / nb_votes

Resultats$ecart_a_la_moy = round(Resultats$p_voix - Resultats$p_voix_France,2)

Resultats$rapport_commune_France = round(Resultats$p_voix / Resultats$p_voix_France,1)

rm(p_candidats)

Resultats = Resultats[,c("candidat","code_commune","libelle_commune","rapport_commune_France")]

saveRDS(Resultats, "../fichiers_RDS/resultats_1er_tour_2022.rds")

communes <- readOGR(dsn = "../../../../DONNEES/communes-20150101-100m-shp", layer = "communes-20150101-100m", GDAL1_integer64_policy = TRUE,encoding = "UTF-8", use_iconv = TRUE,verbose=FALSE)


communes <- spTransform(communes, CRS("+init=epsg:4326"))

saveRDS(communes, "../fichiers_RDS/communes_1er_tour_2022.rds")

communes <- merge(communes, Resultats[candidat == "Jean-Luc MÉLENCHON"], by.x = "insee", by.y = "code_commune", all = FALSE)
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
  #addLegend(pal = pal, values = communes$rapport_commune_France, title = "rapport_commune_France") %>%
  addTiles()

saveWidget(carte, "Melanchon.html", selfcontained = FALSE)
webshot("Melanchon.html", file = "Melanchon.png",
        cliprect = "viewport")
