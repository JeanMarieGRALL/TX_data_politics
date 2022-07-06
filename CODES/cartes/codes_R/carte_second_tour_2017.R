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
saveRDS(communes, "C:/Users/brieu/Desktop/Cours/TX/GIT_TX_P22/DATA/Carte_2_2017.rds")
communes$p_Macron[communes$p_Macron > 75] = 75
communes$p_Macron[communes$p_Macron < 25] = 25
pal <- colorQuantile(c("blue","white","red"), c(25,75), n = 100)

carte = leaflet(communes) %>%
  addPolygons( fillColor = ~pal(p_Macron),
               stroke = FALSE,
               fillOpacity = 0.8,
               smoothFactor = 0.3,
               label = ~paste0("vote Macron : ", communes$p_Macron, "%, commune : ", communes$libelle_commune),
               labelOptions = labelOptions(noHide = F, textsize = "10px")) %>%
  addLegend(pal = colorNumeric(c("blue","white","red"), c(25,75)), values = c(25,75), title = "Score Macron") %>%
  # addLegend(pal = pal, values = communes$p_Macron, title = "Pourcentage vote Macron") %>%
  addTiles()

saveWidget(carte, "second_tour_2017.html", selfcontained = FALSE)
webshot("second_tour_2017.html", file = "second_tour_2017.png",
        cliprect = "viewport")
