library(FactoMineR)
library(factoextra)
library(data.table)
library(corrplot)
library(ggplot2)
library(dplyr)

# chargement des données --------------------------------------------------

emploi_2017 <- readRDS("../../DATA/emploi_2017.rds")
logement_2017_2_relatif <- readRDS("../../DATA/logement_2017_2_relatif.rds")
prix_m2_commune <- readRDS("../../DATA/prix_m2_commune.rds")

resultats_t2_2017 <- readRDS("../../DATA/resultats2_2017.rds")
resultats_t1_2022 <- readRDS("../../DATA/resultats_t1_P2022.rds")
resultats_t2_2022 = readRDS("../../DATA/resultats_T2_2022.rds")

rfr_commune <- readRDS("../../DATA/rfr_2019_commune.rds")
popu_commune <- readRDS("../../DATA/popu_commune_2019.rds")

# Pre-traitement --------------------------------------------------
res_t1_2022_unmelted = resultats_t1_2022
setDT(res_t1_2022_unmelted)
res_t1_2022_unmelted = dcast(data = res_t1_2022_unmelted, formula = code_commune~candidat, fun.aggregate = sum, value.var = "voix")
setDF(res_t1_2022_unmelted)
res_t1_2022_unmelted[,setdiff(names(res_t1_2022_unmelted), c("code_commune", 'abstentions', "nuls", "blancs"))] = res_t1_2022_unmelted[,setdiff(names(res_t1_2022_unmelted), c("code_commune", 'abstentions', "nuls", "blancs"))]/rowSums(res_t1_2022_unmelted[,setdiff(names(res_t1_2022_unmelted), c("code_commune", 'abstentions', "nuls", "blancs"))])


resultats_t2_2017 <- rename(resultats_t2_2017, code_commune = code)
logement_2017_2_relatif <- rename(logement_2017_2_relatif, code_commune = code_com)

dataset <- merge(resultats_t2_2017, emploi_2017, by = "code_commune", all.x = TRUE)
dataset <- merge(dataset, prix_m2_commune, by = "code_commune", all.x = TRUE)
dataset <- merge(dataset, logement_2017_2_relatif, by = "code_commune", all.x = TRUE)
dataset <- merge(dataset, rfr_commune, by = "code_commune", all.x = TRUE)
dataset <- merge(dataset, popu_commune, by = "code_commune", all.x = TRUE)
dataset <- merge(dataset, res_t1_2022_unmelted, by = "code_commune", all.x = TRUE)

row.names(dataset) = dataset$code_commune

dataset$p_blancs <- 100 * dataset$Blancs / dataset$Inscrits
dataset$p_abstentions <- 100 * dataset$Abstentions / dataset$Inscrits
dataset$p_nuls <- 100 * dataset$Nuls / dataset$Inscrits

# corrélations ------------------------------------------------------------
names(dataset)
variables_acp = c("p_chom_15_64",
                  "p_CS3_15_64",
                  "p_CS6_15_64",
                  "prix_moyen_m2",
                  "%_av_1919",
                  "%_ap_2006", 
                  "%_locataires_HLM",
                  "%_proprios",
                  "%_res_secondaires",
                  "%_appartements",
                  "RFR_2019",
                  "popu")



for(var in variables_acp){
  dataset = dataset[!is.na(dataset[var]),]
}

for(var in c("Emmanuel MACRON")){
  dataset = dataset[!is.na(dataset[var]),]
}

mcor <- cor(dataset[, variables_acp], use = "complete.obs")
melted_data <- melt(mcor)

melted_data$value <- round(melted_data$value, 2)

ggplot(data = melted_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +  geom_text(aes(label = value), color = "white", size = 2) +
  theme(axis.text.x = element_text(angle = 90)) # Heatmaap des corrélations entre variables


# ACP ---------------------------------------------------------------------
res.pca <- PCA(dataset[, variables_acp],
               scale.unit = TRUE,
               ncp = 10,
               graph = FALSE
               )


# visualisations ----------------------------------------------------------

fviz_eig(res.pca) # valeurs propres

corrplot(res.pca$var$contrib, is.corr=FALSE) # Heatmap des correlations variables/nouveaux axes

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) # Contribution des variable à l'axe 1

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) # Contribution des variable à l'axe 2

fviz_contrib(res.pca, choice = "var", axes = 3, top = 10) # Contribution des variable à l'axe 3

fviz_contrib(res.pca, choice = "var", axes = 4, top = 10) # Contribution des variable à l'axe 4

# Coloration avec les resultats du premier tour (sans prendre en compte l'abstension)
dataset$gagnant_hors_abs = apply(dataset[, setdiff(names(res_t1_2022_unmelted), c("code_commune", 'abstentions', "nuls", "blancs"))], 1, which.max)
dataset$gagnant_hors_abs = setdiff(names(res_t1_2022_unmelted), c("code_commune", 'abstentions', "nuls", "blancs"))[dataset$gagnant_hors_abs]

fviz_pca_ind(res.pca, col.ind=dataset$gagnant_hors_abs, axes = c(1, 2), geom.ind = c("point")) +
  geom_point(aes(shape = dataset$gagnant_hors_abs, color = dataset$gagnant_hors_abs), size=0.01) +
  scale_shape_manual(values=seq(15,24)) + 
  scale_color_manual(values=c("#404040", "#ffeb00", "#0D378A", "#dd0000", "#cc6666", "#cc2443", "#8040C0", "#dddddd", "#0066cc"))

# Coloration avec les resultats du second tour
dataset = merge(dataset, resultats_t2_2022[, c("code_commune", "Vainqueur")], by = "code_commune", all.x = TRUE)

fviz_pca_ind(res.pca, col.ind=dataset$Vainqueur, axes = c(1, 2), geom.ind = c("point")) +
  geom_point(aes(shape = dataset$Vainqueur, color = dataset$Vainqueur), size=0.01) +
  scale_shape_manual(values=seq(15,24)) + 
  scale_color_manual(values=c("#404040", "#ffeb00", "#0D378A", "#dd0000", "#cc6666", "#cc2443", "#8040C0", "#dddddd", "#0066cc"))


