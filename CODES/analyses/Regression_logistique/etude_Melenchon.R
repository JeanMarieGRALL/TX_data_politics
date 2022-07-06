library(FactoMineR)
library(factoextra)
library(data.table)
library(corrplot)
library(ggplot2)
library(dplyr)
library(shiny)
library(FactoInvestigate)
library(Factoshiny)
library(candisc)
library(questionr)
library(MASS)

# chargement des données --------------------------------------------------
delits_21 <- readRDS("../../../DATA/delits_21.rds")
delits_21 = delits_21[,.(nb_delits = sum(nb)),"code_commune"]
emploi_2017 <- readRDS("../../../DATA/emploi_2017.rds")
logement_2017_2_relatif <- readRDS("../../../DATA/logement_2017_2_relatif.rds")
prix_m2_commune <- readRDS("../../../DATA/prix_m2_commune.rds")
resultats_t2_2017 <- readRDS("../../../DATA/resultats2_2017.rds")
resultats_t1_2022 <- readRDS("../../../DATA/resultats_t1_P2022.rds")
rfr_commune <- readRDS("../../../DATA/rfr_2019_commune.rds")
popu_commune <- readRDS("../../../DATA/popu_commune_2019.rds")
resultats_t2_2022 <- readRDS("../../../DATA/resultats_T2_2022.rds")

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
dataset <- merge(dataset, delits_21, by = "code_commune", all.x = TRUE)
dataset <- merge(dataset, resultats_t2_2022[,c("code_commune","Vainqueur")], by = "code_commune", all.x = TRUE)
dataset <- dataset[!is.na(dataset$popu),]
dataset <- dataset[dataset$popu >= 500,]


row.names(dataset) = dataset$code_commune

dataset$p_blancs <- 100 * dataset$Blancs / dataset$Inscrits
dataset$p_abstentions <- 100 * dataset$Abstentions / dataset$Inscrits
dataset$p_nuls <- 100 * dataset$Nuls / dataset$Inscrits

dataset$nb_delits_p_1000 = round(100 * (dataset$nb_delits / dataset$popu),1)


rm(list = setdiff(ls(),"dataset"))
gc()

names(dataset)

candidats = c("Anne HIDALGO", "Emmanuel MACRON", "Fabien ROUSSEL", "Jean LASSALLE", "Jean-Luc MÉLENCHON", "Marine LE PEN", "Nathalie ARTHAUD","Nicolas DUPONT-AIGNAN", "Philippe POUTOU", "Valérie PÉCRESSE", "Yannick JADOT", "Éric ZEMMOUR" )


names(dataset)
                                                                                                
variables = c("p_chom_15_64",
              "p_CS1_15_64",
              "p_CS2_15_64",
              "p_CS3_15_64",
              "p_CS4_15_64",
              "p_CS5_15_64",
              "p_CS6_15_64",
              "prix_moyen_m2",
              "%_av_1919",
              "%_1919_1945",
              "%_1946_1970",
              "%_1971_1990",
              "%_1991_2005",
              "%_ap_2006", 
              "%_locataires_HLM",
              "%_proprios",
              "%_res_secondaires",
              "%_appartements",
              "%_log_inf_30m2",
              "%_log_120_supm2",
              "RFR_2019",
              "popu",
              "nb_delits_p_1000",
              "Jean-Luc MÉLENCHON",
              "p_blancs",
              "p_abstentions"
)
for(var in variables){
  dataset = dataset[!is.na(dataset[var]) & dataset[var] != "NaN",]
}
dataset = dataset[,variables]


dataset = arrange(dataset, -`Jean-Luc MÉLENCHON` )

dataset$MEL <- FALSE
dataset$MEL[dataset$`Jean-Luc MÉLENCHON` >= 0.25] <- TRUE


rm(list = setdiff(ls(),"dataset"))
gc()


# regression --------------------------------------------------------------

library(GGally)
library(questionr)
library(broom.helpers)
library(effects)
library(ggeffects)
library(car)

freq(dataset$MEL)

dataset <- rename(dataset,
                  p_ap_2006 = "%_ap_2006",
                  p_av_1919 = "%_av_1919" ,
                  p_1919_1945 = "%_1919_1945",
                  p_1946_1970 = "%_1946_1970",
                  p_1971_1990 = "%_1971_1990",
                  p_1991_2005 = "%_1991_2005",
                  p_locataires_HLM = "%_locataires_HLM",
                  p_proprios = "%_proprios",
                  p_res_secondaires = "%_res_secondaires",
                  p_appartements = "%_appartements",
                  p_log_inf_30m2 = "%_log_inf_30m2",
                  p_log_120_supm2 = "%_log_120_supm2")

names(dataset)

reg <- glm(MEL ~ p_chom_15_64 + p_CS3_15_64 + p_CS6_15_64 +
             p_CS4_15_64 +
             prix_moyen_m2 + RFR_2019  +
             p_locataires_HLM  + p_res_secondaires +
             popu + p_av_1919 + p_1919_1945 +
             p_blancs,
           data = dataset,
           family = binomial(logit))

# summary(reg)
# 
# exp(cbind(coef(reg), confint(reg)))
# 
# odds.ratio(reg)
# 
#ggcoef_model(reg, exponentiate = TRUE)
# # 
# # plot(allEffects(reg))
# # 
 cowplot::plot_grid(plotlist = plot(ggeffect(reg)))
# 
# MEL.pred <- predict(reg, type = "response", newdata = dataset)
# 
# table(MEL.pred > 0.5, dataset$MEL)
# 
# 
# 
# drop1(reg, test = "Chisq")

# Anova(reg)
# 
plot(ggeffect(reg, "p_locataires_HLM"))
dataset$code_commune = row.names(dataset)
dataset = dataset[sample(1:nrow(dataset)), ]
z=0
for (k in c(1:10)){
  A = dataset[((k-1)*1550):((k)*1550),]
  A$p_A = 1
  B = merge(dataset, A[,c("code_commune", "p_A")], by = "code_commune", all.x = T)
  B = B[is.na(B$p_A),]
  B$p_A = NULL
  A$p_A = NULL
  reg <- glm(MEL ~ p_chom_15_64 + p_CS3_15_64 + p_CS6_15_64 +
               p_CS4_15_64 +
               prix_moyen_m2 + RFR_2019  +
               p_locataires_HLM  + p_res_secondaires +
               popu + p_av_1919 + p_1919_1945 +
               p_blancs,
             data = B,
             family = binomial(logit))
  LEP.pred <- predict(reg, type = "response", newdata = A)
  X = table(LEP.pred > 0.5 , A$`Jean-Luc MÉLENCHON` >= 0.25)
  print(100*(X[1] + X[4]) /(X[2] + X[3] + X[1] + X[4]))
  z = z +  (100*(X[1] + X[4]) /(X[2] + X[3] + X[1] + X[4]))
  
}
z /10
