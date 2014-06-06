### Création BDD pour SIG ###


require(ggplot2)
require(dplyr)
#require(reshape2)

source("Scripts/functions.R")

full_BDD <- BDD #creation BDD à manipuler

full_BDD$stations_groupees <- full_BDD$station # copie de la colonne "station"

full_BDD$stations_groupees <- gsub("Crique_Nouvelle_France_4", "Crique_Nouvelle_France_contaminee",
                                 full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Nouvelle_France_5", "Crique_Nouvelle_France_contaminee",
                                 full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Nouvelle_France_1", "Crique_Nouvelle_France_non_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Nouvelle_France_2", "Crique_Nouvelle_France_non_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Nouvelle_France_3", "Crique_Nouvelle_France_non_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Nouvelle_France_6", "Crique_Nouvelle_France_non_contaminee",
                                   full_BDD$stations_groupees)

full_BDD$stations_groupees <- gsub("Crique_Chien_3", "Crique_Chien_non_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Chien_4", "Crique_Chien_non_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Chien_7", "Crique_Chien_non_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Chien_2", "Crique_Chien_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Chien_1", "Crique_Chien_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Chien_5", "Crique_Chien_contaminee",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Chien_8", "Crique_Chien_contaminee",
                                   full_BDD$stations_groupees)

full_BDD$stations_groupees <- gsub("Crique_Trois_Sauts_Amont", "Crique_Trois_Sauts",
                                   full_BDD$stations_groupees)
full_BDD$stations_groupees <- gsub("Crique_Trois_Sauts_Aval", "Crique_Trois_Sauts",
                                   full_BDD$stations_groupees)



# Creation d'une BDD avec toutes les stations

df.SIG <- full_BDD %.%
        group_by(station) %.%
        summarise(BV = Mode(BV), Latitude = Mode(Latitude), Longitude = Mode(Longitude),
                  Pression_anthro = Mode(Pression_anthro2), nb_individus = n(),
                  nb_ordres = n_distinct(ordre), nb_familles = n_distinct(Famille),
                  nb_sous_famille = n_distinct(sous_famille), ng_genres = n_distinct(Genre),
                  nb_sp = n_distinct(Code), nb_regimes_trophiques = n_distinct(Regime_alter),
                  nb_dosages_isotopie = length(na.omit(d13C)),
                  nb_dosages_elt_traces = length(na.omit(Cr_ppm)),
                  nb_dosages_Hg = length(na.omit(conc_Hg_muscle_ppm)),
                  masse_mediane_poissons_Hg_g = median(pds_g[!is.na(conc_Hg_muscle_ppm)]),
                  min_Hg = min(na.omit(conc_Hg_muscle_ppm)), max_Hg = max(na.omit(conc_Hg_muscle_ppm)),
                  moyenne_Hg = mean(na.omit(conc_Hg_muscle_ppm)),
                  sd_Hg = sd(na.omit(conc_Hg_muscle_ppm)),
                  nb_Hg_carni_invert = length(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  moyenne_Hg_carni_invert = mean(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'], na.rm = TRUE),
                  sd_Hg_carni_invert = sd(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  min_Hg_carni_invert = min(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  max_Hg_carni_invert = max(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  nb_Hg_omni_invert = length(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'])),
                  moyenne_Hg_omni_invert = mean(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'], na.rm = TRUE),
                  sd_Hg_omni_invert = sd(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'])),
                  min_Hg_omni_invert = min(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'])),
                  max_Hg_omni_invert = max(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore']))
                  )
        
df.SIG <- gsub("-Inf", "NA", as.matrix(df.SIG))
df.SIG <- gsub("Inf", "NA", as.matrix(df.SIG))
df.SIG <- gsub("NaN", "NA", as.matrix(df.SIG))
df.SIG <- gsub("2.47833", "2.247879", as.matrix(df.SIG))
df.SIG <- gsub("-52.874055", "-52.873222", as.matrix(df.SIG))

df.SIG <- as.data.frame(df.SIG, stringsAsFactors = FALSE)

df.SIG[, c(3:4, 6:30)] <- convert.magic(df.SIG[, c(3:4, 6:30)], "numeric")
df.SIG[, c(1:2, 5)] <- convert.magic(df.SIG[, c(1:2, 5)], "factor")

write.table(df.SIG, file = "Data/BDD_SIG.csv", row.names=FALSE, sep = ";")


# Creation d'une BDD avec les stations groupées

df.SIG_group <- full_BDD %.%
        group_by(stations_groupees) %.%
        summarise(BV = Mode(BV), Latitude = Mode(Latitude), Longitude = Mode(Longitude),
                  Pression_anthro = Mode(Pression_anthro2), nb_individus = n(),
                  nb_ordres = n_distinct(ordre), nb_familles = n_distinct(Famille),
                  nb_sous_famille = n_distinct(sous_famille), ng_genres = n_distinct(Genre),
                  nb_sp = n_distinct(Code), nb_regimes_trophiques = n_distinct(Regime_alter),
                  nb_dosages_isotopie = length(na.omit(d13C)),
                  nb_dosages_elt_traces = length(na.omit(Cr_ppm)),
                  nb_dosages_Hg = length(na.omit(conc_Hg_muscle_ppm)),
                  masse_mediane_poissons_Hg_g = median(pds_g[!is.na(conc_Hg_muscle_ppm)]),
                  min_Hg = min(na.omit(conc_Hg_muscle_ppm)), max_Hg = max(na.omit(conc_Hg_muscle_ppm)),
                  moyenne_Hg = mean(na.omit(conc_Hg_muscle_ppm)),
                  sd_Hg = sd(na.omit(conc_Hg_muscle_ppm)),
                  nb_Hg_carni_invert = length(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  moyenne_Hg_carni_invert = mean(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'], na.rm = TRUE),
                  sd_Hg_carni_invert = sd(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  min_Hg_carni_invert = min(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  max_Hg_carni_invert = max(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Carnivore_Invertivore'])),
                  nb_Hg_omni_invert = length(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'])),
                  moyenne_Hg_omni_invert = mean(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'], na.rm = TRUE),
                  sd_Hg_omni_invert = sd(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'])),
                  min_Hg_omni_invert = min(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore'])),
                  max_Hg_omni_invert = max(na.omit(conc_Hg_muscle_ppm[Regime_alter == 'Omnivore_Invertivore']))
        )

df.SIG_group <- gsub("-Inf", "", as.matrix(df.SIG_group))
df.SIG_group <- gsub("Inf", "", as.matrix(df.SIG_group))
df.SIG_group <- gsub("NaN", "", as.matrix(df.SIG_group))
df.SIG_group <- gsub("2.47833", "2.247879", as.matrix(df.SIG_group))
df.SIG_group <- gsub("-52.874055", "-52.873222", as.matrix(df.SIG_group))

df.SIG_group <- as.data.frame(df.SIG_group, stringsAsFactors = FALSE)

df.SIG_group[, c(3:4, 6:30)] <- convert.magic(df.SIG_group[, c(3:4, 6:30)], "numeric")
df.SIG_group[, c(1:2, 5)] <- convert.magic(df.SIG_group[, c(1:2, 5)], "factor")


write.table(df.SIG_group, file = "Data/BDD_SIG_stations_groupees.csv", row.names=FALSE, sep = ";")
