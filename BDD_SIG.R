### Création BDD pour SIG ###


require(ggplot2)
require(dplyr)
#require(reshape2)

source("Scripts/functions.R")

df.SIG <- BDD %.%
        group_by(station) %.%
        summarise(BV = Mode(BV), Latitude = Mode(Latitude), Longitude = Mode(Longitude),
                  Pression_anthro = Mode(Pression_anthro2), nb_individus = n(),
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

df.SIG <- as.data.frame(df.SIG, stringsAsFactors = FALSE)


df.SIG[, c(3:4, 7:26)] <- convert.magic(df.SIG[, c(3:4, 7:26)], "numeric")
df.SIG[, c(1:2, 5:6)] <- convert.magic(df.SIG[, c(1:2, 5:6)], "factor")
