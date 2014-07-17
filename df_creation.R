# Données sous forme de tableaux pour les reproduire sous Excel

require(dplyr)
require(ggplot2)

### Répartition des Régimes alimentaires pour poissons ayant Hg dosé dans muscles selon les pressions anthropiques

Bd <- BDD.sansNA

levels(Bd$Regime_alter) <- sub("^Carnivore_Charognard$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Insectivore$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Scaliphage$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Herbivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Piscivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Herbivore$", "Herbivore_Phyllophage", levels(Bd$Regime_alter))

df.repartition_regime <- Bd %.%
  group_by(Pression_anthro2) %.%
  summarise(nb_poissons = length(ID_general),
            nb_carnivore_piscivore = length(ID_general[Regime_alter == 'Carnivore_Piscivore']),
            nb_carnivore_invertivore = length(ID_general[Regime_alter == 'Carnivore_Invertivore']),
            nb_carnivore_autre = length(ID_general[Regime_alter == 'Carnivore']),
            nb_omnivore_invertivore = length(ID_general[Regime_alter == 'Omnivore_Invertivore']),
            nb_omnivore_autre = length(ID_general[Regime_alter == 'Omnivore']),
            nb_detritivore = length(ID_general[Regime_alter == 'Detritivore']),
            nb_herbivore_phyllophage = length(ID_general[Regime_alter == 'Herbivore_Phyllophage']),
            nb_herbivore_periphytophage = length(ID_general[Regime_alter == 'Herbivore_Périphytophage']))

write.table(df.repartition_regime, file = "Data/repartition_regime_pression.csv", row.names=FALSE, sep = ";")


### Impact des pressions anthropiques sur la contamination en Hg

# Tous poissons

df.pression_Hg <- BDD.sansNA %.%
  group_by(Pression_anthro2) %.%
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),
            nb_stations = n_distinct(station),            
            concentration_mercure_min = min(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_max = max(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_moyenne = mean(na.omit(conc_Hg_muscle_ppm)),
            ecart_type_concentration_mercure = sd(na.omit(conc_Hg_muscle_ppm)),
            erreur_type_concentration_mercure = se(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_mediane = median((conc_Hg_muscle_ppm)),
            moustache_superieure = quantile(conc_Hg_muscle_ppm, 0.75) + (1.5 * IQR(conc_Hg_muscle_ppm)),
            moustache_inferieure = min(na.omit(conc_Hg_muscle_ppm)),
            premier_quartile = quantile(conc_Hg_muscle_ppm, 0.25),
            troisieme_quartile = quantile(conc_Hg_muscle_ppm, 0.75))

data.frame(df.pression_Hg)


write.table(df.pression_Hg, file = "Data/impact_pression_sur_Hg.csv", row.names=FALSE, sep = ";")


# Carnivores Piscivores

df.pression_Hg_pisc <- BDD.sansNA[BDD.sansNA$Regime_alter %in% "Carnivore_Piscivore", ] %.%
  group_by(Pression_anthro2) %.%
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),
            nb_stations = n_distinct(station),            
            concentration_mercure_min = min(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_max = max(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_moyenne = mean(na.omit(conc_Hg_muscle_ppm)),
            ecart_type_concentration_mercure = sd(na.omit(conc_Hg_muscle_ppm)),
            erreur_type_concentration_mercure = se(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_mediane = median((conc_Hg_muscle_ppm)),
            moustache_superieure = quantile(conc_Hg_muscle_ppm, 0.75) + (1.5 * IQR(conc_Hg_muscle_ppm)),
            moustache_inferieure = min(na.omit(conc_Hg_muscle_ppm)),
            premier_quartile = quantile(conc_Hg_muscle_ppm, 0.25),
            troisieme_quartile = quantile(conc_Hg_muscle_ppm, 0.75))

write.table(df.pression_Hg_pisc, file = "Data/impact_pression_sur_Hg_chez_piscivores.csv", row.names=FALSE, sep = ";")

# Carnivores Invertivores

df.pression_Hg_carn <- BDD.sansNA[BDD.sansNA$Regime_alter %in% "Carnivore_Invertivore", ] %.%
  group_by(Pression_anthro2) %.%
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),
            nb_stations = n_distinct(station),            
            concentration_mercure_min = min(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_max = max(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_moyenne = mean(na.omit(conc_Hg_muscle_ppm)),
            ecart_type_concentration_mercure = sd(na.omit(conc_Hg_muscle_ppm)),
            erreur_type_concentration_mercure = se(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_mediane = median((conc_Hg_muscle_ppm)),
            moustache_superieure = quantile(conc_Hg_muscle_ppm, 0.75) + (1.5 * IQR(conc_Hg_muscle_ppm)),
            moustache_inferieure = min(na.omit(conc_Hg_muscle_ppm)),
            premier_quartile = quantile(conc_Hg_muscle_ppm, 0.25),
            troisieme_quartile = quantile(conc_Hg_muscle_ppm, 0.75))

write.table(df.pression_Hg_carn, file = "Data/impact_pression_sur_Hg_chez_carnivores_invertivores.csv", row.names=FALSE, sep = ";")

# Omnivores Invertivores

df.pression_Hg_omn <- BDD.sansNA[BDD.sansNA$Regime_alter %in% "Omnivore_Invertivore", ] %.%
  group_by(Pression_anthro2) %.%
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),
            nb_stations = n_distinct(station),            
            concentration_mercure_min = min(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_max = max(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_moyenne = mean(na.omit(conc_Hg_muscle_ppm)),
            ecart_type_concentration_mercure = sd(na.omit(conc_Hg_muscle_ppm)),
            erreur_type_concentration_mercure = se(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_mediane = median((conc_Hg_muscle_ppm)),
            moustache_superieure = quantile(conc_Hg_muscle_ppm, 0.75) + (1.5 * IQR(conc_Hg_muscle_ppm)),
            moustache_inferieure = min(na.omit(conc_Hg_muscle_ppm)),
            premier_quartile = quantile(conc_Hg_muscle_ppm, 0.25),
            troisieme_quartile = quantile(conc_Hg_muscle_ppm, 0.75))

write.table(df.pression_Hg_omn, file = "Data/impact_pression_sur_Hg_chez_omnivores_invertivores.csv", row.names=FALSE, sep = ";")

    # Vérif du résultat obtenu
ggplot(df.pression_Hg, aes(x = factor(Pression_anthro2))) + 
  geom_boxplot(aes(lower = premier_quartile, upper = troisieme_quartile, middle = concentration_mercure_mediane, ymin = concentration_mercure_min, ymax = concentration_mercure_max), stat = "identity") +
  geom_point(aes( y = concentration_mercure_moyenne))


### Différence contamination mercure selon les groupes de stations

df <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ]

df.groupe_station <- df %.%
  group_by(Groupe_station) %.%  
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),   
            concentration_mercure_min = min(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_max = max(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_moyenne = mean(na.omit(conc_Hg_muscle_ppm)),
            ecart_type_concentration_mercure = sd(na.omit(conc_Hg_muscle_ppm)),
            erreur_type_concentration_mercure = se(na.omit(conc_Hg_muscle_ppm)),
            concentration_mercure_mediane = median((conc_Hg_muscle_ppm)),
            moustache_superieure = quantile(conc_Hg_muscle_ppm, 0.75) + (1.5 * IQR(conc_Hg_muscle_ppm)),
            moustache_inferieure = min(na.omit(conc_Hg_muscle_ppm)),
            premier_quartile = quantile(conc_Hg_muscle_ppm, 0.25),
            troisieme_quartile = quantile(conc_Hg_muscle_ppm, 0.75))

write.table(df.groupe_station, file = "Data/contamination_Hg_groupe_stations.csv", row.names=FALSE, sep = ";")

### [Hg] dans les organes, d13C et d15N en fonctions du régime alimentaire

# Poissons ayant données dans tous les organes
df.reg.org <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$d15N)) & !(is.na(BDD$conc_Hg_branchie_ppm))
                  & !(is.na(BDD$conc_Hg_foie_ppm)), ] %.% # Selection BDD globale
  group_by(Regime_alter) %.% # Sélection par régime
  summarise(nb_poissons = length(conc_Hg_muscle_ppm), Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)),
            Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Sélection des données à calculer

data.frame(df.reg.org)

write.table(df.reg.org, file = "Data/isotopie_Hg_tous_organes.csv", row.names=FALSE, sep = ";")

# Poissons ayant données mercure muscle et isotopie
df.reg.muscle <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$d15N)), ] %.% # Selection BDD globale
  group_by(Regime_alter) %.% # Sélection par régime
  summarise(nb_poissons = length(conc_Hg_muscle_ppm), Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

data.frame(df.reg.muscle)


write.table(df.reg.muscle, file = "Data/isotopie_Hg_muscle.csv", row.names=FALSE, sep = ";")


# Poissons ayant données mercure foie et isotopie
df.reg.foie <- BDD[!(is.na(BDD$conc_Hg_foie_ppm)) & !(is.na(BDD$d15N)), ] %.% # Selection BDD globale
  group_by(Regime_alter) %.% # Sélection par régime
  summarise(nb_poissons = length(conc_Hg_muscle_ppm), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

data.frame(df.reg.foie)

write.table(df.reg.foie, file = "Data/isotopie_Hg_foie.csv", row.names=FALSE, sep = ";")


# Poissons ayant données mercure branchie et isotopie
df.reg.branchie <- BDD[!(is.na(BDD$conc_Hg_branchie_ppm)) & !(is.na(BDD$d15N)), ] %.% # Selection BDD globale
  group_by(Regime_alter) %.% # Sélection par régime
  summarise(nb_poissons = length(conc_Hg_muscle_ppm), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

data.frame(df.reg.branchie)

write.table(df.reg.branchie, file = "Data/isotopie_Hg_branchie.csv", row.names=FALSE, sep = ";")



#### Organotropisme


df.organotropism <- df.sp.ratio.melt %.%
  group_by(variable, Regime_alt) %.%
  summarise(nb_poissons = length(value),   
            concentration_mercure_min = min(na.omit(value)),
            concentration_mercure_max = max(na.omit(value)),
            concentration_mercure_moyenne = mean(na.omit(value)),
            ecart_type_concentration_mercure = sd(na.omit(value)),
            erreur_type_concentration_mercure = se(na.omit(value)),
            concentration_mercure_mediane = median((value)),
            moustache_superieure = quantile(value, 0.75) + (1.5 * IQR(value)),
            moustache_inferieure = min(na.omit(value)),
            premier_quartile = quantile(value, 0.25),
            troisieme_quartile = quantile(value, 0.75))


data.frame(df.organotropism)


write.table(df.organotropism, file = "Data/organotropisme.csv", row.names=FALSE, sep = ";")


### Elements traces

df.elt_trace <- sub_BDD_PME4 %.%
  group_by(Groupe_station) %.%  
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),
            concentration_Cd_min = min(na.omit(Cd_ppm)),
            concentration_Cd_max = max(na.omit(Cd_ppm)),
            concentration_Cd_moyenne = mean(na.omit(Cd_ppm)),
            ecart_type_concentration_Cd = sd(na.omit(Cd_ppm)),
            erreur_type_concentration_Cd = se(na.omit(Cd_ppm)),
            concentration_Cd_mediane = median((Cd_ppm)),
            moustache_superieure_Cd = quantile(Cd_ppm, 0.75) + (1.5 * IQR(Cd_ppm)),
            moustache_inferieure_Cd = min(na.omit(Cd_ppm)),
            premier_quartile_Cd = quantile(Cd_ppm, 0.25),
            troisieme_quartile_Cd = quantile(Cd_ppm, 0.75),
            concentration_Co_min = min(na.omit(Co_ppm)),
            concentration_Co_max = max(na.omit(Co_ppm)),
            concentration_Co_moyenne = mean(na.omit(Co_ppm)),
            ecart_type_concentration_Co = sd(na.omit(Co_ppm)),
            erreur_type_concentration_Co = se(na.omit(Co_ppm)),
            concentration_Co_mediane = median((Co_ppm)),
            moustache_superieure_Co = quantile(Co_ppm, 0.75) + (1.5 * IQR(Co_ppm)),
            moustache_inferieure_Co = min(na.omit(Co_ppm)),
            premier_quartile_Co = quantile(Co_ppm, 0.25),
            troisieme_quartile_Co = quantile(Co_ppm, 0.75),
            concentration_Cr_min = min(na.omit(Cr_ppm)),
            concentration_Cr_max = max(na.omit(Cr_ppm)),
            concentration_Cr_moyenne = mean(na.omit(Cr_ppm)),
            ecart_type_concentration_Cr = sd(na.omit(Cr_ppm)),
            erreur_type_concentration_Cr = se(na.omit(Cr_ppm)),
            concentration_Cr_mediane = median((Cr_ppm)),
            moustache_superieure_Cr = quantile(Cr_ppm, 0.75) + (1.5 * IQR(Cr_ppm)),
            moustache_inferieure_Cr = min(na.omit(Cr_ppm)),
            premier_quartile_Cr = quantile(Cr_ppm, 0.25),
            troisieme_quartile_Cr = quantile(Cr_ppm, 0.75),
            concentration_Cu_min = min(na.omit(Cu_ppm)),
            concentration_Cu_max = max(na.omit(Cu_ppm)),
            concentration_Cu_moyenne = mean(na.omit(Cu_ppm)),
            ecart_type_concentration_Cu = sd(na.omit(Cu_ppm)),
            erreur_type_concentration_Cu = se(na.omit(Cu_ppm)),
            concentration_Cu_mediane = median((Cu_ppm)),
            moustache_superieure_Cu = quantile(Cu_ppm, 0.75) + (1.5 * IQR(Cu_ppm)),
            moustache_inferieure_Cu = min(na.omit(Cu_ppm)),
            premier_quartile_Cu = quantile(Cu_ppm, 0.25),
            troisieme_quartile_Cu = quantile(Cu_ppm, 0.75),
            concentration_Ni_min = min(na.omit(Ni_ppm)),
            concentration_Ni_max = max(na.omit(Ni_ppm)),
            concentration_Ni_moyenne = mean(na.omit(Ni_ppm)),
            ecart_type_concentration_Ni = sd(na.omit(Ni_ppm)),
            erreur_type_concentration_Ni = se(na.omit(Ni_ppm)),
            concentration_Ni_mediane = median((Ni_ppm)),
            moustache_superieure_Ni = quantile(Ni_ppm, 0.75) + (1.5 * IQR(Ni_ppm)),
            moustache_inferieure_Ni = min(na.omit(Ni_ppm)),
            premier_quartile_Ni = quantile(Ni_ppm, 0.25),
            troisieme_quartile_Ni = quantile(Ni_ppm, 0.75),
            concentration_Pb_min = min(na.omit(Pb_ppm)),
            concentration_Pb_max = max(na.omit(Pb_ppm)),
            concentration_Pb_moyenne = mean(na.omit(Pb_ppm)),
            ecart_type_concentration_Pb = sd(na.omit(Pb_ppm)),
            erreur_type_concentration_Pb = se(na.omit(Pb_ppm)),
            concentration_Pb_mediane = median((Pb_ppm)),
            moustache_superieure_Pb = quantile(Pb_ppm, 0.75) + (1.5 * IQR(Pb_ppm)),
            moustache_inferieure_Pb = min(na.omit(Pb_ppm)),
            premier_quartile_Pb = quantile(Pb_ppm, 0.25),
            troisieme_quartile_Pb = quantile(Pb_ppm, 0.75),
            concentration_Zn_min = min(na.omit(Zn_ppm)),
            concentration_Zn_max = max(na.omit(Zn_ppm)),
            concentration_Zn_moyenne = mean(na.omit(Zn_ppm)),
            ecart_type_concentration_Zn = sd(na.omit(Zn_ppm)),
            erreur_type_concentration_Zn = se(na.omit(Zn_ppm)),
            concentration_Zn_mediane = median((Zn_ppm)),
            moustache_superieure_Zn = quantile(Zn_ppm, 0.75) + (1.5 * IQR(Zn_ppm)),
            moustache_inferieure_Zn = min(na.omit(Zn_ppm)),
            premier_quartile_Zn = quantile(Zn_ppm, 0.25),
            troisieme_quartile_Zn = quantile(Zn_ppm, 0.75))
  

write.table(df.elt_trace, file = "Data/elements_traces.csv", row.names=FALSE, sep = ";")


df.Se <- sub_BDD_PME4[!(is.na(sub_BDD_PME4$Se_ppm)), ] %.%
  group_by(Groupe_station) %.%  
  summarise(nb_poissons = length(conc_Hg_muscle_ppm),
            concentration_Se_min = min(na.omit(Se_ppm)),
            concentration_Se_max = max(na.omit(Se_ppm)),
            concentration_Se_moyenne = mean(na.omit(Se_ppm)),
            ecart_type_concentration_Se = sd(na.omit(Se_ppm)),
            erreur_type_concentration_Se = se(na.omit(Se_ppm)),
            concentration_Se_mediane = median((Se_ppm)),
            moustache_superieure_Se = quantile(Se_ppm, 0.75) + (1.5 * IQR(Se_ppm)),
            moustache_inferieure_Se = min(na.omit(Se_ppm)),
            premier_quartile_Se = quantile(Se_ppm, 0.25),
            troisieme_quartile_Se = quantile(Se_ppm, 0.75))


write.table(df.Se, file = "Data/Se.csv", row.names=FALSE, sep = ";")
