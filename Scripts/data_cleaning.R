###############################
#### Nettoyage des données ####
###############################


######################0000000000000########################

###
# Chargement BDD_PME comprenant les données PME, c'est à dire les criques Nouvelle-France, Chien et Trois Sauts
###

BDD_PME <-read.csv("Data/2014_04_11_subset_BDD.csv", sep=";", stringsAsFactors = FALSE) # Chemin relatif
# stringsAsFactors = FALSE permet d'éviter que les variables numériques soient reconnues comme des facteurs 


# str(BDD_PME)


BDD_PME$Code_Station <- as.factor(BDD_PME$Code_Station)
BDD_PME$code_sp <- as.factor(BDD_PME$code_sp)
BDD_PME$ordre <- as.factor(BDD_PME$ordre)
BDD_PME$Genre <- as.factor(BDD_PME$Genre)
BDD_PME$Groupe_station <- as.factor(BDD_PME$Groupe_station)
BDD_PME$station <- as.factor(BDD_PME$station)
BDD_PME$Regime_alter <- as.factor(BDD_PME$Regime_alter)
BDD_PME$Regime_principal <- as.factor(BDD_PME$Regime_principal)
BDD_PME$Regime_secondaire <- as.factor(BDD_PME$Regime_secondaire)
BDD_PME$Famille <- as.factor(BDD_PME$Famille)
BDD_PME$sous_famille <- as.factor(BDD_PME$sous_famille)
BDD_PME$Espece <- as.factor(BDD_PME$Espece)
BDD_PME$Site <- as.factor(BDD_PME$Site)
BDD_PME$BV <- as.factor(BDD_PME$BV)
BDD_PME$date <- as.factor(BDD_PME$date)

# Les valeurs étant supposées être numériques sont numériques, et les facteurs sont des facteurs

###
# Reordonne les niveaux du facteur "Regime_principal" dans l'ordre decroissant de contamination au mercure
###
BDD_PME$Regime_alter <- factor(BDD_PME$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

BDD_PME$Regime_principal <- factor(BDD_PME$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))


######################0000000000000########################

###
# Subset de données à partir de la BDD_PME  ; uniquement les échantillons ayant des métaux dosés
###

sub_BDD_PME <- BDD_PME[!(is.na(BDD_PME$Cr_ppm)), ]


######################0000000000000########################

###
# Subset de données à partir de la BDD_PME  ; uniquement les échantillons ayant du Hg dosé dans le muscle
###

sub_BDD <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ]

######################0000000000000########################

###
# Chargement de l'ensemble de la BDD comprenant les 21404 échantillons
###

BDD <- read.csv("Data/2014_04_15_BDD_PME.csv", sep=";", stringsAsFactors = FALSE)

# str(BDD)

BDD$pds_g<- as.numeric(BDD$pds_g) # Pas d'explication concernant le besoin de forcer la classe "numériqueé sur cette variable
BDD$Pression <- as.factor(BDD$Pression)
BDD$Orpaillage <- as.factor(BDD$Orpaillage)
BDD$Pression_anthro <- as.factor(BDD$Pression_anthro)
BDD$Code_Station <- as.factor(BDD$Code_Station)
BDD$code_sp <- as.factor(BDD$code_sp)
BDD$ordre <- as.factor(BDD$ordre)
BDD$Genre <- as.factor(BDD$Genre)
BDD$station <- as.factor(BDD$station)
BDD$Regime_alter <- as.factor(BDD$Regime_alter)
BDD$Regime_principal <- as.factor(BDD$Regime_principal)
BDD$Regime_secondaire <- as.factor(BDD$Regime_secondaire)
BDD$Famille <- as.factor(BDD$Famille)
BDD$sous_famille <- as.factor(BDD$sous_famille)
BDD$Espece <- as.factor(BDD$Espece)
BDD$BV <- as.factor(BDD$BV)
BDD$date <- as.factor(BDD$date)

BDD$Regime_alter <- factor(BDD$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 
BDD$Regime_principal <- factor(BDD$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))

######################0000000000000########################

###
# Subset comprenant tous les échantillons ayant du Hg dosé dans le muscle
###

BDD.sansNA <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)), ]

BDD.sansNA$station <- droplevels(BDD.sansNA$station)
