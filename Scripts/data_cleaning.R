###############################
#### Nettoyage des données ####
###############################


######################0000000000000########################

###
# Chargement BDD_PME comprenant les données des criques Nouvelle-France, Chien et Trois Sauts
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
# Subset de données à partir de la BDD_PME  ; uniquement les échantillons ayant des éléments traces dosés
###

sub_BDD_PME <- BDD_PME[!(is.na(BDD_PME$Cr_ppm)), ]

# Supression détritivore (n=2)

sub_BDD_PME2 <- sub_BDD_PME[sub_BDD_PME$Regime_principal %in% c("Carnivore", "Omnivore", "Herbivore"),]

# Supression détritivore (n=2), scaliphage (n = 2), insectivore (n=1), carnivore (n=7 sur une seule station), Omnivore herbivore (n=16 mais uniquement 3 stations)

sub_BDD_PME3 <- sub_BDD_PME[sub_BDD_PME$Regime_alter %in% c("Carnivore_Piscivore", "Carnivore_Invertivore","Omnivore_Invertivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage") ,]

# Tous les herbivores regroupés

sub_BDD_PME4 <- sub_BDD_PME[sub_BDD_PME$Regime_alter %in% c("Carnivore_Piscivore", "Carnivore_Invertivore","Omnivore_Invertivore", "Omnivore_Herbivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage") ,]

levels(sub_BDD_PME4$Regime_alter) <- sub("^Omnivore_Herbivore$", "Omnivore", levels(sub_BDD_PME4$Regime_alter))
levels(sub_BDD_PME4$Regime_alter) <- sub("^Omnivore_Invertivore$", "Omnivore", levels(sub_BDD_PME4$Regime_alter))
levels(sub_BDD_PME4$Regime_alter) <- sub("^Herbivore_Periphytophage$", "Herbivore", levels(sub_BDD_PME4$Regime_alter))
levels(sub_BDD_PME4$Regime_alter) <- sub("^Herbivore_Phyllophage$", "Herbivore", levels(sub_BDD_PME4$Regime_alter))

sub_BDD_PME4$Regime_alter <- droplevels(sub_BDD_PME4$Regime_alter) # Drop unused levels

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
BDD$Pression_anthro <- factor(BDD$Pression_anthro, levels = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_legal", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"))
BDD$Pression_anthro2 <- factor(BDD$Pression_anthro2, levels = c( "Reference_Trois_Sauts", "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_legal", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"))


######################0000000000000########################

###
# Subset comprenant tous les échantillons ayant du Hg dosé dans le muscle
###

BDD.sansNA <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)), ]

BDD.sansNA$station <- droplevels(BDD.sansNA$station)

BDD.sansNA$Pression_anthro <- droplevels(BDD.sansNA$Pression_anthro)

BDD.sansNA$Pression_anthro2 <- droplevels(BDD.sansNA$Pression_anthro2)


######################0000000000000########################

#
## Mise en forme données pour graphiques [hg] en fonction de d15N
#

# Intégralité BDD qui possède valeurs dans les 3 organes

df.reg.org <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$d15N)) & !(is.na(BDD$conc_Hg_branchie_ppm))
                         & !(is.na(BDD$conc_Hg_foie_ppm)), ] %.% # Selection BDD globale
  group_by(Regime_alter) %.% # Sélection par régime
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)),
            Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Sélection des données à calculer

df.reg.org <- na.omit(df.reg.org)

# Données muscle et d15N

df.reg.muscle <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$d15N)), ] %.% # Selection BDD globale
        group_by(Regime_alter) %.% # Sélection par régime
        filter(Regime_alter != "Carnivore" & Regime_alter != "Carnivore_Scaliphage" & Regime_alter != "Herbivore") %.% # régimes mineurs ou trop flous retirés
        summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
                  d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.reg.muscle <- na.omit(df.reg.muscle)

# Données muscle et d15N dans les sites orpaillage + barrage

bdd <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$d15N)), ]

df.reg.conta <- bdd[bdd$Pression_anthro2 == "Reference" | bdd$Pression_anthro2 == "Reference_Trois_Sauts", ] %.% # Selection BDD globale
        group_by(Regime_alter) %.% # Sélection par régime
        summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
                  d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.reg.conta <- na.omit(df.reg.conta)

# Test ratio entre [] dans les divers organes
df.reg.org2 <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$conc_Hg_branchie_ppm))
                  & !(is.na(BDD$conc_Hg_foie_ppm)), ] %.% # Selection BDD globale
        group_by(Regime_alter) %.% # Sélection par régime
        summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
                  d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)),
                  Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Sélection des données à calculer

df.reg.org2 <- na.omit(df.reg.org2)

df.reg.ratio <- df.reg.org2 %.%
  group_by(Regime_alter) %.%
  mutate(muscle.foie = (Hg_muscle_mean / Hg_foie_mean), muscle.branchie = (Hg_muscle_mean / Hg_branchie_mean), foie.branchie = (Hg_foie_mean / Hg_branchie_mean))

df.ratio.conc <- melt(df.reg.ratio, id.vars = "Regime_alter", measure.vars = c("muscle.foie", "muscle.branchie", "foie.branchie"))
df.ratio.conc <- filter(df.ratio.conc, Regime_alter != "Carnivore" & Regime_alter != "Carnivore_Insectivore" & Regime_alter != "Herbivore" & Regime_alter != "Detritivore" & Regime_alter != "Herbivore_Phyllophage")

df.sp.ratio <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)) & !(is.na(BDD$conc_Hg_branchie_ppm))
                & !(is.na(BDD$conc_Hg_foie_ppm)), ] %.%
        group_by(ID_general) %.%
        select(ID_general, conc_Hg_muscle_ppm, conc_Hg_foie_ppm, conc_Hg_branchie_ppm, Regime_alter) %.%# Sélection des données à calculer
        mutate(muscle.foie = (conc_Hg_muscle_ppm / conc_Hg_foie_ppm), muscle.branchie = (conc_Hg_muscle_ppm / conc_Hg_branchie_ppm), foie.branchie = (conc_Hg_foie_ppm / conc_Hg_branchie_ppm))

df.sp.ratio <- as.data.frame(df.sp.ratio)
df.sp.ratio$Regime_alt <- df.sp.ratio$Regime_alter
df.sp.ratio <- df.sp.ratio[, -c(1:5)]

df.sp.ratio.melt <- melt(df.sp.ratio, id.vars = "Regime_alt", measure.vars = c("muscle.foie", "muscle.branchie", "foie.branchie"))
df.sp.ratio.melt <- filter(df.sp.ratio.melt, Regime_alt != "Carnivore" & Regime_alt != "Carnivore_Insectivore" & Regime_alt != "Herbivore" & Regime_alt != "Detritivore" & Regime_alt != "Herbivore_Phyllophage" & variable != "foie.branchie")

# Données uniquement de 3 Sauts, Crique Chien et Nouvelle France
df.PME <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)), ] %.% # Selection BDD_PME
  group_by(Regime_alter) %.% # Selection par régime
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.PME <- na.omit(df.PME)
df.PME <- df.PME[- nrow(df.PME),] # Enlève la dernière ligne ("inconnu")

# Données uniquement Crique Chien non contaminée
df.chien.nonconta <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "Chien_non_conta", ] %.%
  group_by(Regime_alter) %.% # Selection par régime
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.chien.nonconta <- na.omit(df.chien.nonconta)

# Données uniquement Crique Chien contaminée
df.chien.conta <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "Chien_conta", ] %.%
  group_by(Regime_alter) %.% # Selection par régime
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.chien.conta <- na.omit(df.chien.conta)

# Données uniquement Nouvelle France contaminée
df.NF.conta <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "NF_conta", ] %.%
  group_by(Regime_alter) %.% # Selection par régime
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.NF.conta <- na.omit(df.NF.conta)

# Données uniquement Nouvelle France non contaminée
df.NF.nonconta <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "NF_non_conta", ] %.%
  group_by(Regime_alter) %.% # Selection par régime
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
            d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.NF.nonconta <- na.omit(df.NF.nonconta)
df.NF.nonconta <- df.NF.nonconta[- nrow(df.NF.nonconta),]


######################0000000000000########################


# Création de légendes homogènes

        #au niveau des couleurs

 ### Pour les éléments traces

color <-  c("#F8766D", "#00BFC4", "#C77CFF", "#7CAE00")

 ### Pour isotopie d15N

scales::hue_pal()(10)

colo <- c( "#F8766D", "#D89000", "#A3A500", "#FF62BC", "#E76BF3", "#9590FF",
           "#00B0F6", "#39B600", "#00BFC4", "#00BF7D") # 10 couleurs
colo2 <- c( "#F8766D", "#D89000", "#A3A500", "#FF62BC", "#E76BF3", "#9590FF",
            "#00B0F6", "#39B600", "#00BF7D") # 9 couleurs
colo8 <- c("#F8766D", "#00BE67", "#CD9600", "#00A9FF", "#C77CFF", "#FF61CC", "#00BFC4", "#7CAE00") #8 couleurs ; gris pour NA = "3D3D3D"


        # au niveau du nom des groupes de stations

limit_groupes <- c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta")
label_groupes <- c("Trois Sauts", "Chien non \ncontaminée", "Chien \ncontaminée", "Nouvelle France \nnon contaminée", "Nouvelle France \ncontaminée")
