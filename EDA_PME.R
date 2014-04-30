######################
##### Analyses PME ###
######################


rm(list = ls () ) #nettoyage memoire de R

require(MASS)
require(ggplot2)
require(plyr)
require(dplyr)
require(scales)
require(reshape) # pr fction melt, afin chgt format wide -> long
require(reshape2)
require(FactoMineR)
require(gridExtra)
require(gtable) # alternative pour arranger graphe ensembles
require(missMDA)


source("Scripts/functions.R")
source("Scripts/data_cleaning.R")

# Chargement BDD_PME comprenant donnees Criques Nouvelle-France, Chien et Trois Sauts

BDD_PME <-read.csv("Data/2014_04_11_subset_BDD.csv", sep=";", stringsAsFactors = FALSE) # Chemin relatif
# stringsAsFactors = FALSE permet d'eviter que les variables numeriques soient reconnues comme des facteurs 


str(BDD_PME)

class(BDD_PME$ls_mm)
class(BDD_PME$pds_g)
class(BDD_PME$conc_Hg_muscle_ppm)
class(BDD_PME$conc_Hg_foie_ppm)
class(BDD_PME$conc_Hg_branchie_ppm)

class(BDD_PME$As_ppm)
class(BDD_PME$Cr_ppm)
class(BDD_PME$Co_ppm)
class(BDD_PME$Cu_ppm)
class(BDD_PME$Ni_ppm)
class(BDD_PME$Zn_ppm)
class(BDD_PME$Se_ppm)
class(BDD_PME$Cd_ppm)
class(BDD_PME$Pb_ppm)

class(BDD_PME$Regime_alter)
BDD_PME$Code_Station <- as.factor(BDD_PME$Code_Station)
BDD_PME$code_sp <- as.factor(BDD_PME$code_sp)
BDD_PME$ordre <- as.factor(BDD_PME$ordre)
BDD_PME$Genre <- as.factor(BDD_PME$Genre)
BDD_PME$Groupe_station <- as.factor(BDD_PME$Groupe_station)
BDD_PME$station <- as.factor(BDD_PME$station)
BDD_PME$Regime_alter <- as.factor(BDD_PME$Regime_alter)
BDD_PME$Regime_principal <- as.factor(BDD_PME$Regime_principal)


# Les valeurs etant supposees etre numeriques sont numeriques

#BDD_PME$conc_Hg_muscle_ppm<- as.numeric(BDD_PME$conc_Hg_muscle_ppm)
#BDD_PME$Pb_ppm<- as.numeric(BDD_PME$Pb_ppm)


###############################
## Exploratory Data Analysis ##
###############################

#####______######

### scatterplot poids fonction taille + projection marginale des distributions

#placeholder plot
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#### pds fction de longueur
scatter <- ggplot(BDD_PME, aes(x = ls_mm, y = pds_g)) + 
  geom_point(aes(color = Regime_alter, shape = Regime_principal)) + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  scale_x_continuous(limits = c(0, 200)) + 
  scale_y_continuous(limits = c(0, 100)) # 2 Hoplias aimara de presque 2 kg qui entrainent le tassement de la majorite du jeu de donnees

#marginal density of x - plot on top
plot_top <- ggplot(BDD_PME, aes(ls_mm, fill=Regime_alter)) + 
  geom_density(alpha = .5) + 
  scale_x_continuous(limits = c(0, 200)) + # pour apercu plus detaille de la distrib de la majorite des poissons
  theme(legend.position = "none")

#### distribution approximative du poids
#ggplot(BDD_PME, aes(x = Regime_alter, y = pds_g)) +
#  geom_boxplot() +
#  scale_y_continuous(limits = c(0, 200)) # la grande majorite est <200g

#marginal density of y - plot on the right
plot_right <- ggplot(BDD_PME, aes(pds_g, fill=Regime_alter)) + 
  geom_density(alpha = .5) + 
  scale_x_continuous(limits = c(0, 50)) + # limites d'apres divers essais. poissons tres legers
  coord_flip() + 
  theme(legend.position = "none") 


#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

# nuage de points alternatif avec zoom sur majorite de points
scatter <- ggplot(BDD_PME, aes(x = ls_mm, y = pds_g)) + 
  geom_point(aes(color = Regime_alter, shape = Regime_principal)) + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  scale_x_continuous(limits = c(0, 200)) + 
  scale_y_continuous(limits = c(0, 100)) #+ # 2 Hoplias aimara de presque 2 kg qui entrainent le tassement de la majorite du jeu de donnees
#geom_smooth(aes(color = Regime_alter), method = "loess")


#### Distribution poids des individus

ggplot(BDD_PME, aes(x = pds_g, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 10) +
  geom_density()

#### Distribution taille des individus

ggplot(BDD_PME, aes(x = ls_mm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 5) +
  geom_density()

##___##



## Quelle est la repartition des regimes alimentaires parmi les poissons preleves sur chaque groupe de station ?


# Reordonne les niveaux du facteur "Regime_principal" dans l'ordre decroissant de contamination au mercure
BDD_PME$Regime_alter <- factor(BDD_PME$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

BDD_PME$Regime_principal <- factor(BDD_PME$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))


# Repartition des regimes pr chaque groupe de stations pour BDD_PME generale
p1 <- ggplot(BDD_PME, aes(Groupe_station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill")
# repartition des regimes pr chaque groupe de stations (sans prendre en compte le nb d'individus)
p2 <- ggplot(BDD_PME, aes(x = Groupe_station)) +
  geom_bar(aes(fill = Regime_alter))
# repartition des regimes pr chaque groupe de stations (en prenant en compte le nb d'individus)
grid.arrange(p1, p2, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))

# Repartition des regimes pr chaque groupe de stations pour subset BDD_PME
p11 <- ggplot(sub_BDD_PME, aes(Groupe_station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill")
# repartition des regimes pr chaque groupe de stations (sans prendre en compte le nb d'individus)
p22 <- ggplot(sub_BDD_PME, aes(x = Groupe_station)) +
  geom_bar(aes(fill = Regime_alter))
# repartition des regimes pr chaque groupe de stations (en prenant en compte le nb d'individus)
grid.arrange(p11, p22, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))


# Repartition des regimes sur chaque station
p10 <- ggplot(BDD_PME, aes(Code_Station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill")
# repartition des regimes sur chaque station (sans prendre en compte le nb d'individus)
p20 <- ggplot(BDD_PME, aes(x = Code_Station)) +
  geom_bar(aes(fill = Regime_alter))
# repartition des regimes sur chaque station (en prenant en compte le nb d'individus)
grid.arrange(p10, p20, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))

# La repartition des regimes est fondamentalement differente au niveau de la Crique Nouvelle France 6
# Peu d'individus ont ete preleves au niveau des criques Chien

# Repartition des genres sur chaque station
p100 <- ggplot(BDD_PME, aes(Code_Station)) +
  geom_bar(aes(fill = Genre), position = "fill") +
  guides(fill=guide_legend(ncol=2))
# 
p200 <- ggplot(BDD_PME, aes(x = Code_Station)) +
  geom_bar(aes(fill = Genre))

# extraction de la legende
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(p100)
# 
grid.arrange(arrangeGrob(p100 + theme(legend.position="none"),
                         p200 + theme(legend.position="none"),
                         ncol = 1),
             legend, ncol = 2, nrow = 1, widths = c(8, 1), heights = c(1, 1))

# Repartition des genres a chaque groupe de stations
p1000 <- ggplot(BDD_PME, aes(Groupe_station)) +
  geom_bar(aes(fill = Genre), position = "fill") +
  guides(fill=guide_legend(ncol=2))
# 
p2000 <- ggplot(BDD_PME, aes(x = Groupe_station)) +
  geom_bar(aes(fill = Genre))

legend <- g_legend(p1000)
# 
grid.arrange(arrangeGrob(p1000 + theme(legend.position="none"),
                         p2000 + theme(legend.position="none"),
                         ncol = 1),
             legend, ncol = 2, nrow = 1, widths = c(8, 1), heights = c(1, 1))


##___##



## Quelle est la contamination moyenne en Hg parmi les poissons preleves sur chaque groupe de station en fonction de leur regime alimentaire ?

dfmelt_Hg <- melt(BDD_PME, id.vars = c("Groupe_station", "Regime_principal", "Code_Station"), measure.vars = c("conc_Hg_muscle_ppm", "conc_Hg_foie_ppm", "conc_Hg_branchie_ppm"))
# la fonction melt permet de modifier le format d'une BDD_PME. Changement wide -> long

p_1<-ggplot(BDD_PME, aes(x = Groupe_station, y = conc_Hg_muscle_ppm)) +
  geom_boxplot()
p_2<-ggplot(BDD_PME, aes(x = Groupe_station, y = conc_Hg_foie_ppm)) +
  geom_boxplot()
p_3<-ggplot(BDD_PME, aes(x = Groupe_station, y = conc_Hg_branchie_ppm)) +
  geom_boxplot()
grid.arrange(p_1, p_2, p_3, ncol = 1, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# ou

ggplot(dfmelt_Hg, aes(x = Groupe_station, y = value, color = variable)) +
  geom_boxplot() +
  facet_grid(Regime_principal~., scales = "free_y")

ggplot(dfmelt_Hg, aes(x = Groupe_station, y = value, color = variable)) +
  geom_boxplot() +
  facet_grid(Regime_principal~.)


## Quelle est la contamination moyenne en Hg parmi les poissons preleves sur chaque station en fonction de leur regime alimentaire ?


ggplot(dfmelt_Hg, aes(x = Code_Station, y = value, color = variable)) +
  geom_boxplot() +
  facet_grid(Regime_principal~., scales = "free_y")

ggplot(dfmelt_Hg, aes(x = Code_Station, y = value, color = variable)) +
  geom_boxplot() +
  facet_grid(Regime_principal~.)



##___##



###########################
### MTX : Vue d'ensemble ##
###########################



dfmelt_mtx <- melt(BDD_PME, id.vars = c("Groupe_station", "Code_Station", "Site", "Regime_principal"), measure.vars = c("Hg_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm"))

## Quelle est la contamination moyenne en metaux sur chaque site ?
ggplot(dfmelt_mtx, aes(x = Site, y = value, color = variable)) +
  geom_boxplot()

## Quelle est la contamination moyenne en metaux sur chaque groupe de station ?
ggplot(dfmelt_mtx, aes(x = Groupe_station, y = value, color = variable)) +
  geom_boxplot()

## Quelle est la contamination moyenne en metaux sur chaque station ?
ggplot(na.omit(dfmelt_mtx), aes(x = Code_Station, y = value, color = variable)) +
  geom_boxplot() # na.omit(df) permet de ne pas tenir compte des valeurs manquantes. Cpdt, s'applique sur l'ensemble de la dataframe donc faire attention


## Informations globales sur la contamination metallique

# tests non concluants

# df_mtx <- melt(BDD_PME, id.vars = NULL, measure.vars = c("Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm")
# ggplot(df_mtx, aes(x = value, y = ..density..)) + geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.3) +  geom_density() +  facet_grid(variable~., scales = "free_x")
# ne fonctionne pas, je suppose qu'un ajustement manuel est necessaire



# Exemple de representation de distribution
ggplot(BDD_PME, aes(Cu_ppm)) +
  geom_density(fill = "blue", colour = NA, alpha=.2) + # polygone bleu pour la densite
  geom_line(stat = "density") # contour du polygone en noir

# Pr chq metal, superposition densite theorique et histogramme de distribution

Hg <- ggplot(BDD_PME, aes(x = conc_Hg_muscle_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.1) +
  geom_density()

Cu <- ggplot(BDD_PME, aes(x = Cu_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.5) +
  geom_density()

Cr <- ggplot(BDD_PME, aes(x = Cr_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.05) +
  geom_density()

Co <- ggplot(BDD_PME, aes(x = Co_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.05) +
  geom_density()

Cd <- ggplot(BDD_PME, aes(x = Cd_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.002) +
  geom_density()

Ni <- ggplot(BDD_PME, aes(x = Ni_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.2) +
  geom_density()

Zn <- ggplot(BDD_PME, aes(x = Zn_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 3) +
  geom_density()

As <- ggplot(BDD_PME, aes(x = As_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.04) +
  geom_density()

Se <- ggplot(BDD_PME, aes(x = Se_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.2) +
  geom_density()

Pb <- ggplot(BDD_PME, aes(x = Pb_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.08) +
  geom_density()

# Repartition mtx hors Hg
grid.arrange(Zn, Cu, Se, Ni, Cr, Pb, Co, As, Cd, ncol = 3, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# Repartition mtx avec Hg
grid.arrange(Zn, Cu, Se, Hg, Ni, Cr, Pb, Co, As, Cd, ncol = 4, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# ggplot(BDD_PME, aes(x = Cu_ppm, y = ..density..)) +  geom_histogram(aes(fill= Regime_principal), binwidth = 0.5) +  geom_density()
# tentative d'ajout de la repartition des regimes alimentaires au niveau de chq distribution. Non concluant mais non developpe


# Informations plus precises

# Trois groupes car trois gammes de concentrations

# [Zn] : 0-90 ppm
dfmelt_mtx1 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes"), measure.vars = c("Zn_ppm"))
p1 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Regime_principal)) +
  geom_boxplot()
p01 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Genre)) +
  geom_boxplot()
p001 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Code_Station)) +
  geom_boxplot()
p0001 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Groupe_station)) +
  geom_boxplot()

# [Hg], [Cr], [Co], [Cd], [Ni], [As], [Pb] : 0-10 ppm
dfmelt_mtx2 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes"), measure.vars = c("Hg_ppm","Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
p2 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Regime_principal)) +
  geom_boxplot() + theme(legend.position = "none")
p02 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Genre)) +
  geom_boxplot() + theme(legend.position = "none")
p002 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Code_Station)) +
  geom_boxplot() + theme(legend.position = "none")
p0002 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Groupe_station)) +
  geom_boxplot() + theme(legend.position = "none")

# [Cu], [Se] : 0-30 ppm
dfmelt_mtx3 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes"), measure.vars = c("Cu_ppm", "Se_ppm"))
p3 <- ggplot(dfmelt_mtx3, aes(x = variable, y = value, color = Regime_principal)) +
  geom_boxplot() + theme(legend.position = "none")
p03 <- ggplot(dfmelt_mtx3, aes(x = variable, y = value, color = Genre)) +
  geom_boxplot() + theme(legend.position = "none")
p003 <- ggplot(dfmelt_mtx3, aes(x = variable, y = value, color = Code_Station)) +
  geom_boxplot() + theme(legend.position = "none")
p0003 <- ggplot(dfmelt_mtx3, aes(x = variable, y = value, color = Groupe_station)) +
  geom_boxplot() + theme(legend.position = "none")

# Pr chq metal, concentrations selon regime trophique
grid.arrange(p2, p3, p1, ncol = 3, nrow = 1, widths = c(6, 2, 2), heights = c(1, 1))

# Pr chq metal, concentrations selon genre. Beaucoup de donnees ; difficile de s'y retrouver
grid.arrange(p02, p03, p01, ncol = 3, nrow = 1, widths = c(6, 2, 2), heights = c(1, 1))

# Pr chq metal, concentration selon station
grid.arrange(p002, p003, p001, ncol = 3, nrow = 1, widths = c(6, 2, 2), heights = c(1, 1))

# Pr chq metal, concentration selon groupe de stations
grid.arrange(p0002, p0003, p0001, ncol = 3, nrow = 1, widths = c(6, 2, 2), heights = c(1, 1))


#
dfmelt_mtx <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Hg_ppm","Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
p <- ggplot(na.omit(dfmelt_mtx), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p8 <- ggplot(na.omit(dfmelt_mtx), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()
dfmelt_mt0 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
p_0 <- ggplot(na.omit(dfmelt_mt0), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p_8 <- ggplot(na.omit(dfmelt_mt0), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()
dfmelt_mt <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Zn_ppm"))
p0 <- ggplot(na.omit(dfmelt_mt), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p88 <- ggplot(na.omit(dfmelt_mt), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()
dfmelt_m <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Cu_ppm", "Se_ppm"))
p00 <- ggplot(na.omit(dfmelt_m), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p888 <- ggplot(na.omit(dfmelt_m), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()

# Pr chq genre, contamination metallique. Avec Hg 
grid.arrange(p0, p00, p, ncol = 1, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# Pr chq genre, contamination metallique. Sans Hg ; un peu plus lisible
grid.arrange(p0, p00, p_0, ncol = 1, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# Pr chq sp, contamination metallique. Sans Hg ; un peu plus lisible
grid.arrange(p88, p888, p_8, ncol = 1, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# Pr chq sp, contamination metallique. Avec Hg
grid.arrange(p88, p888, p8, ncol = 1, nrow = 3, widths = c(1, 1), heights = c(1, 1))





##___##



# Isotopie : Repartition generale


scat <- ggplot(BDD_PME, aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = Regime_principal, shape = Regime_principal)) + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1)) #+
#scale_x_continuous(limits = c(- 35, - 24)) + 
#scale_y_continuous(limits = c(5, 15))

plot_t <- ggplot(BDD_PME, aes(d13C, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD_PME, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

# d15n = f d13C
grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

##

scat <- ggplot(BDD_PME, aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = Regime_alter)) + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1)) #+
#scale_x_continuous(limits = c(- 35, - 24)) + 
#scale_y_continuous(limits = c(5, 15))

plot_t <- ggplot(BDD_PME, aes(d13C, fill = Regime_alter)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD_PME, aes(d15N, fill = Regime_alter)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

# d15n = f d13C
grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))



##___##

# Relation [Hg] = f d15N

### Muscle
scat <- ggplot(BDD_PME, aes(x = d15N, y = conc_Hg_muscle_ppm, colour = Regime_principal, shape = Regime_principal)) +
  geom_point() + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1))

plot_t <- ggplot(BDD_PME, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD_PME, aes(conc_Hg_muscle_ppm, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

### Foie
scat <- ggplot(BDD_PME, aes(x = d15N, y = conc_Hg_foie_ppm, colour = Regime_principal, shape = Regime_principal)) +
  geom_point() + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1))

plot_t <- ggplot(BDD_PME, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD_PME, aes(conc_Hg_foie_ppm, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

### Branchie
scat <- ggplot(BDD_PME, aes(x = d15N, y = conc_Hg_branchie_ppm, colour = Regime_principal, shape = Regime_principal)) +
  geom_point() + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1))

plot_t <- ggplot(BDD_PME, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD_PME, aes(conc_Hg_branchie_ppm, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

# Rq : les 3 distributions projetees en haut ne sont pas tout a fait correctes : il s'agit de l'ensemble des valeurs de d15N et non pas des valeurs ayant des [Hg] mesurees



###### 26/03/2014 #####



str(BDD_PME)

dfmelt <- melt(BDD_PME, id.vars = c("Groupe_station", "Code_Station", "Site", "Regime_principal", "Regime_alter"), measure.vars = c("Hg_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm"))

dfmelt$Regime_alter <- factor(dfmelt$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

dfmelt$Regime_principal <- factor(dfmelt$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))


# normes conta sous forme de lignes ajoutees a chaque graphe

hline.data <- data.frame(z = c(2.5, 1, 2, 5, 15, 40, 0.5, 5, 0.05, 1) , variable = c("Hg_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm"))

hline.data <- data.frame(z = c(2.5, 0.5, 2.5) , variable = c("Hg_ppm", "Cd_ppm", "Pb_ppm"))

# Pr chq site, difference selon omni/carni en fction du metal

site <- ggplot(na.omit(dfmelt[dfmelt$Regime_principal == c("Carnivore", "Omnivore"),]), aes(x = Code_Station, y = value, color = Regime_principal)) +
  geom_boxplot() +
  facet_grid(variable~., scales = "free_y") +
  geom_hline(aes(yintercept = z), hline.data)

site + geom_hline(aes(yintercept = z), hline.data)

# Pr chq groupe de site, difference selon regime en fction du metal

groupe_site <- ggplot(na.omit(dfmelt), aes(x = Groupe_station, y = value, color = Regime_principal)) +
  geom_boxplot() +
  facet_grid(variable~., scales = "free_y")

groupe_site + geom_hline(aes(yintercept = z), hline.data)


# Pr chq groupe de site, difference selon regime en fction du metal

groupe_site_detail <- ggplot(na.omit(dfmelt), aes(x = Groupe_station, y = value, color = Regime_alter)) +
  geom_boxplot() +
  facet_grid(variable~., scales = "free_y")

groupe_site_detail + geom_hline(aes(yintercept = z), hline.data)

##################### tests comparaisons multiples ################
require(nortest)

tapply(dfmelt$value, dfmelt$Code_Station, shapiro.test)

require(agricolae)

comp <- kruskal(dfmelt$value, dfmelt$Groupe_station, alpha = 0.05)

norm <- NULL

for(i in BDD_PME$Groupe_station){
  for (j in BDD_PME$Regime_principal){
    norm[i+j] <- shapiro.test(BDD_PME$Se_ppm)
  }
  
}

for(i in BDD_PME$Groupe_station){
  norm[i] <- shapiro.test(BDD_PME[BDD_PME$Groupe_station == "i", ]$Se_ppm)
}


shapiro.test(BDD_PME$As_ppm)
?friedman.test()
#################################################################


# Details pour chq metal


sub_BDD_PME <- BDD_PME[!(is.na(BDD_PME$Cr_ppm)), ]

sub_BDD_PME$Regime_alter <- factor(sub_BDD_PME$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

sub_BDD_PME$Regime_principal <- factor(sub_BDD_PME$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))

# Hg

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Hg_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

Hg <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Hg_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Hg_ppm, color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

Hg <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = pds_g, color = Regime_principal)) +
  geom_boxplot()

ggplot(BDD_PME[BDD_PME$Site =="Camopi",], aes(x = station, y= conc_Hg_muscle_ppm , color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

# a imprimer !
ggplot(BDD_PME, aes(x = Groupe_station, y = conc_Hg_muscle_ppm, color = Regime_alter)) + geom_boxplot()

# Il y a une couleur qui ne devrait pas apparaitre, comme si il y avait encore des valeurs manquantes...


# Cd

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Cd_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 0.5))

Cd <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cd_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 0.5))

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cd_ppm, color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 0.5))

# Pb

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Pb_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

Pb <-ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Pb_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 1.5))

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Pb_ppm, color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

# Cr

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Cr_ppm, color = Regime_principal)) +
  geom_boxplot()

Cr <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cr_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cr_ppm, color = Regime_alter)) +
  geom_boxplot()

# a imprimer
ggplot(sub_BDD_PME, aes(x = Site, y = Cr_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Site, y = Cr_ppm)) +
  geom_boxplot()


# Co

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Co_ppm, color = Regime_principal)) +
  geom_boxplot()

Co <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Co_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Co_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD_PME[sub_BDD_PME$Site == "Saul",], aes(x= pds_g, y = Co_ppm, color = Regime_principal)) + geom_point()

# Ni

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Ni_ppm, color = Regime_principal)) +
  geom_boxplot()

Ni <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Ni_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Ni_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Site, y = Ni_ppm, color = Regime_principal)) +
  geom_boxplot()

# Cu

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Cu_ppm, color = Regime_principal)) +
  geom_boxplot()

Cu <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cu_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cu_ppm, color = Regime_alter)) +
  geom_boxplot()

# Zn

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Zn_ppm, color = Regime_principal)) +
  geom_boxplot()

Zn <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Zn_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Zn_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Site, y = Zn_ppm, color = Regime_principal)) +
  geom_boxplot()

# As

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = As_ppm, color = Regime_principal)) +
  geom_boxplot()

As <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = As_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Groupe_station, y = As_ppm, color = Regime_alter)) +
  geom_boxplot()

# Se

ggplot(sub_BDD_PME[sub_BDD_PME$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Se_ppm, color = Regime_principal)) +
  geom_boxplot()

Se <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Se_ppm, color = Regime_principal)) +
  geom_boxplot()

Se <- ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Se_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD_PME, aes(x = Site, y = Se_ppm, color = Regime_alter)) +
  geom_boxplot()


# fichier pdf

pdf("2014_03_27_As_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(As)
dev.off()

pdf("2014_03_27_Cd_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Cd)
dev.off()

pdf("2014_03_27_Co_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Co)
dev.off()

pdf("2014_03_27_Cu_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Cu)
dev.off()

pdf("2014_03_27_Cr_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Cr)
dev.off()

pdf("2014_03_27_Ni_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Ni)
dev.off()

pdf("2014_03_27_Pb_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Pb)
dev.off()

pdf("2014_04_08_Se_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Se)
dev.off()

pdf("2014_03_27_Zn_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Zn)
dev.off()

pdf("2014_03_28_masse_groupe_station.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier
print(Hg)
dev.off()


##### TESTS DIVERS

# ACP


require(FactoMineR)


res.pca = PCA(na.omit(sub_BDD_PME[,c(37, 38, 53:62)]), scale.unit=TRUE, ncp=5, graph=T) 

dimdesc(res.pca, axes=c(1,2))

require(ade4)

acp <- dudi.pca(na.omit(sub_BDD_PME[,c(37, 38, 52:61)]), scannf = TRUE)


# Comparisons

tapply(sub_BDD_PME$Se_ppm, sub_BDD_PME$Regime_principal, shapiro.test)

sub_BDD_PME$inter <- interaction(sub_BDD_PME$Groupe_station, sub_BDD_PME$Regime_principal)

require (agricolae)

comparison <- kruskal(sub_BDD_PME$Hg_ppm, sub_BDD_PME$inter, alpha = 0.05)

compariso <- kruskal(sub_BDD_PME$Se_ppm, sub_BDD_PME$inter, alpha = 0.05)

require(pgirmess)

compa <- kruskalmc(sub_BDD_PME$Se_ppm, sub_BDD_PME$inter) # Test post-hoc 


ano <- aov(sub_BDD_PME$Se_ppm~sub_BDD_PME$Regime_principal+sub_BDD_PME$Groupe_station) ; summary(ano)

ggplot(sub_BDD_PME, aes(y = Hg_ppm, x = Regime_principal)) + geom_boxplot()


box.cox <- function(x, lambda) {
  if (lambda==0) log(x) else ((x)^lambda - 1)/lambda
}


fligner.test(Se_ppm~inter, data = sub_BDD_PME)
sub_BDD_PME$boxcox_Se <- box.cox(sub_BDD_PME$Se_ppm, 1/2) # transformation box cox pour lambda = 1/2
fligner.test(boxcox_Se~inter, data = sub_BDD_PME) # Homogénéité des variances
tapply(sub_BDD_PME$boxcox_Se, sub_BDD_PME$inter, shapiro.test)
qplot(sample = boxcox_Se, data = sub_BDD_PME, colour = factor(inter))

# message d'erreur de type Error in FUN(X[[1L]], ...) : sample size must be between 3 and 5000
# peut etre cause par presence de NA, ce qui est le cas pour le mercure !!!
# Mais pas d'explication pour Se...



# Test Ordinal Logistic Regression

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

lapply(sub_BDD_PME[, c("Groupe_station", "Regime_principal")], table)
ftable(xtabs(~ Groupe_station + Regime_principal, data = sub_BDD_PME))
m <- polr(Se_ppm ~ Groupe_station + Regime_principal, data = sub_BDD_PME, Hess = TRUE)
summary(m)
# [Se] n'est pas un facteur...

# Test Friedman


sub_BDD_PME$Groupe_station <- as.factor(sub_BDD_PME$Groupe_station)

friedman.test(Se_ppm ~ Regime_principal | Groupe_station, data = sub_BDD_PME)

str(sub_BDD_PME)
# not an unreplicated complete block design


# Rank test on interaction
sub_BDD_PME <- select(BDD_PME, Se_ppm, Regime_principal, Groupe_station)


?rank()
sub_BDD_PME$rank <- rank(sub_BDD_PME$inter)

fligner.test(Se_ppm ~ Regime_principal*Groupe_station, data = sub_BDD_PME)
leveneTest(Se_ppm ~ Regime_principal*Groupe_station, data = sub_BDD_PME)

tapply(sub_BDD_PME$Se_ppm, sub_BDD_PME$Regime_principal, shapiro.test)
tapply(sub_BDD_PME$Se_ppm, sub_BDD_PME$Groupe_station, shapiro.test)

# Comment faire pour obtenir des resultats plus precis ?
require(dplyr)
Ch_cont <- filter(sub_BDD_PME, Groupe_station == "Chien_conta")
fligner.test(Se_ppm ~ Regime_principal, data = Ch_cont)
tapply(Ch_cont$Se_ppm, Ch_cont$Regime_principal, shapiro.test)


sub_BDD_PME$inter <- interaction(sub_BDD_PME$Groupe_station, sub_BDD_PME$Regime_principal)
fligner.test(Se_ppm ~ inter, data = sub_BDD_PME)
tapply(sub_BDD_PME$Se_ppm, sub_BDD_PME$inter, shapiro.test)


lm1 <- lm(rank(sub_BDD_PME$Se_ppm) ~ sub_BDD_PME$Groupe_station * sub_BDD_PME$Regime_alter)

anolm1 <- anova(lm1)
anolm1

MS <- anolm1[1:3,1:3]
MS

MS[,4] <- MS[,3]/(length(rat)+1) #le test H

MS[,5] <- (1-pchisq(MS[,4],MS[,1]))

colnames(MS)[4:5] <- c("H","pvalue")

MS



# Realisation des comparaisons multiples ?
require(agricolae)
comp <- kruskal(sub_BDD_PME$Se_ppm, sub_BDD_PME$inter, alpha = 0.05)
comp$groups

require(pgirmess)
compa <- kruskalmc()
?kruskalmc()

# Test FAMD

require(FactoMineR)
df.famd <- select(sub_BDD_PME, Groupe_station, Regime_alter, Se_ppm)
na.famd <- FAMD(na.omit(df.famd))
summary(na.famd)

###
df <- select(BDD_PME, Groupe_station, Regime_principal, ordre, d13C, d15N, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm)
famd <- FAMD(na.omit(df))
summary(famd)
famd

df2 <- select(BDD_PME, Regime_principal, d15N, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm)
famd2 <- FAMD(na.omit(df2))
summary(famd2)
plot(famd2, habillage = 1)

df3 <- select(BDD_PME, Regime_principal, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm)
famd3 <- FAMD(na.omit(df3))
summary(famd3)
plot(famd3, habillage = 1)


####### Quel regroupement est il possible de faire au niveau des especes ? #########

#d.melt <- melt(BDD_PME, id.vars = c("Groupe_station", "Regime_principal", "Code_Station", "d15N"), measure.vars = c("conc_Hg_muscle_ppm", "conc_Hg_foie_ppm", "conc_Hg_branchie_ppm"))
#carn <- na.omit(d.melt[d.melt$Regime_principal == "Carnivore",], )
#sBDD_PME <- BDD_PME[!(is.na(BDD_PME$Genre)), ]

# Chien non contamine

ggplot(BDD_PME[BDD_PME$Groupe_station %in% "Chien_non_conta" & BDD_PME$Regime_principal %in% c("Carnivore", "Omnivore"), ], aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")

### Quelle difference entre %in% et == lorsque utilisation de subset ?

ggplot( filter(BDD_PME, Groupe_station %in% "Chien_non_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Genre, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# equivalent a la selection precedente.
# regroupement de CCAR, PFIL, SMAC, RIVULUS SP, RLUN ?, BHYP ?

# Chien contamine

ggplot( filter(BDD_PME, Groupe_station %in% "Chien_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# JKEI

# NF contamine

ggplot( filter(BDD_PME, Groupe_station %in% "NF_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# JKIR ?

# NF non contamine

ggplot( filter(BDD_PME, Groupe_station %in% "NF_non_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# PBRE, MRAR, RGUY, JABR

# Vue d'ensemble carnivores
se <- function(x) sqrt(var(x)/length(x))

df.sp <- filter(BDD_PME, Regime_principal %in% "Carnivore") %.% # Selection BDD_PME
  group_by(Code) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)))  # Selection donnees a  calculer
#df.sp <- na.omit(merge(df.sp, select(filter(BDD_PME, Regime_principal %in% "Carnivore"), ordre, Code), by = 'Code'))

ggplot( filter(BDD_PME, Regime_principal %in% "Carnivore"), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code)) +
  geom_point(aes(shape = ordre)) +
  geom_point(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code), size = 4) +
  geom_text(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code, label = Code), hjust = 1, vjust = -1) +
  geom_errorbarh(data = df.sp, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Code), width = .1)


# Vue d'ensemble omnivores
se <- function(x) sqrt(var(x)/length(x))

df.sp <- filter(BDD_PME, Regime_principal %in% "Omnivore") %.% # Selection BDD_PME
  group_by(Code) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)))  # Selection donnees a  calculer
#df.sp <- na.omit(merge(df.sp, select(filter(BDD_PME, Regime_principal %in% "Carnivore"), ordre, Code), by = 'Code'))

ggplot( filter(BDD_PME, Regime_principal %in% "Omnivore"), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code)) +
  geom_point(aes(shape = ordre)) +
  geom_point(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code), size = 4) +
  geom_text(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code, label = Code), hjust = 1, vjust = -1) +
  geom_errorbarh(data = df.sp, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Code), width = .1)



# Vue d'ensemble tous poissons selon sp
df.sp.muscle <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)), ] %.% # Selection BDD_PME
  group_by(Code) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Selection donnees a  calculer
# Toujours la meme question : comment faire pour appliquer "summarise" sur un facteur et ainsi ajouter l'information Regime_alimenaire pour chaque Code...

df.sp.muscle <- na.omit(merge(df.sp.muscle, select(BDD_PME, Regime_alter, Code), by = 'Code')) # Solution potentielle. Malheureusement, nombreux duplicats

df.sp.muscle <- df.sp.muscle[!duplicated(df.sp.muscle[,'Code']),] # Sélectionne uniquement un ligne par code uniques. Comme toutes les valeurs sont dupliquées, la ligne selectionnée n'a pas d'importance


ggplot( df.sp.muscle, aes(x = d15N, y = conc_Hg_muscle_ppm, color = Regime_alter)) +
  #geom_point(aes(shape=ordre)) +
  geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = Code), hjust=1, vjust=-1) +
  geom_errorbarh(data = df.sp.muscle, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .1) + 
  geom_errorbar(data = df.sp.muscle, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .1)

ggplot( BDD_PME, aes(x = d15N, y = conc_Hg_muscle_ppm, color = Regime_alter)) +
 # geom_point(alpha = 0.6) +
  geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = Code), hjust=1, vjust=-1) 
  #geom_errorbarh(data = df.sp.muscle, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .1) + 
  #geom_errorbar(data = df.sp.muscle, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .1)




ggplot( BDD_PME, aes(x = d13C, y = d15N, color = Code)) +
  #geom_point(aes(shape=ordre)) +
  geom_point(data = df.sp.muscle, aes(y = d15N_mean, x = d13C_mean, color = Code), size = 4) +
  geom_text(data = df.sp.muscle, aes(y = d15N_mean, x = d13C_mean, color = Code, label = Code), hjust=1, vjust=-1) +
  geom_errorbarh(data = df.sp.muscle, aes(xmin = d13C_mean + d13C_se, xmax = d13C_mean - d13C_se, y = d15N_mean, x = d13C_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp.muscle, aes(ymin = d15N_mean - d15N_se, ymax = d15N_mean + d15N_se, x = d13C_mean, y = d15N_mean, colour = Code), width = .1)


# Vue d'ensemble tous poissons selon regime
df.reg <- BDD_PME %.% # Selection BDD_PME
  group_by(Regime_alter) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)), Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Selection donnees a  calculer


ggplot(BDD_PME, aes(x = d13C, y = d15N)) +
  geom_point(data = df.sp, aes(x = d13C_mean, y = d15N_mean, fill = Code), show_guide = FALSE) +
  geom_point(data = df.reg, aes(y = d15N_mean, x = d13C_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg, aes(y = d15N_mean, x = d13C_mean, color = Regime_alter, label = Regime_alter), hjust = 1.02, vjust = -1, size = 6.5) +
  geom_errorbarh(data = df.reg, aes(xmin = d13C_mean + d13C_se, xmax = d13C_mean - d13C_se, y = d15N_mean, x = d13C_mean, colour = Regime_alter), height = .025) + 
  geom_errorbar(data = df.reg, aes(ymin = d15N_mean - d15N_se, ymax = d15N_mean + d15N_se, x = d13C_mean, y = d15N_mean, colour = Regime_alter), width = .05)


df.reg.muscle <- BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)), ] %.% # Selection BDD_PME
  group_by(Regime_alter) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Selection donnees a  calculer

df.reg.muscle <- df.reg.muscle[- nrow(df.reg.muscle),] # Enlève la dernière ligne ("inconnu")

# Avec na.omit car certains régimes n'ont pas toutes les informations ([Hg] et d15N)
pl1 <- ggplot(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
 # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
  geom_point(data = df.reg.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = Regime_alter), hjust=1.02, vjust=-1, size = 6.5) +
  geom_errorbarh(data = df.reg.muscle, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
  geom_errorbar(data = df.reg.muscle, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05)




df.reg.foie <- BDD_PME[!(is.na(BDD_PME$conc_Hg_foie_ppm)) & !(is.na(BDD_PME$d15N)), ] %.% # Selection BDD_PME
  group_by(Regime_alter) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)), Hg_foie_se = se(na.omit(conc_Hg_foie_ppm))) # Selection donnees a  calculer

df.reg.foie <- na.omit(df.reg.foie)

pl2 <- ggplot( BDD_PME[!(is.na(BDD_PME$conc_Hg_foie_ppm)), ], aes(x = d15N, y = conc_Hg_foie_ppm)) +
 # geom_point(aes(color = Regime_alter), alpha = 0.65) +
  geom_point(data = df.reg.foie, aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg.foie, aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter, label = Regime_alter), hjust=1, vjust=-1, size = 6.5) +
  geom_errorbarh(data = df.reg.foie, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_foie_mean, x = d15N_mean, colour = Regime_alter), height = .05) + 
  geom_errorbar(data = df.reg.foie, aes(ymin = Hg_foie_mean - Hg_foie_se, ymax = Hg_foie_mean + Hg_foie_se, x = d15N_mean, y = Hg_foie_mean, colour = Regime_alter), width = .05)


df.reg.branchie <- BDD_PME[!(is.na(BDD_PME$conc_Hg_branchie_ppm)) & !(is.na(BDD_PME$d15N)), ] %.% # Selection BDD_PME
  group_by(Regime_alter) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Selection donnees a  calculer

df.reg.branchie <- na.omit(df.reg.branchie)

pl3 <- ggplot( BDD_PME[!(is.na(BDD_PME$conc_Hg_branchie_ppm)), ], aes(x = d15N, y = conc_Hg_branchie_ppm)) +
#  geom_point(aes(color = Regime_alter), alpha = 0.65) +
  geom_point(data = df.reg.branchie, aes(x = d15N_mean, y = Hg_branchie_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg.branchie, aes(x = d15N_mean, y = Hg_branchie_mean, color = Regime_alter, label = Regime_alter), hjust=1, vjust=-1, size = 6.5) +
  geom_errorbarh(data = df.reg.branchie, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_branchie_mean, x = d15N_mean, colour = Regime_alter), height = .05) + 
  geom_errorbar(data = df.reg.branchie, aes(ymin = Hg_branchie_mean - Hg_branchie_se, ymax = Hg_branchie_mean + Hg_branchie_se, x = d15N_mean, y = Hg_branchie_mean, colour = Regime_alter), width = .05)




legend <- g_legend(pl1)

grid.arrange(pl1, pl2, pl3, ncol = 1, nrow = 3) # Basic

grid.arrange(arrangeGrob(pl1 + theme(legend.position="none"),
                         pl2 + theme(legend.position="none"),
                         pl3 + theme(legend.position="none"),
                         ncol = 1),
             legend, ncol = 2, nrow = 1, widths = c(9, 1), heights = c(1, 1))


# Conta Hg globale sur Camopi, Saul et 3 Sauts

ggplot(BDD_PME, aes( x = station, y = conc_Hg_muscle_ppm, color = Regime_alter)) +
  geom_boxplot()






########### Test rigoureux ANOVA 08/04/2014 ########################


anova <- aov(Se_ppm ~ Regime_alter * Groupe_station, data = sub_BDD_PME)
summary(anova)


BDD_PME.res = sub_BDD_PME
BDD_PME.res$Fit = fitted(anova)
BDD_PME.res$Resid = resid(anova)

shapiro.test(BDD_PME.res$Resid)

# Distribution plots of the residuals
ggplot(BDD_PME.res, aes(x = Resid, y = ..density..)) + geom_histogram(binwidth = 0.15)

# Normal Probability Plot
p <- ggplot(BDD_PME.res, aes(sample = Resid)) + stat_qq()

#QQ-plot pr chq groupe
p + facet_grid(. ~ Groupe_station)
ggplot(BDD_PME.res[BDD_PME.res$Regime_alter %in% c("Carnivore_Piscivore", "Carnivore_Invertivore", "Omnivore_Invertivore"),], aes(sample = Resid)) + stat_qq() + facet_grid(. ~ Regime_alter)

# Residuals vs Fitted
ggplot(BDD_PME.res, aes(Fit, Resid, colour = Regime_alter, shape = Groupe_station)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

# Residuals vs groupes de station
ggplot(BDD_PME.res, aes(x = Groupe_station, Resid, colour = Regime_alter, shape = Groupe_station)) + geom_point() +
  xlab("Groupe de stations") + ylab("Residuals")

# Residuals vs Regimes
ggplot(BDD_PME.res, aes(x = Regime_alter, Resid, colour = Regime_alter, shape = Groupe_station)) + geom_point() +
  xlab("Regimes") + ylab("Residuals")

# non respect des assumptions
# test avec transformations des donnees en boxcox et en log : toujours non respect. Besoin d'un test non param



# Test ANOVA uniquement avec les 3 regimes presents en abondance : Piscivores et Invertivores.

aov.df <- sub_BDD_PME[sub_BDD_PME$Regime_alter %in% c("Carnivore_Piscivore", "Carnivore_Invertivore", "Omnivore_Invertivore"),]

anova <- aov(Se_ppm ~ Regime_alter * Groupe_station, data = aov.df)
summary(anova)

BDD_PME.res = aov.df

# toujours non correct

# too much skewness or potential for outliers ? -> non-parametric methods


# transformation boxcox
require(MASS)

boxcox(Se_ppm ~ Regime_alter * Groupe_station, data= sub_BDD_PME, lambda = seq(-1.5, 1.5, by = .1), plotit = TRUE)
# lambda optimal = ~ 0.5
boxcox(Se_ppm ~ Regime_alter * Groupe_station, data= sub_BDD_PME, lambda = seq(0.4, 0.7, length = 25), plotit = FALSE )
# lambda optimal = 0.5250

lambda <- 0.5250

# Transformation
sub_BDD_PME$boxcoxSe <- ((sub_BDD_PME$Se_ppm^lambda)-1)/lambda

# comparaison des QQ plots. Probleme
par(mfrow=c(2, 1))
qqnorm(sub_BDD_PME$boxcoxSe)
qqnorm(sub_BDD_PME$Se_ppm)


boxcox.anova <- aov(boxcoxSe ~ Regime_alter * Groupe_station, data = sub_BDD_PME)

summary(boxcox.anova)

BDD_PME.res = sub_BDD_PME
BDD_PME.res$Fit = fitted(boxcox.anova)
BDD_PME.res$Resid = resid(boxcox.anova)

# Toujours pas bon meme apres boxcox


# Test pour Cr

plot(density(sub_BDD_PME$Cr_ppm))

anova <- aov(Cr_ppm ~ Regime_alter * Groupe_station, data = sub_BDD_PME)
summary(anova)

# pas bon sans transfo

plot(density(log(sub_BDD_PME$Cr_ppm)))

boxcox(Cr_ppm ~ Regime_alter * Groupe_station, data= sub_BDD_PME, lambda = seq(-1.5, 1.5, by = .1), plotit = TRUE)
# lambda optimal = ~ 0.4
boxcox(Cr_ppm ~ Regime_alter * Groupe_station, data= sub_BDD_PME, lambda = seq(0.25, 0.5, length = 30), plotit = FALSE )
# lambda optimal = 0.36

lambda <- 0.36

sub_BDD_PME$boxcoxCr <- ((sub_BDD_PME$Cr_ppm^lambda)-1)/lambda

boxcox.anova <- aov(boxcoxCr ~ Regime_alter * Groupe_station, data = sub_BDD_PME)

summary(boxcox.anova)

BDD_PME.res = sub_BDD_PME
BDD_PME.res$Fit = fitted(boxcox.anova)
BDD_PME.res$Resid = resid(boxcox.anova)

# Toujours pas bon meme apres la transformation boxcox.

# Il faut reellement trouver un equivalent non parametrique a une ANOVA factorielle


######### TeachingDemos ########

require(TeachingDemos)

SnowsPenultimateNormalityTest(sub_BDD_PME$Se_ppm)

vis.test()

test.visu <- function (x) {
  if(interactive()) {
    vis.test(x, vt.qqnorm)
    vis.test(x, vt.normhist)
  }
}

vis.test(BDD_PME.res$Fit, BDD_PME.res$Resid, vt.scatterpermute)


##### Siluriformes etaient plus contamines en Se a Nouvelle-France #####


BDD_PME[["Siluriforme"]] = ifelse(BDD_PME[["ordre"]] == "Siluriformes" & BDD_PME[["Regime_principal"]] == "Carnivore", "Siluriformes_Carnivores", NA)

BDD_PME[["Siluriformes"]] = ifelse(BDD_PME[["ordre"]] == "Siluriformes" & BDD_PME[["Regime_principal"]] == "Carnivore" & !is.na(BDD_PME$Se_ppm), "Siluriformes_Carnivores", NA)


length(which(!is.na(BDD_PME$Siluriformes) & !is.na(BDD_PME$Se_ppm)))

dfmelt_mtx1 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes", "Siluriforme"), measure.vars = c("Zn_ppm"))
dfmelt_mtx2 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes", "Siluriforme"), measure.vars = c("Hg_ppm","Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
dfmelt_mtx3 <- melt(BDD_PME, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes", "Siluriforme"), measure.vars = c("Cu_ppm", "Se_ppm"))
df.melt <- melt(BDD_PME, id.vars = c("Siluriformes", "Siluriforme"), measure.vars = c("conc_Hg_muscle_ppm", "conc_Hg_foie_ppm", "conc_Hg_branchie_ppm"))

ggplot(BDD_PME, aes(x = d15N, y = Hg_ppm , color = Siluriforme)) +
  geom_point()

p <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Siluriforme )) +
  geom_point(position = "jitter")
pp <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Siluriforme )) +
  geom_point(position = "jitter") + theme(legend.position = "none")
ppp <- ggplot(dfmelt_mtx3, aes(x = variable, y = value, color = Siluriforme )) +
  geom_point(position = "jitter") + theme(legend.position = "none")

grid.arrange(pp, ppp, p, ncol = 3, nrow = 1, widths = c(7, 2, 2), heights = c(1, 1))

ggplot(BDD_PME, aes(ls_mm, pds_g, color = Siluriformes)) +
  #geom_point(aes(shape = Siluriforme), alpha = 0.8) +
  geom_point(alpha = 0.8) + # alternative a la ligne precedente
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 200))

ggplot(BDD_PME, aes(d13C, d15N, color = Siluriformes)) +
  geom_point(aes(shape = Siluriforme)) 

ggplot(df.melt, aes(x = variable, y = value, color = Siluriforme)) +
  geom_point(position = "jitter")


ggplot(BDD_PME, aes(x = Se_ppm, y = Hg_ppm, color = Regime_alter)) +
  geom_point()

BDD_PME$HgSe_ratio <- BDD_PME$Hg_ppm / BDD_PME$Se_ppm

ggplot(BDD_PME, aes(y = HgSe_ratio, x = Regime_alter)) +
  geom_boxplot() +  scale_y_continuous(limits = c(0, 2.5)) # un point aberrant car pas de valeurs pour les LQ pour l'instant

na.omit(BDD_PME$HgSe_ratio)

######### Analyses Multivariees ##########



require(FactoMineR)


sub.df <- na.omit(sub_BDD_PME[,c(40, 41, 56:65)])
scale.df <- scale(sub.df)
colMeans(scale.df)
apply(scale.df, 2, sd)


res.pca = PCA(na.omit(sub_BDD_PME[,c(40, 41, 56:65)]), scale.unit=TRUE, ncp=5, graph=T) 
res.pca = PCA(na.omit(sub_BDD_PME[,c(56:66)]), scale.unit=TRUE, ncp=5, graph=T)
res.pca = PCA((BDD_PME[,c(49, 51, 53, 40, 41, 56:65)]), scale.unit=TRUE, ncp=5, graph=T) 
res.pca = PCA(sub_BDD_PME[,c(40, 41, 56:65)], scale.unit=TRUE, ncp=5, graph=T, quali.sup = 10) # NA = column mean here
res.pca = PCA(scale.df, scale.unit=TRUE, ncp=5, graph=T) # Inutile car deja argument scale dans la fonction ; meme resultat obtenu dc rassurant

plot.PCA(res.pca, axes = c(1:3))

dimdesc(res.pca, axes=c(1,2))



require(ade4)

acp <- dudi.pca(na.omit(sub_BDD_PME[,c(40, 41, 56:65)]), scannf = FALSE, nf = 2, center = TRUE, scale = TRUE)

par(mfrow = c(2,2))
s.class(acp$li, sub_BDD_PME$Groupe_station, cpoint = 1)
s.arrow(acp$c1, lab = names(deug$tab))
s.class(acp$li, deug$result, cpoint = 1)
s.corcircle(acp$co, full = TRUE, box = TRUE)

scatter(acp, clab.row = 0, posieig = "none") # fonctionne
s.class(acp$li, sub_BDD_PME$Groupe_station, add.plot = TRUE)


###### MAtrice de correlation entre les differentes variables

mcor <- cor((BDD_PME[,c(40, 41, 56:65)]), use="complete.obs")
mat <- (BDD_PME[,c(40, 41, 56:65)])

# Print mcor and round to 2 digits
round(mcor, digits=2)

require(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", addcolorlabel="no", order="FPC")

# Affichage p-value

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(mat, 0.95)
## specialized the insignificant value according to the significant level
corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05)
corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05, insig = "pch", method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", addcolorlabel="no", order="FPC")


# Autre methode alternative
# http://stackoverflow.com/questions/19012529/correlation-corrplot-configuration

require(corrgram)

panel.shadeNtext <- function (x, y, corr = NULL, col.regions, ...) 
{
  corr <- cor(x, y, use = "pair")
  results <- cor.test(x, y, alternative = "two.sided")
  est <- results$p.value
  stars <- ifelse(est < 5e-4, "***", 
                  ifelse(est < 5e-3, "**", 
                         ifelse(est < 5e-2, "*", "")))
  ncol <- 14
  pal <- col.regions(ncol)
  col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
                                               length = ncol + 1), include.lowest = TRUE))
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind], 
       border = NA)
  box(col = "lightgray")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- formatC(corr, digits = 2, format = "f")
  cex.cor <- .8/strwidth("-X.xx")
  fonts <- ifelse(stars != "", 2,1)
  # option 1: stars:
  #text(0.5, 0.4, paste0(r,"\n", stars), cex = cex.cor)
  # option 2: bolding:
  text(0.5, 0.5, r, cex = cex.cor, font=fonts)
}

# Call the corrgram function with the new panel functions
# NB: call on the data, not the correlation matrix
corrgram(BDD_PME, type="data", lower.panel=panel.shadeNtext, 
         upper.panel=NULL)







#################### 11/04/2014 #####################

#### Contamination moyenne en Hg pour chq site

BDD <- read.csv("C:/Users/laurent/Desktop/Stage_Jerome_LAURENT/BDD_PME/2014_04_15_BDD_PME.csv", sep=";", stringsAsFactors = FALSE)


BDD$pds_g<- as.numeric(BDD$pds_g)

BDD$station <- as.factor(BDD$station)

BDD$Pression <- as.factor(BDD$Pression)

BDD$Orpaillage <- as.factor(BDD$Orpaillage)

BDD$Pression_anthro <- as.factor(BDD$Pression_anthro)

BDD$Regime_alter <- factor(BDD$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

BDD$Regime_principal <- factor(BDD$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))


# calcul de differentes infos
BDD.Hg <- BDD %.% # Selection BDD_PME
  group_by(station) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)), Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Selection donnees a  calculer

# calcul des moyennes pour chaque site
means <- aggregate(conc_Hg_muscle_ppm ~  station, BDD, mean)
means$conc_Hg_muscle_ppm <- round(means$conc_Hg_muscle_ppm, digits = 2)

# plot sur donnees ne comprenant pas de NA pour [Hg]muscle
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

BDD.ssNA <- completeFun(BDD, "conc_Hg_muscle_ppm")
BDD.sansNA <- BDD[!(is.na(BDD$conc_Hg_muscle_ppm)), ] # meme resultat que fonction precedente
BDD.sansNA$station <- droplevels(BDD.sansNA$station) # drop unused levels
levels(BDD.sansNA$station)
BDD.sansNA$station <- factor(BDD.sansNA$station, levels = c(1:54)) 


ftable(xtabs(~ BV + Regime_principal, data = BDD.sansNA))


# Distribution [Hg] muscle + valeur moyenne pr chq station

p0 <- ggplot(BDD.sansNA, aes(x = station , y = conc_Hg_muscle_ppm, color = Pression_anthro)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "red")

# OU

ggplot(BDD.sansNA, aes(x = station, y = conc_Hg_muscle_ppm, color = Regime_alter )) +
  geom_point(position = "jitter")

# Repartition des regimes sur chaque station
p10 <- ggplot(BDD.sansNA, aes(x = station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill") +
  scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54)) # Remplace nom de la station par un numero ; ordre alphabetique conserve
# repartition des regimes sur chaque station (sans prendre en compte le nb d'individus)
p20 <- ggplot(BDD.sansNA, aes(x = station)) +
  geom_bar(aes(fill = Regime_alter)) +
  scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54))
# repartition des regimes sur chaque station (en prenant en compte le nb d'individus)
grid.arrange(p10, p20, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))

p <- ggplot(BDD.sansNA, aes(x = station, y = pds_g, color = Regime_alter)) +
  geom_point(position = "jitter") + scale_y_continuous(limits = c(0, 250)) +
  scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54))
pp <- ggplot(BDD.sansNA, aes(x = station, y = ls_mm, color = Regime_alter )) +
  geom_point(position = "jitter") +
  scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54))
grid.arrange(p, pp, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))


grid.arrange(p0, p10, p, ncol = 1, nrow = 5, widths = c(1, 1), heights = c(1, 1))


legend <- g_legend(p10)


grid.arrange(arrangeGrob(p0 + theme(legend.position="none"),
                         p10 + theme(legend.position="none"),
                         p + theme(legend.position="none"),
                         ncol = 1),
             legend, ncol = 2, nrow = 1, widths = c(9, 1), heights = c(1, 1))



#### test non parametrique sur difference de concentrations en Hg dans les muscles selon les stations

require(agricolae)

# detach("package:MASS", unload=TRUE)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = BDD.sansNA) # Il esxiste des differences significatives


comparison <- kruskal(BDD.sansNA$conc_Hg_muscle_ppm, BDD.sansNA$station, alpha = 0.05, p.adj = "holm")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(BDD.sansNA, station, conc_Hg_muscle_ppm, Pression_anthro) # Subset plus simple a  manipuler

p0 <- ggplot(BD, aes(x = station , y = conc_Hg_muscle_ppm)) +
  geom_boxplot(aes(colour = Pression_anthro)) +
  #scale_colour_brewer(palette="Set3") +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
lettpos <- function(BD) boxplot(BD$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
test <- ddply(BD, .(station), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
test_f <- merge(test, posthoc, by.x = "station", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
colnames(test_f)[2] <- "upper"
colnames(test_f)[4] <- "signif"
test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") +
  scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54)) +
  scale_colour_discrete(breaks = levels(BD$Pression_anthro), labels = c("Agriculture", "Barrage", "Déforestation", "Orpaillage ancien", "Orpaillage illégal récent", "Orpaillage illégal récent", "Piste", "Référence")) +
  labs(colour = "Pression anthropique", y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec", x = "N° de la station", title = "[Hg] dans les muscles de poissons selon les stations")


pdf("Graph/Hgmuscle_stations.pdf", width = 20, height = 15) # la fction pdf enregistre directement ds le dossier
print(p0)
dev.off()


## Concentration selon impact anthropique :


BD <- select(BDD.sansNA, conc_Hg_muscle_ppm, Pression_anthro) # Subset plus simple a  manipuler

means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD, mean)
means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD) # Il esxiste des differences significatives

comparison <- kruskal(BD$conc_Hg_muscle_ppm, BD$Pression_anthro, alpha = 0.05, p.adj = "holm")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


p0 <- ggplot(BD, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
  geom_boxplot(aes(colour = Pression_anthro)) +
  #scale_colour_brewer(palette="Set3") +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
lettpos <- function(BD) boxplot(BD$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
test <- ddply(BD, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
colnames(test_f)[2] <- "upper"
colnames(test_f)[4] <- "signif"
test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") 
 


#### Repartition des concentrations selon regime alimentaire

## Omnivores(Omnivores Invertivores)

BDD.omni <- BDD.sansNA[BDD.sansNA$Regime_alter %in% "Omnivore_Invertivore",]


means.omni <- aggregate(conc_Hg_muscle_ppm ~  station, BDD.omni, mean)
means.omni$conc_Hg_muscle_ppm <- round(means.omni$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = BDD.omni)


comparison <- kruskal(BDD.omni$conc_Hg_muscle_ppm, BDD.omni$station, alpha = 0.05, p.adj = "holm")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(BDD.omni, station, conc_Hg_muscle_ppm) # Subset plus simple a  manipuler

p0 <- ggplot(BD, aes(x = station , y = conc_Hg_muscle_ppm)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means.omni, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
lettpos <- function(BD) boxplot(BD$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
test <- ddply(BD, .(station), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
test_f <- merge(test, posthoc, by.x = "station", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
colnames(test_f)[2] <- "upper"
colnames(test_f)[4] <- "signif"
test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54))


## Carnivores(Carnivores Invertivores)

BDD.carni <- BDD.sansNA[BDD.sansNA$Regime_alter %in% "Carnivore_Invertivore",]


means.carni <- aggregate(conc_Hg_muscle_ppm ~  station, BDD.carni, mean)
means.carni$conc_Hg_muscle_ppm <- round(means.carni$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = BDD.carni)


# PROBLEME avec test post hoc ; resultats aberrants
comparison <- kruskal(BDD.carni$conc_Hg_muscle_ppm, BDD.carni$station, alpha = 0.05, p.adj = "holm")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(BDD.carni, station, conc_Hg_muscle_ppm) # Subset plus simple a  manipuler

p0 <- ggplot(BD, aes(x = station , y = conc_Hg_muscle_ppm)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means.carni, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
lettpos <- function(BD) boxplot(BD$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
test <- ddply(BD, .(station), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
test_f <- merge(test, posthoc, by.x = "station", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
colnames(test_f)[2] <- "upper"
colnames(test_f)[4] <- "signif"
test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54))

######################### 16/04/2014 ###############################


####### Genres en communs entre toutes les stations #########

casted <- dcast(BDD.sansNA, Genre ~ station) # dcast = oppose de la fonction melt
# Ne donne pas le resultat attendu. On obtient un tableau de contingence avec le nb d'individu de chq genre au niveau de chq station
# et non pas une liste d'espece pour chaque station.

# Reduce(intersect, list(a, b , d)) # Du coup, ne fonctionne pas mais bonne synthaxe. Toujours utile au cas ou

# d'apres observation de casted, Moenkhausia semble etre l'espece la plus commune. Confirme par ligne suivante

sort(table(BDD.sansNA$Genre),decreasing=TRUE)[1:3] # 3 genres les plus communs


BDD.moenk <- BDD.sansNA[BDD.sansNA$Genre %in% "Moenkhausia",]

means.moenk <- aggregate(conc_Hg_muscle_ppm ~  station, BDD.moenk, mean)
means.moenk$conc_Hg_muscle_ppm <- round(means.moenk$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = BDD.moenk)


comparison <- kruskal(BDD.moenk$conc_Hg_muscle_ppm, BDD.moenk$station, alpha = 0.05, p.adj = "none")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(BDD.moenk, station, conc_Hg_muscle_ppm) # Subset plus simple a  manipuler

p0 <- ggplot(BD, aes(x = station , y = conc_Hg_muscle_ppm)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means.moenk, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
lettpos <- function(BD) boxplot(BD$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
test <- ddply(BD, .(station), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
test_f <- merge(test, posthoc, by.x = "station", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
colnames(test_f)[2] <- "upper"
colnames(test_f)[4] <- "signif"
test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(BDD.sansNA$station)), labels=c(1:54))


ggplot(BDD.moenk, aes(x = pds_g, y = conc_Hg_muscle_ppm)) +
  geom_point() + geom_smooth()


######################0000000000000########################

sort(table(sub_BDD$Genre),decreasing=TRUE)[1:3]

# Mohenkausia est également le genre le plus abondant au niveau des 3 stations de PME

ggplot(sub_BDD[sub_BDD$Genre %in% "Moenkhausia",], aes(x = ls_mm, y = pds_g)) +
  geom_point() +
  geom_smooth()

ggplot(sub_BDD[sub_BDD$Genre %in% "Moenkhausia",], aes(x = ls_mm, y = conc_Hg_muscle_ppm)) +
  geom_point() +
  geom_smooth()

ggplot(sub_BDD[sub_BDD$Genre %in% "Moenkhausia",], aes(x = pds_g, y = conc_Hg_muscle_ppm)) +
  geom_point() +
  geom_smooth()

ggplot(BDD, aes(y = conc_Hg_muscle_ppm, x = 1)) +
  geom_boxplot()

ggplot(BDD_PME, aes(y = conc_Hg_muscle_ppm, x = 1)) +
  geom_boxplot()



#######################Test régression########################



#r = lm(rank(sub_BDD_PME$Hg_ppm) ~ sub_BDD_PME$station + sub_BDD_PME$Regime_alter)
#r1 = anova(r)
#
#r = lm(rank(sub_BDD_PME$Hg_ppm) ~ sub_BDD_PME$station * sub_BDD_PME$Regime_alter)
#r1 = anova(r)
#
#
## test de Scheirer-Ray-Hare # Est-ce que le fait que le nombre d'échantillons ne soit pas constant pour chaque condition n'est pas problématique ?
#SHR<-function(rppm,rf,sf){
#  lm1 <- lm(rank(rppm)~rf*sf)
# anolm1 <- anova(lm1)
# MS <-  anolm1[1:3,1:3]
# MS[,4] <- MS[,2]/(length(rppm)*(length(rppm)+1)/12)
# MS[,5] <- (1-pchisq(MS[,4],MS[,1]))
# colnames(MS)[4:5] <- c(“H”,”pvalue”)
# MS
#}



#rb=aov(lm(rank(sub_BDD_PME$Hg_ppm) ~ sub_BDD_PME$station + sub_BDD_PME$Regime_alter))
#TukeyHSD(rb)

#tukeyy=TukeyHSD(rb)
#tukeyy$station
#tukeyy$Regime_alter

#tkr=tukeyy$regime
#tks=tukeyy$station

#### pour k hypotèses, il y a k*(k-1)/2 comparaisons à faire

# En attente, peu pertinent car analyse des interaction trop complexe


# Se rabattre sur AFC


######## test AFC #######

# Première question : AFC (FCA)ou ACM (CMA) ?

# Plus de 2 variables donc plutôt ACM.

# Il faut convertir les variables quantitatives en qualitatives. Pour cela, utiliser la fonction cut()
# http://gastonsanchez.com/blog/how-to/2012/10/13/MCA-in-R.html


# Se
Bd <- select(sub_BDD_PME, Groupe_station, Regime_alter, Se_ppm)

Bd$Se_qual <- cut(Bd$Se_ppm, 5)

require(gtools)

Bd$Se_qual2 <- quantcut(Bd$Se_ppm, q = seq(0, 1, by = 0.2))
Bd$Se_qual2
Bd2 <- Bd[,- 3]
Bd2$Se_qual <- as.factor(Bd2$Se_qual)
Bd2$Se_qual2 <- as.factor(Bd2$Se_qual2)
Bd_quint <- Bd2 [, - 3]
Bd_cut <- Bd2 [, - 4]

cats <- apply(Bd2, 2, function(x) nlevels(as.factor(x)))

mca1 <- MCA(Bd2)

mca1_vars_df <- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df <- data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")

# Hg
Bd <- select(sub_BDD_PME, Groupe_station, Regime_alter, Hg_ppm)
Bd <- Bd[!(is.na(Bd$Hg_ppm)),]

Bd$Hg_qual <- cut(Bd$Hg_ppm, 5)

require(gtools)

Bd$Hg_qual2 <- quantcut(Bd$Hg_ppm, q = seq(0, 1, by = 0.2))
Bd$Hg_qual2
Bd2 <- Bd[,- 3]
Bd2$Hg_qual <- as.factor(Bd2$Hg_qual)
Bd2$Hg_qual2 <- as.factor(Bd2$Hg_qual2)
Bd_quint <- Bd2 [, - 3]
Bd_quint$Hg_qual2 <- as.factor(Bd_quint$Hg_qual2)

cats <- apply(Bd_quint, 2, function(x) nlevels(as.factor(x)))

mca2 <- MCA(Bd_quint)

mca2_vars_df <- data.frame(mca2$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca2_obs_df <- data.frame(mca2$ind$coord)

# plot of variable categories
ggplot(data=mca2_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca2_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca2_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca2_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")





# Ward Hierarchical Clustering
d <- dist(Bd2, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 



require(pvclust)

# Ward Hierarchical Clustering with Bootstrapped p values

fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 



######## Test couleurs

scales::hue_pal()(14) # code des couleurs utilisées de base par ggplot
# "#F8766D" "#D89000" "#A3A500" "#39B600" "#00BF7D" "#00BFC4" "#00B0F6" "#9590FF" "#E76BF3" "#FF62BC"
# Alternative pour projections, couleurs plus saturées
# #a6cee3 #1f78b4 #b2df8a #33a02c #fb9a99 #e31a1c #fdbf6f #ff7f00 #cab2d6 #6a3d9a

# http://stackoverflow.com/questions/19068432/ggplot2-how-to-use-same-colors-in-different-plots-for-same-factor?rq=1

# Graphical Data overview :
#http://stats.stackexchange.com/questions/4089/graphical-data-overview-summary-function-in-r


library(PerformanceAnalytics)
chart.Correlation(iris[,1:4],col=iris$Species)

# ?scatterplot.matrix() from car package

library(fitdistrplus)