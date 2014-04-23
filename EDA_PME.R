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

# Chargement BDD comprenant donnees Criques Nouvelle-France, Chien et Trois Sauts

BDD <-read.csv("Data/2014_04_11_subset_BDD.csv", sep=";", stringsAsFactors = FALSE) # Chemin relatif
# stringsAsFactors = FALSE permet d'eviter que les variables numeriques soient reconnues comme des facteurs 


str(BDD)

class(BDD$ls_mm)
class(BDD$pds_g)
class(BDD$conc_Hg_muscle_ppm)
class(BDD$conc_Hg_foie_ppm)
class(BDD$conc_Hg_branchie_ppm)

class(BDD$As_ppm)
class(BDD$Cr_ppm)
class(BDD$Co_ppm)
class(BDD$Cu_ppm)
class(BDD$Ni_ppm)
class(BDD$Zn_ppm)
class(BDD$Se_ppm)
class(BDD$Cd_ppm)
class(BDD$Pb_ppm)

class(BDD$Regime_alter)
BDD$Code_Station <- as.factor(BDD$Code_Station)
BDD$code_sp <- as.factor(BDD$code_sp)
BDD$ordre <- as.factor(BDD$ordre)
BDD$Genre <- as.factor(BDD$Genre)
BDD$Groupe_station <- as.factor(BDD$Groupe_station)
BDD$station <- as.factor(BDD$station)
BDD$Regime_alter <- as.factor(BDD$Regime_alter)
BDD$Regime_principal <- as.factor(BDD$Regime_principal)


# Les valeurs etant supposees etre numeriques sont numeriques

#BDD$conc_Hg_muscle_ppm<- as.numeric(BDD$conc_Hg_muscle_ppm)
#BDD$Pb_ppm<- as.numeric(BDD$Pb_ppm)


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
scatter <- ggplot(BDD, aes(x = ls_mm, y = pds_g)) + 
  geom_point(aes(color = Regime_alter, shape = Regime_principal)) + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  scale_x_continuous(limits = c(0, 200)) + 
  scale_y_continuous(limits = c(0, 100)) # 2 Hoplias aimara de presque 2 kg qui entrainent le tassement de la majorite du jeu de donnees

#marginal density of x - plot on top
plot_top <- ggplot(BDD, aes(ls_mm, fill=Regime_alter)) + 
  geom_density(alpha = .5) + 
  scale_x_continuous(limits = c(0, 200)) + # pour apercu plus detaille de la distrib de la majorite des poissons
  theme(legend.position = "none")

#### distribution approximative du poids
#ggplot(BDD, aes(x = Regime_alter, y = pds_g)) +
#  geom_boxplot() +
#  scale_y_continuous(limits = c(0, 200)) # la grande majorite est <200g

#marginal density of y - plot on the right
plot_right <- ggplot(BDD, aes(pds_g, fill=Regime_alter)) + 
  geom_density(alpha = .5) + 
  scale_x_continuous(limits = c(0, 50)) + # limites d'apres divers essais. poissons tres legers
  coord_flip() + 
  theme(legend.position = "none") 


#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

# nuage de points alternatif avec zoom sur majorite de points
scatter <- ggplot(BDD, aes(x = ls_mm, y = pds_g)) + 
  geom_point(aes(color = Regime_alter, shape = Regime_principal)) + 
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  scale_x_continuous(limits = c(0, 200)) + 
  scale_y_continuous(limits = c(0, 100)) #+ # 2 Hoplias aimara de presque 2 kg qui entrainent le tassement de la majorite du jeu de donnees
#geom_smooth(aes(color = Regime_alter), method = "loess")





##___##



## Quelle est la repartition des regimes alimentaires parmi les poissons preleves sur chaque groupe de station ?


# Reordonne les niveaux du facteur "Regime_principal" dans l'ordre decroissant de contamination au mercure
BDD$Regime_alter <- factor(BDD$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

BDD$Regime_principal <- factor(BDD$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))


# Repartition des regimes pr chaque groupe de stations pour BDD generale
p1 <- ggplot(BDD, aes(Groupe_station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill")
# repartition des regimes pr chaque groupe de stations (sans prendre en compte le nb d'individus)
p2 <- ggplot(BDD, aes(x = Groupe_station)) +
  geom_bar(aes(fill = Regime_alter))
# repartition des regimes pr chaque groupe de stations (en prenant en compte le nb d'individus)
grid.arrange(p1, p2, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))

# Repartition des regimes pr chaque groupe de stations pour subset BDD
p11 <- ggplot(sub_BDD, aes(Groupe_station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill")
# repartition des regimes pr chaque groupe de stations (sans prendre en compte le nb d'individus)
p22 <- ggplot(sub_BDD, aes(x = Groupe_station)) +
  geom_bar(aes(fill = Regime_alter))
# repartition des regimes pr chaque groupe de stations (en prenant en compte le nb d'individus)
grid.arrange(p11, p22, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))


# Repartition des regimes sur chaque station
p10 <- ggplot(BDD, aes(Code_Station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill")
# repartition des regimes sur chaque station (sans prendre en compte le nb d'individus)
p20 <- ggplot(BDD, aes(x = Code_Station)) +
  geom_bar(aes(fill = Regime_alter))
# repartition des regimes sur chaque station (en prenant en compte le nb d'individus)
grid.arrange(p10, p20, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))

# La repartition des regimes est fondamentalement differente au niveau de la Crique Nouvelle France 6
# Peu d'individus ont ete preleves au niveau des criques Chien

# Repartition des genres sur chaque station
p100 <- ggplot(BDD, aes(Code_Station)) +
  geom_bar(aes(fill = Genre), position = "fill") +
  guides(fill=guide_legend(ncol=2))
# 
p200 <- ggplot(BDD, aes(x = Code_Station)) +
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
p1000 <- ggplot(BDD, aes(Groupe_station)) +
  geom_bar(aes(fill = Genre), position = "fill") +
  guides(fill=guide_legend(ncol=2))
# 
p2000 <- ggplot(BDD, aes(x = Groupe_station)) +
  geom_bar(aes(fill = Genre))

legend <- g_legend(p1000)
# 
grid.arrange(arrangeGrob(p1000 + theme(legend.position="none"),
                         p2000 + theme(legend.position="none"),
                         ncol = 1),
             legend, ncol = 2, nrow = 1, widths = c(8, 1), heights = c(1, 1))


##___##



## Quelle est la contamination moyenne en Hg parmi les poissons preleves sur chaque groupe de station en fonction de leur regime alimentaire ?

dfmelt_Hg <- melt(BDD, id.vars = c("Groupe_station", "Regime_principal", "Code_Station"), measure.vars = c("conc_Hg_muscle_ppm", "conc_Hg_foie_ppm", "conc_Hg_branchie_ppm"))
# la fonction melt permet de modifier le format d'une BDD. Changement wide -> long

p_1<-ggplot(BDD, aes(x = Groupe_station, y = conc_Hg_muscle_ppm)) +
  geom_boxplot()
p_2<-ggplot(BDD, aes(x = Groupe_station, y = conc_Hg_foie_ppm)) +
  geom_boxplot()
p_3<-ggplot(BDD, aes(x = Groupe_station, y = conc_Hg_branchie_ppm)) +
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



dfmelt_mtx <- melt(BDD, id.vars = c("Groupe_station", "Code_Station", "Site", "Regime_principal"), measure.vars = c("Hg_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm"))

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

# df_mtx <- melt(BDD, id.vars = NULL, measure.vars = c("Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm")
# ggplot(df_mtx, aes(x = value, y = ..density..)) + geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.3) +  geom_density() +  facet_grid(variable~., scales = "free_x")
# ne fonctionne pas, je suppose qu'un ajustement manuel est necessaire



# Exemple de representation de distribution
ggplot(BDD, aes(Cu_ppm)) +
  geom_density(fill = "blue", colour = NA, alpha=.2) + # polygone bleu pour la densite
  geom_line(stat = "density") # contour du polygone en noir

# Pr chq metal, superposition densite theorique et histogramme de distribution

Hg <- ggplot(BDD, aes(x = conc_Hg_muscle_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.1) +
  geom_density()

Cu <- ggplot(BDD, aes(x = Cu_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.5) +
  geom_density()

Cr <- ggplot(BDD, aes(x = Cr_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.05) +
  geom_density()

Co <- ggplot(BDD, aes(x = Co_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.05) +
  geom_density()

Cd <- ggplot(BDD, aes(x = Cd_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.002) +
  geom_density()

Ni <- ggplot(BDD, aes(x = Ni_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.2) +
  geom_density()

Zn <- ggplot(BDD, aes(x = Zn_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 3) +
  geom_density()

As <- ggplot(BDD, aes(x = As_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.04) +
  geom_density()

Se <- ggplot(BDD, aes(x = Se_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.2) +
  geom_density()

Pb <- ggplot(BDD, aes(x = Pb_ppm, y = ..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth = 0.08) +
  geom_density()

# Repartition mtx hors Hg
grid.arrange(Zn, Cu, Se, Ni, Cr, Pb, Co, As, Cd, ncol = 3, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# Repartition mtx avec Hg
grid.arrange(Zn, Cu, Se, Hg, Ni, Cr, Pb, Co, As, Cd, ncol = 4, nrow = 3, widths = c(1, 1), heights = c(1, 1))

# ggplot(BDD, aes(x = Cu_ppm, y = ..density..)) +  geom_histogram(aes(fill= Regime_principal), binwidth = 0.5) +  geom_density()
# tentative d'ajout de la repartition des regimes alimentaires au niveau de chq distribution. Non concluant mais non developpe


# Informations plus precises

# Trois groupes car trois gammes de concentrations

# [Zn] : 0-90 ppm
dfmelt_mtx1 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes"), measure.vars = c("Zn_ppm"))
p1 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Regime_principal)) +
  geom_boxplot()
p01 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Genre)) +
  geom_boxplot()
p001 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Code_Station)) +
  geom_boxplot()
p0001 <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Groupe_station)) +
  geom_boxplot()

# [Hg], [Cr], [Co], [Cd], [Ni], [As], [Pb] : 0-10 ppm
dfmelt_mtx2 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes"), measure.vars = c("Hg_ppm","Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
p2 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Regime_principal)) +
  geom_boxplot() + theme(legend.position = "none")
p02 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Genre)) +
  geom_boxplot() + theme(legend.position = "none")
p002 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Code_Station)) +
  geom_boxplot() + theme(legend.position = "none")
p0002 <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Groupe_station)) +
  geom_boxplot() + theme(legend.position = "none")

# [Cu], [Se] : 0-30 ppm
dfmelt_mtx3 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes"), measure.vars = c("Cu_ppm", "Se_ppm"))
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
dfmelt_mtx <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Hg_ppm","Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
p <- ggplot(na.omit(dfmelt_mtx), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p8 <- ggplot(na.omit(dfmelt_mtx), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()
dfmelt_mt0 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
p_0 <- ggplot(na.omit(dfmelt_mt0), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p_8 <- ggplot(na.omit(dfmelt_mt0), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()
dfmelt_mt <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Zn_ppm"))
p0 <- ggplot(na.omit(dfmelt_mt), aes(x = Genre, y = value, color = variable)) +
  geom_boxplot()
p88 <- ggplot(na.omit(dfmelt_mt), aes(x = Code, y = value, color = variable)) +
  geom_boxplot()
dfmelt_m <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal", "Genre", "Code"), measure.vars = c("Cu_ppm", "Se_ppm"))
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


scat <- ggplot(BDD, aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = Regime_principal, shape = Regime_principal)) + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1)) #+
#scale_x_continuous(limits = c(- 35, - 24)) + 
#scale_y_continuous(limits = c(5, 15))

plot_t <- ggplot(BDD, aes(d13C, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

# d15n = f d13C
grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

##

scat <- ggplot(BDD, aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = Regime_alter)) + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1)) #+
#scale_x_continuous(limits = c(- 35, - 24)) + 
#scale_y_continuous(limits = c(5, 15))

plot_t <- ggplot(BDD, aes(d13C, fill = Regime_alter)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD, aes(d15N, fill = Regime_alter)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

# d15n = f d13C
grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))



##___##

# Relation [Hg] = f d15N

### Muscle
scat <- ggplot(BDD, aes(x = d15N, y = conc_Hg_muscle_ppm, colour = Regime_principal, shape = Regime_principal)) +
  geom_point() + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1))

plot_t <- ggplot(BDD, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD, aes(conc_Hg_muscle_ppm, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

### Foie
scat <- ggplot(BDD, aes(x = d15N, y = conc_Hg_foie_ppm, colour = Regime_principal, shape = Regime_principal)) +
  geom_point() + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1))

plot_t <- ggplot(BDD, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD, aes(conc_Hg_foie_ppm, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

### Branchie
scat <- ggplot(BDD, aes(x = d15N, y = conc_Hg_branchie_ppm, colour = Regime_principal, shape = Regime_principal)) +
  geom_point() + 
  theme(legend.position = c(0.25, 1), legend.justification = c(1, 1))

plot_t <- ggplot(BDD, aes(d15N, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(- 35, - 24)) +
  theme(legend.position = "none")

plot_r <- ggplot(BDD, aes(conc_Hg_branchie_ppm, fill = Regime_principal)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(5, 15)) +
  coord_flip() + 
  theme(legend.position = "none") 

grid.arrange(plot_t, empty, scat, plot_r, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

# Rq : les 3 distributions projetees en haut ne sont pas tout a fait correctes : il s'agit de l'ensemble des valeurs de d15N et non pas des valeurs ayant des [Hg] mesurees



###### 26/03/2014 #####



str(BDD)

dfmelt <- melt(BDD, id.vars = c("Groupe_station", "Code_Station", "Site", "Regime_principal", "Regime_alter"), measure.vars = c("Hg_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "Cu_ppm", "Zn_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm"))

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

for(i in BDD$Groupe_station){
  for (j in BDD$Regime_principal){
    norm[i+j] <- shapiro.test(BDD$Se_ppm)
  }
  
}

for(i in BDD$Groupe_station){
  norm[i] <- shapiro.test(BDD[BDD$Groupe_station == "i", ]$Se_ppm)
}


shapiro.test(BDD$As_ppm)
?friedman.test()
#################################################################


# Details pour chq metal


sub_BDD <- BDD[!(is.na(BDD$Cr_ppm)), ]

sub_BDD$Regime_alter <- factor(sub_BDD$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

sub_BDD$Regime_principal <- factor(sub_BDD$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))

# Hg

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Hg_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

Hg <- ggplot(sub_BDD, aes(x = Groupe_station, y = Hg_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

ggplot(sub_BDD, aes(x = Groupe_station, y = Hg_ppm, color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

Hg <- ggplot(sub_BDD, aes(x = Groupe_station, y = pds_g, color = Regime_principal)) +
  geom_boxplot()

ggplot(BDD[BDD$Site =="Camopi",], aes(x = station, y= conc_Hg_muscle_ppm , color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

# a imprimer !
ggplot(BDD, aes(x = Groupe_station, y = conc_Hg_muscle_ppm, color = Regime_alter)) + geom_boxplot()

# Il y a une couleur qui ne devrait pas apparaitre, comme si il y avait encore des valeurs manquantes...


# Cd

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Cd_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 0.5))

Cd <- ggplot(sub_BDD, aes(x = Groupe_station, y = Cd_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 0.5))

ggplot(sub_BDD, aes(x = Groupe_station, y = Cd_ppm, color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 0.5))

# Pb

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Pb_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

Pb <-ggplot(sub_BDD, aes(x = Groupe_station, y = Pb_ppm, color = Regime_principal)) +
  geom_boxplot() + geom_hline(aes(yintercept = 1.5))

ggplot(sub_BDD, aes(x = Groupe_station, y = Pb_ppm, color = Regime_alter)) +
  geom_boxplot() + geom_hline(aes(yintercept = 2.5))

# Cr

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Cr_ppm, color = Regime_principal)) +
  geom_boxplot()

Cr <- ggplot(sub_BDD, aes(x = Groupe_station, y = Cr_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Groupe_station, y = Cr_ppm, color = Regime_alter)) +
  geom_boxplot()

# a imprimer
ggplot(sub_BDD, aes(x = Site, y = Cr_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Site, y = Cr_ppm)) +
  geom_boxplot()


# Co

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Co_ppm, color = Regime_principal)) +
  geom_boxplot()

Co <- ggplot(sub_BDD, aes(x = Groupe_station, y = Co_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Groupe_station, y = Co_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD[sub_BDD$Site == "Saul",], aes(x= pds_g, y = Co_ppm, color = Regime_principal)) + geom_point()

# Ni

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Ni_ppm, color = Regime_principal)) +
  geom_boxplot()

Ni <- ggplot(sub_BDD, aes(x = Groupe_station, y = Ni_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Groupe_station, y = Ni_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Site, y = Ni_ppm, color = Regime_principal)) +
  geom_boxplot()

# Cu

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Cu_ppm, color = Regime_principal)) +
  geom_boxplot()

Cu <- ggplot(sub_BDD, aes(x = Groupe_station, y = Cu_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Groupe_station, y = Cu_ppm, color = Regime_alter)) +
  geom_boxplot()

# Zn

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Zn_ppm, color = Regime_principal)) +
  geom_boxplot()

Zn <- ggplot(sub_BDD, aes(x = Groupe_station, y = Zn_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Groupe_station, y = Zn_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Site, y = Zn_ppm, color = Regime_principal)) +
  geom_boxplot()

# As

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = As_ppm, color = Regime_principal)) +
  geom_boxplot()

As <- ggplot(sub_BDD, aes(x = Groupe_station, y = As_ppm, color = Regime_principal)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Groupe_station, y = As_ppm, color = Regime_alter)) +
  geom_boxplot()

# Se

ggplot(sub_BDD[sub_BDD$Regime_principal == c("Carnivore", "Omnivore"),], aes(x = Code_Station, y = Se_ppm, color = Regime_principal)) +
  geom_boxplot()

Se <- ggplot(sub_BDD, aes(x = Groupe_station, y = Se_ppm, color = Regime_principal)) +
  geom_boxplot()

Se <- ggplot(sub_BDD, aes(x = Groupe_station, y = Se_ppm, color = Regime_alter)) +
  geom_boxplot()

ggplot(sub_BDD, aes(x = Site, y = Se_ppm, color = Regime_alter)) +
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


res.pca = PCA(na.omit(sub_BDD[,c(37, 38, 53:62)]), scale.unit=TRUE, ncp=5, graph=T) 

dimdesc(res.pca, axes=c(1,2))

require(ade4)

acp <- dudi.pca(na.omit(sub_BDD[,c(37, 38, 52:61)]), scannf = TRUE)


# Comparisons

tapply(sub_BDD$Se_ppm, sub_BDD$Regime_principal, shapiro.test)

sub_BDD$inter <- interaction(sub_BDD$Groupe_station, sub_BDD$Regime_principal)

require (agricolae)

comparison <- kruskal(sub_BDD$Hg_ppm, sub_BDD$inter, alpha = 0.05)

compariso <- kruskal(sub_BDD$Se_ppm, sub_BDD$inter, alpha = 0.05)

require(pgirmess)

compa <- kruskalmc(sub_BDD$Se_ppm, sub_BDD$inter) # Test post-hoc 


ano <- aov(sub_BDD$Se_ppm~sub_BDD$Regime_principal+sub_BDD$Groupe_station) ; summary(ano)

ggplot(sub_BDD, aes(y = Hg_ppm, x = Regime_principal)) + geom_boxplot()


box.cox <- function(x, lambda) {
  if (lambda==0) log(x) else ((x)^lambda - 1)/lambda
}


fligner.test(Se_ppm~inter, data = sub_BDD)
sub_BDD$boxcox_Se <- box.cox(sub_BDD$Se_ppm, 1/2) # transformation box cox pour lambda = 1/2
fligner.test(boxcox_Se~inter, data = sub_BDD) # Homogénéité des variances
tapply(sub_BDD$boxcox_Se, sub_BDD$inter, shapiro.test)
qplot(sample = boxcox_Se, data = sub_BDD, colour = factor(inter))

# message d'erreur de type Error in FUN(X[[1L]], ...) : sample size must be between 3 and 5000
# peut etre cause par presence de NA, ce qui est le cas pour le mercure !!!
# Mais pas d'explication pour Se...



# Test Ordinal Logistic Regression

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

lapply(sub_BDD[, c("Groupe_station", "Regime_principal")], table)
ftable(xtabs(~ Groupe_station + Regime_principal, data = sub_BDD))
m <- polr(Se_ppm ~ Groupe_station + Regime_principal, data = sub_BDD, Hess = TRUE)
summary(m)
# [Se] n'est pas un facteur...

# Test Friedman


sub_BDD$Groupe_station <- as.factor(sub_BDD$Groupe_station)

friedman.test(Se_ppm ~ Regime_principal | Groupe_station, data = sub_BDD)

str(sub_BDD)
# not an unreplicated complete block design


# Rank test on interaction
sub_BDD <- select(BDD, Se_ppm, Regime_principal, Groupe_station)


?rank()
sub_BDD$rank <- rank(sub_BDD$inter)

fligner.test(Se_ppm ~ Regime_principal*Groupe_station, data = sub_BDD)
leveneTest(Se_ppm ~ Regime_principal*Groupe_station, data = sub_BDD)

tapply(sub_BDD$Se_ppm, sub_BDD$Regime_principal, shapiro.test)
tapply(sub_BDD$Se_ppm, sub_BDD$Groupe_station, shapiro.test)

# Comment faire pour obtenir des resultats plus precis ?
require(dplyr)
Ch_cont <- filter(sub_BDD, Groupe_station == "Chien_conta")
fligner.test(Se_ppm ~ Regime_principal, data = Ch_cont)
tapply(Ch_cont$Se_ppm, Ch_cont$Regime_principal, shapiro.test)


sub_BDD$inter <- interaction(sub_BDD$Groupe_station, sub_BDD$Regime_principal)
fligner.test(Se_ppm ~ inter, data = sub_BDD)
tapply(sub_BDD$Se_ppm, sub_BDD$inter, shapiro.test)


lm1 <- lm(rank(sub_BDD$Se_ppm) ~ sub_BDD$Groupe_station * sub_BDD$Regime_alter)

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
comp <- kruskal(sub_BDD$Se_ppm, sub_BDD$inter, alpha = 0.05)
comp$groups

require(pgirmess)
compa <- kruskalmc()
?kruskalmc()

# Test FAMD

require(FactoMineR)
df.famd <- select(sub_BDD, Groupe_station, Code_Station, code_sp, pds_g, ordre, Genre, Regime_principal, d13C, d15N, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm, Cr_ppm, Co_ppm, Ni_ppm, Cu_ppm, Zn_ppm, As_ppm, Se_ppm, Cd_ppm, Pb_ppm)
na.famd <- FAMD(na.omit(df.famd))
summary(na.famd)

###
df <- select(BDD, Groupe_station, Regime_principal, ordre, d13C, d15N, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm)
famd <- FAMD(na.omit(df))
summary(famd)
famd

df2 <- select(BDD, Regime_principal, d15N, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm)
famd2 <- FAMD(na.omit(df2))
summary(famd2)
plot(famd2, habillage = 1)

df3 <- select(BDD, Regime_principal, conc_Hg_foie_ppm, conc_Hg_muscle_ppm, conc_Hg_branchie_ppm)
famd3 <- FAMD(na.omit(df3))
summary(famd3)
plot(famd3, habillage = 1)


####### Quel regroupement est il possible de faire au niveau des especes ? #########

#d.melt <- melt(BDD, id.vars = c("Groupe_station", "Regime_principal", "Code_Station", "d15N"), measure.vars = c("conc_Hg_muscle_ppm", "conc_Hg_foie_ppm", "conc_Hg_branchie_ppm"))
#carn <- na.omit(d.melt[d.melt$Regime_principal == "Carnivore",], )
#sBDD <- BDD[!(is.na(BDD$Genre)), ]

# Chien non contamine

ggplot(BDD[BDD$Groupe_station %in% "Chien_non_conta" & BDD$Regime_principal %in% c("Carnivore", "Omnivore"), ], aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")

### Quelle difference entre %in% et == lorsque utilisation de subset ?

ggplot( filter(BDD, Groupe_station %in% "Chien_non_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Genre, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# equivalent a la selection precedente.
# regroupement de CCAR, PFIL, SMAC, RIVULUS SP, RLUN ?, BHYP ?

# Chien contamine

ggplot( filter(BDD, Groupe_station %in% "Chien_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# JKEI

# NF contamine

ggplot( filter(BDD, Groupe_station %in% "NF_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# JKIR ?

# NF non contamine

ggplot( filter(BDD, Groupe_station %in% "NF_non_conta", Regime_principal %in% c("Carnivore", "Omnivore")), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code, label = Code)) +
  geom_text(hjust=0, vjust=0) +
  geom_point(aes(shape=ordre)) +
  facet_grid(Regime_principal ~ ., scales = "free")
# PBRE, MRAR, RGUY, JABR

# Vue d'ensemble carnivores
se <- function(x) sqrt(var(x)/length(x))

df.sp <- filter(BDD, Regime_principal %in% "Carnivore") %.% # Selection BDD
  group_by(Code) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)))  # Selection donnees a  calculer
#df.sp <- na.omit(merge(df.sp, select(filter(BDD, Regime_principal %in% "Carnivore"), ordre, Code), by = 'Code'))

ggplot( filter(BDD, Regime_principal %in% "Carnivore"), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code)) +
  geom_point(aes(shape = ordre)) +
  geom_point(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code), size = 4) +
  geom_text(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code, label = Code), hjust = 1, vjust = -1) +
  geom_errorbarh(data = df.sp, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Code), width = .1)


# Vue d'ensemble omnivores
se <- function(x) sqrt(var(x)/length(x))

df.sp <- filter(BDD, Regime_principal %in% "Omnivore") %.% # Selection BDD
  group_by(Code) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)))  # Selection donnees a  calculer
#df.sp <- na.omit(merge(df.sp, select(filter(BDD, Regime_principal %in% "Carnivore"), ordre, Code), by = 'Code'))

ggplot( filter(BDD, Regime_principal %in% "Omnivore"), aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code)) +
  geom_point(aes(shape = ordre)) +
  geom_point(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code), size = 4) +
  geom_text(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code, label = Code), hjust = 1, vjust = -1) +
  geom_errorbarh(data = df.sp, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Code), width = .1)



# Vue d'ensemble tous poissons selon sp
df.sp <- BDD %.% # Selection BDD
  group_by(Code) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Selection donnees a  calculer
# Toujours la meme question : comment faire pour appliquer "summarise" sur un facteur et ainsi ajouter l'information Regime_alimenaire pour chaque Code...
df.sp <- na.omit(merge(df.sp, select(BDD, Regime_principal, Code), by = 'Code')) # Solution potentielle

ggplot( BDD, aes(x = d15N, y = conc_Hg_muscle_ppm, color = Code)) +
  #geom_point(aes(shape=ordre)) +
  geom_point(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code), size = 4) +
  geom_text(data = df.sp, aes(x = d15N_mean, y = Hg_muscle_mean, color = Code, label = Code), hjust=1, vjust=-1) +
  geom_errorbarh(data = df.sp, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Code), width = .1)

ggplot( BDD, aes(x = d13C, y = d15N, color = Code)) +
  #geom_point(aes(shape=ordre)) +
  geom_point(data = df.sp, aes(y = d15N_mean, x = d13C_mean, color = Code), size = 4) +
  geom_text(data = df.sp, aes(y = d15N_mean, x = d13C_mean, color = Code, label = Code), hjust=1, vjust=-1) +
  geom_errorbarh(data = df.sp, aes(xmin = d13C_mean + d13C_se, xmax = d13C_mean - d13C_se, y = d15N_mean, x = d13C_mean, colour = Code), height = .1) + 
  geom_errorbar(data = df.sp, aes(ymin = d15N_mean - d15N_se, ymax = d15N_mean + d15N_se, x = d13C_mean, y = d15N_mean, colour = Code), width = .1)


# Vue d'ensemble tous poissons selon regime
df.reg <- BDD %.% # Selection BDD
  group_by(Regime_alter) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)), Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Selection donnees a  calculer


ggplot(BDD, aes(x = d13C, y = d15N)) +
  geom_point(data = df.sp, aes(x = d13C_mean, y = d15N_mean, fill = Code), show_guide = FALSE) +
  geom_point(data = df.reg, aes(y = d15N_mean, x = d13C_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg, aes(y = d15N_mean, x = d13C_mean, color = Regime_alter, label = Regime_alter), hjust = 1.02, vjust = -1, size = 6.5) +
  geom_errorbarh(data = df.reg, aes(xmin = d13C_mean + d13C_se, xmax = d13C_mean - d13C_se, y = d15N_mean, x = d13C_mean, colour = Regime_alter), height = .025) + 
  geom_errorbar(data = df.reg, aes(ymin = d15N_mean - d15N_se, ymax = d15N_mean + d15N_se, x = d13C_mean, y = d15N_mean, colour = Regime_alter), width = .05)


pl1 <- ggplot( BDD, aes(x = d15N, y = conc_Hg_muscle_ppm)) +
  #geom_point(aes(color = Regime_alter), alpha = 0.65) +
  geom_point(data = na.omit(df.reg), aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
  geom_text(data = na.omit(df.reg), aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = Regime_alter), hjust=1.02, vjust=-1, size = 6.5) +
  geom_errorbarh(data = na.omit(df.reg), aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
  geom_errorbar(data = na.omit(df.reg), aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05)

pl11 <- ggplot( BDD, aes(x = d15N, y = conc_Hg_foie_ppm)) +
  #geom_point(aes(color = Regime_alter), alpha = 0.65) +
  geom_point(data = na.omit(df.reg), aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter), size = 4) +
  geom_text(data = na.omit(df.reg), aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter, label = Regime_alter), hjust=1.02, vjust=-1, size = 6.5) +
  geom_errorbarh(data = na.omit(df.reg), aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_foie_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
  geom_errorbar(data = na.omit(df.reg), aes(ymin = Hg_foie_mean - Hg_foie_se, ymax = Hg_foie_mean + Hg_foie_se, x = d15N_mean, y = Hg_foie_mean, colour = Regime_alter), width = .05)



pdf("2014_04_08_[Hg]_regime.pdf", width = 14, height = 9) # la fction pdf enregistre directement ds le dossier
print(pl1)
dev.off()

pl2 <- ggplot( BDD, aes(x = d15N, y = conc_Hg_foie_ppm)) +
  geom_point(aes(color = Regime_alter), alpha = 0.65) +
  geom_point(data = df.reg, aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg, aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter, label = Regime_alter), hjust=1, vjust=-1, size = 6.5) +
  geom_errorbarh(data = df.reg, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_foie_mean, x = d15N_mean, colour = Regime_alter), height = .05) + 
  geom_errorbar(data = df.reg, aes(ymin = Hg_foie_mean - Hg_foie_se, ymax = Hg_foie_mean + Hg_foie_se, x = d15N_mean, y = Hg_foie_mean, colour = Regime_alter), width = .05)

pl3 <- ggplot( BDD, aes(x = d15N, y = conc_Hg_branchie_ppm)) +
  geom_point(aes(color = Regime_alter), alpha = 0.65) +
  geom_point(data = df.reg, aes(x = d15N_mean, y = Hg_branchie_mean, color = Regime_alter), size = 4) +
  geom_text(data = df.reg, aes(x = d15N_mean, y = Hg_branchie_mean, color = Regime_alter, label = Regime_alter), hjust=1, vjust=-1, size = 6.5) +
  geom_errorbarh(data = df.reg, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_branchie_mean, x = d15N_mean, colour = Regime_alter), height = .05) + 
  geom_errorbar(data = df.reg, aes(ymin = Hg_branchie_mean - Hg_branchie_se, ymax = Hg_branchie_mean + Hg_branchie_se, x = d15N_mean, y = Hg_branchie_mean, colour = Regime_alter), width = .05)


legend <- g_legend(pl1)

grid.arrange(pl1, pl2, pl3, ncol = 1, nrow = 3) # Basic

grid.arrange(arrangeGrob(pl1 + theme(legend.position="none"),
                         pl2 + theme(legend.position="none"),
                         pl3 + theme(legend.position="none"),
                         ncol = 1),
             legend, ncol = 2, nrow = 1, widths = c(9, 1), heights = c(1, 1))


# Conta Hg globale sur Camopi, Saul et 3 Sauts

ggplot(BDD, aes( x = station, y = conc_Hg_muscle_ppm, color = Regime_alter)) +
  geom_boxplot()






########### Test rigoureux ANOVA 08/04/2014 ########################


anova <- aov(Se_ppm ~ Regime_alter * Groupe_station, data = sub_BDD)
summary(anova)


BDD.res = sub_BDD
BDD.res$Fit = fitted(anova)
BDD.res$Resid = resid(anova)

shapiro.test(BDD.res$Resid)

# Distribution plots of the residuals
ggplot(BDD.res, aes(x = Resid, y = ..density..)) + geom_histogram(binwidth = 0.15)

# Normal Probability Plot
p <- ggplot(BDD.res, aes(sample = Resid)) + stat_qq()

#QQ-plot pr chq groupe
p + facet_grid(. ~ Groupe_station)
ggplot(BDD.res[BDD.res$Regime_alter %in% c("Carnivore_Piscivore", "Carnivore_Invertivore", "Omnivore_Invertivore"),], aes(sample = Resid)) + stat_qq() + facet_grid(. ~ Regime_alter)

# Residuals vs Fitted
ggplot(BDD.res, aes(Fit, Resid, colour = Regime_alter, shape = Groupe_station)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

# Residuals vs groupes de station
ggplot(BDD.res, aes(x = Groupe_station, Resid, colour = Regime_alter, shape = Groupe_station)) + geom_point() +
  xlab("Groupe de stations") + ylab("Residuals")

# Residuals vs Regimes
ggplot(BDD.res, aes(x = Regime_alter, Resid, colour = Regime_alter, shape = Groupe_station)) + geom_point() +
  xlab("Regimes") + ylab("Residuals")

# non respect des assumptions
# test avec transformations des donnees en boxcox et en log : toujours non respect. Besoin d'un test non param



# Test ANOVA uniquement avec les 3 regimes presents en abondance : Piscivores et Invertivores.

aov.df <- sub_BDD[sub_BDD$Regime_alter %in% c("Carnivore_Piscivore", "Carnivore_Invertivore", "Omnivore_Invertivore"),]

anova <- aov(Se_ppm ~ Regime_alter * Groupe_station, data = aov.df)
summary(anova)

BDD.res = aov.df

# toujours non correct

# too much skewness or potential for outliers ? -> non-parametric methods


# transformation boxcox
require(MASS)

boxcox(Se_ppm ~ Regime_alter * Groupe_station, data= sub_BDD, lambda = seq(-1.5, 1.5, by = .1), plotit = TRUE)
# lambda optimal = ~ 0.5
boxcox(Se_ppm ~ Regime_alter * Groupe_station, data= sub_BDD, lambda = seq(0.4, 0.7, length = 25), plotit = FALSE )
# lambda optimal = 0.5250

lambda <- 0.5250

# Transformation
sub_BDD$boxcoxSe <- ((sub_BDD$Se_ppm^lambda)-1)/lambda

# comparaison des QQ plots. Probleme
par(mfrow=c(2, 1))
qqnorm(sub_BDD$boxcoxSe)
qqnorm(sub_BDD$Se_ppm)


boxcox.anova <- aov(boxcoxSe ~ Regime_alter * Groupe_station, data = sub_BDD)

summary(boxcox.anova)

BDD.res = sub_BDD
BDD.res$Fit = fitted(boxcox.anova)
BDD.res$Resid = resid(boxcox.anova)

# Toujours pas bon meme apres boxcox


# Test pour Cr

plot(density(sub_BDD$Cr_ppm))

anova <- aov(Cr_ppm ~ Regime_alter * Groupe_station, data = sub_BDD)
summary(anova)

# pas bon sans transfo

plot(density(log(sub_BDD$Cr_ppm)))

boxcox(Cr_ppm ~ Regime_alter * Groupe_station, data= sub_BDD, lambda = seq(-1.5, 1.5, by = .1), plotit = TRUE)
# lambda optimal = ~ 0.4
boxcox(Cr_ppm ~ Regime_alter * Groupe_station, data= sub_BDD, lambda = seq(0.25, 0.5, length = 30), plotit = FALSE )
# lambda optimal = 0.36

lambda <- 0.36

sub_BDD$boxcoxCr <- ((sub_BDD$Cr_ppm^lambda)-1)/lambda

boxcox.anova <- aov(boxcoxCr ~ Regime_alter * Groupe_station, data = sub_BDD)

summary(boxcox.anova)

BDD.res = sub_BDD
BDD.res$Fit = fitted(boxcox.anova)
BDD.res$Resid = resid(boxcox.anova)

# Toujours pas bon meme apres la transformation boxcox.

# Il faut reellement trouver un equivalent non parametrique a une ANOVA factorielle


######### TeachingDemos ########

require(TeachingDemos)

SnowsPenultimateNormalityTest(sub_BDD$Se_ppm)

vis.test()

test.visu <- function (x) {
  if(interactive()) {
    vis.test(x, vt.qqnorm)
    vis.test(x, vt.normhist)
  }
}

vis.test(BDD.res$Fit, BDD.res$Resid, vt.scatterpermute)


##### Siluriformes etaient plus contamines en Se a Nouvelle-France #####


BDD[["Siluriforme"]] = ifelse(BDD[["ordre"]] == "Siluriformes" & BDD[["Regime_principal"]] == "Carnivore", "Siluriformes_Carnivores", NA)

BDD[["Siluriformes"]] = ifelse(BDD[["ordre"]] == "Siluriformes" & BDD[["Regime_principal"]] == "Carnivore" & !is.na(BDD$Se_ppm), "Siluriformes_Carnivores", NA)


length(which(!is.na(BDD$Siluriformes) & !is.na(BDD$Se_ppm)))

dfmelt_mtx1 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes", "Siluriforme"), measure.vars = c("Zn_ppm"))
dfmelt_mtx2 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes", "Siluriforme"), measure.vars = c("Hg_ppm","Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Cd_ppm", "Pb_ppm"))
dfmelt_mtx3 <- melt(BDD, id.vars = c("Groupe_station", "Site", "Regime_principal","Genre", "Code_Station", "Siluriformes", "Siluriforme"), measure.vars = c("Cu_ppm", "Se_ppm"))
df.melt <- melt(BDD, id.vars = c("Siluriformes", "Siluriforme"), measure.vars = c("conc_Hg_muscle_ppm", "conc_Hg_foie_ppm", "conc_Hg_branchie_ppm"))

ggplot(BDD, aes(x = d15N, y = Hg_ppm , color = Siluriforme)) +
  geom_point()

p <- ggplot(dfmelt_mtx1, aes(x = variable, y = value, color = Siluriforme )) +
  geom_point(position = "jitter")
pp <- ggplot(dfmelt_mtx2, aes(x = variable, y = value, color = Siluriforme )) +
  geom_point(position = "jitter") + theme(legend.position = "none")
ppp <- ggplot(dfmelt_mtx3, aes(x = variable, y = value, color = Siluriforme )) +
  geom_point(position = "jitter") + theme(legend.position = "none")

grid.arrange(pp, ppp, p, ncol = 3, nrow = 1, widths = c(7, 2, 2), heights = c(1, 1))

ggplot(BDD, aes(ls_mm, pds_g, color = Siluriformes)) +
  #geom_point(aes(shape = Siluriforme), alpha = 0.8) +
  geom_point(alpha = 0.8) + # alternative a la ligne precedente
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 200))

ggplot(BDD, aes(d13C, d15N, color = Siluriformes)) +
  geom_point(aes(shape = Siluriforme)) 

ggplot(df.melt, aes(x = variable, y = value, color = Siluriforme)) +
  geom_point(position = "jitter")


ggplot(BDD, aes(x = Se_ppm, y = Hg_ppm, color = Regime_alter)) +
  geom_point()

BDD$HgSe_ratio <- BDD$Hg_ppm / BDD$Se_ppm

ggplot(BDD, aes(y = HgSe_ratio, x = Regime_alter)) +
  geom_boxplot() +  scale_y_continuous(limits = c(0, 2.5)) # un point aberrant car pas de valeurs pour les LQ pour l'instant

na.omit(BDD$HgSe_ratio)

######### Analyses Multivariees ##########



require(FactoMineR)


sub.df <- na.omit(sub_BDD[,c(40, 41, 56:65)])
scale.df <- scale(sub.df)
colMeans(scale.df)
apply(scale.df, 2, sd)


res.pca = PCA(na.omit(sub_BDD[,c(40, 41, 56:65)]), scale.unit=TRUE, ncp=5, graph=T) 
res.pca = PCA(na.omit(sub_BDD[,c(56:66)]), scale.unit=TRUE, ncp=5, graph=T)
res.pca = PCA((BDD[,c(49, 51, 53, 40, 41, 56:65)]), scale.unit=TRUE, ncp=5, graph=T) 
res.pca = PCA(sub_BDD[,c(40, 41, 56:65)], scale.unit=TRUE, ncp=5, graph=T, quali.sup = 10) # NA = column mean here
res.pca = PCA(scale.df, scale.unit=TRUE, ncp=5, graph=T) # Inutile car deja argument scale dans la fonction ; meme resultat obtenu dc rassurant

plot.PCA(res.pca, axes = c(1:3))

dimdesc(res.pca, axes=c(1,2))



require(ade4)

acp <- dudi.pca(na.omit(sub_BDD[,c(40, 41, 56:65)]), scannf = FALSE, nf = 2, center = TRUE, scale = TRUE)

par(mfrow = c(2,2))
s.class(acp$li, sub_BDD$Groupe_station, cpoint = 1)
s.arrow(acp$c1, lab = names(deug$tab))
s.class(acp$li, deug$result, cpoint = 1)
s.corcircle(acp$co, full = TRUE, box = TRUE)

scatter(acp, clab.row = 0, posieig = "none") # fonctionne
s.class(acp$li, sub_BDD$Groupe_station, add.plot = TRUE)


###### MAtrice de correlation entre les differentes variables

mcor <- cor((BDD[,c(40, 41, 56:65)]), use="complete.obs")
mat <- (BDD[,c(40, 41, 56:65)])

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
corrgram(BDD, type="data", lower.panel=panel.shadeNtext, 
         upper.panel=NULL)







#################### 11/04/2014 #####################

#### Contamination moyenne en Hg pour chq site

df <- read.csv("C:/Users/laurent/Desktop/Stage_Jerome_LAURENT/BDD/2014_04_15_BDD_PME.csv", sep=";", stringsAsFactors = FALSE)


df$pds_g<- as.numeric(df$pds_g)

df$station <- as.factor(df$station)

df$Pression <- as.factor(df$Pression)

df$Orpaillage <- as.factor(df$Orpaillage)

df$Pression_anthro <- as.factor(df$Pression_anthro)

df$Regime_alter <- factor(df$Regime_alter, levels = c("Carnivore_Piscivore", "Carnivore_Insectivore", "Carnivore_Invertivore", "Carnivore_Scaliphage", "Carnivore_Charognard", "Carnivore", "Omnivore_Piscivore", "Omnivore_Invertivore", "Omnivore_Insectivore", "Omnivore_Periphytophage", "Omnivore_Herbivore", "Detritivore", "Herbivore_Periphytophage", "Herbivore","Herbivore_Phyllophage", "Inconnu")) 

df$Regime_principal <- factor(df$Regime_principal, levels = c( "Carnivore", "Omnivore", "Detritivore", "Herbivore", "Inconnu"))


# calcul de differentes infos
df.Hg <- df %.% # Selection BDD
  group_by(station) %.% # Selection sp
  summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C)), Hg_foie_mean = mean(na.omit(conc_Hg_foie_ppm)), Hg_foie_se = se(na.omit(conc_Hg_foie_ppm)), Hg_branchie_mean = mean(na.omit(conc_Hg_branchie_ppm)), Hg_branchie_se = se(na.omit(conc_Hg_branchie_ppm))) # Selection donnees a  calculer

# calcul des moyennes pour chaque site
means <- aggregate(conc_Hg_muscle_ppm ~  station, df, mean)
means$conc_Hg_muscle_ppm <- round(means$conc_Hg_muscle_ppm, digits = 2)

# plot sur donnees ne comprenant pas de NA pour [Hg]muscle
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

df.ssNA <- completeFun(df, "conc_Hg_muscle_ppm")
df.sansNA <- df[!(is.na(df$conc_Hg_muscle_ppm)), ] # meme resultat que fonction precedente
df.sansNA$station <- droplevels(df.sansNA$station) # drop unused levels
levels(df.sansNA$station)
df.sansNA$station <- factor(df.sansNA$station, levels = c(1:54)) 


ftable(xtabs(~ BV + Regime_principal, data = df.sansNA))


# Distribution [Hg] muscle + valeur moyenne pr chq station

p0 <- ggplot(df.sansNA, aes(x = station , y = conc_Hg_muscle_ppm, color = Pression_anthro)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "red")

# OU

ggplot(df.sansNA, aes(x = station, y = conc_Hg_muscle_ppm, color = Regime_alter )) +
  geom_point(position = "jitter")

# Repartition des regimes sur chaque station
p10 <- ggplot(df.sansNA, aes(x = station)) +
  geom_bar(aes(fill = Regime_alter), position = "fill") +
  scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54)) # Remplace nom de la station par un numero ; ordre alphabetique conserve
# repartition des regimes sur chaque station (sans prendre en compte le nb d'individus)
p20 <- ggplot(df.sansNA, aes(x = station)) +
  geom_bar(aes(fill = Regime_alter)) +
  scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54))
# repartition des regimes sur chaque station (en prenant en compte le nb d'individus)
grid.arrange(p10, p20, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))

p <- ggplot(df.sansNA, aes(x = station, y = pds_g, color = Regime_alter)) +
  geom_point(position = "jitter") + scale_y_continuous(limits = c(0, 250)) +
  scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54))
pp <- ggplot(df.sansNA, aes(x = station, y = ls_mm, color = Regime_alter )) +
  geom_point(position = "jitter") +
  scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54))
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

kruskal.test(conc_Hg_muscle_ppm ~ station, data = df.sansNA) # Il esxiste des differences significatives


comparison <- kruskal(df.sansNA$conc_Hg_muscle_ppm, df.sansNA$station, alpha = 0.05, p.adj = "hochberg")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(df.sansNA, station, conc_Hg_muscle_ppm, Pression_anthro) # Subset plus simple a  manipuler

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
phoch <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54)) +
  ggtitle("Correction Hochberg")


grid.arrange(pnone, pbonf, pholm, phoch, ncol = 1, nrow = 4)


#### Repartition des concentrations selon regime alimentaire

## Omnivores(Omnivores Invertivores)

df.omni <- df.sansNA[df.sansNA$Regime_alter %in% "Omnivore_Invertivore",]


means.omni <- aggregate(conc_Hg_muscle_ppm ~  station, df.omni, mean)
means.omni$conc_Hg_muscle_ppm <- round(means.omni$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = df.omni)


comparison <- kruskal(df.omni$conc_Hg_muscle_ppm, df.omni$station, alpha = 0.05, p.adj = "holm")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(df.omni, station, conc_Hg_muscle_ppm) # Subset plus simple a  manipuler

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
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54))


## Carnivores(Carnivores Invertivores)

df.carni <- df.sansNA[df.sansNA$Regime_alter %in% "Carnivore_Invertivore",]


means.carni <- aggregate(conc_Hg_muscle_ppm ~  station, df.carni, mean)
means.carni$conc_Hg_muscle_ppm <- round(means.carni$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = df.carni)


# PROBLEME avec test post hoc ; resultats aberrants
comparison <- kruskal(df.carni$conc_Hg_muscle_ppm, df.carni$station, alpha = 0.05, p.adj = "holm")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(df.carni, station, conc_Hg_muscle_ppm) # Subset plus simple a  manipuler

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
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54))

######################### 16/04/2014 ###############################


####### Genres en communs entre toutes les stations #########

casted <- dcast(df.sansNA, Genre ~ station) # dcast = oppose de la fonction melt
# Ne donne pas le resultat attendu. On obtient un tableau de contingence avec le nb d'individu de chq genre au niveau de chq station
# et non pas une liste d'espece pour chaque station.

# Reduce(intersect, list(a, b , d)) # Du coup, ne fonctionne pas mais bonne synthaxe. Toujours utile au cas ou

# d'apres observation de casted, Moenkhausia semble etre l'espece la plus commune. Confirme par ligne suivante

sort(table(df.sansNA$Genre),decreasing=TRUE)[1:3] # 3 genres les plus communs


df.moenk <- df.sansNA[df.sansNA$Genre %in% "Moenkhausia",]

means.moenk <- aggregate(conc_Hg_muscle_ppm ~  station, df.moenk, mean)
means.moenk$conc_Hg_muscle_ppm <- round(means.moenk$conc_Hg_muscle_ppm, digits = 2)

kruskal.test(conc_Hg_muscle_ppm ~ station, data = df.moenk)


comparison <- kruskal(df.moenk$conc_Hg_muscle_ppm, df.moenk$station, alpha = 0.05, p.adj = "none")

posthoc <- comparison[['groups']]
posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite


BD <- select(df.moenk, station, conc_Hg_muscle_ppm) # Subset plus simple a  manipuler

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
p0 <- p0 + geom_text(aes(station, upper + 0.1, label = signif), data = test_f, vjust = -2, color = "red") + scale_x_discrete(breaks = c(levels(df.sansNA$station)), labels=c(1:54))


ggplot(df.moenk, aes(x = pds_g, y = conc_Hg_muscle_ppm)) +
  geom_point() + geom_smooth()

