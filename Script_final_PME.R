    ############################
    ##### SCRIPT FINAL PME #####
    ############################

    
    
rm(list = ls () ) # nettoyage memoire de R

require(MASS)
require(ggplot2)
require(plyr)
require(dplyr)
require(scales)
require(reshape) # pr fction melt, afin chgt format wide -> long
require(reshape2)
require(FactoMineR)
require(gridExtra)
# require(gtable) # alternative pour arranger graphes ensembles
require(missMDA)
require(agricolae)
# require(xtable)
require(gtools)
require(cluster)

source("Scripts/functions.R")
source("Scripts/data_cleaning.R")
    
    
    
    # Reprendre le script et remplacer dat$ind par dat[,'ind']
    
    

    ######################0000000000000########################   
    
    
    #### SOMMAIRE ####
    
    
    
    ######################0000000000000########################
    
    
#o# scatterplot poids fonction taille + projection marginale des distributions
    
    #### pds fction de longueur
    scatter <- ggplot(BDD_PME, aes(x = ls_mm, y = pds_g)) + 
      geom_point(aes(color = Regime_alter, shape = Regime_principal)) + 
      theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
      scale_x_continuous(limits = c(0, 200)) + 
      scale_y_continuous(limits = c(0, 100)) +# 2 Hoplias aimara de presque 2 kg qui entrainent le tassement de la majorite du jeu de donnees
      guides(shape = FALSE) # Pas de légende pour les formes
    
    #marginal density of x - plot on top
    plot_top <- ggplot(BDD_PME, aes(ls_mm, fill=Regime_alter)) + 
      geom_density(alpha = .5) + 
      scale_x_continuous(limits = c(0, 200)) + # pour apercu plus detaille de la distrib de la majorite des poissons
      theme(legend.position = "none")
        
    #marginal density of y - plot on the right
    plot_right <- ggplot(BDD_PME, aes(pds_g, fill=Regime_alter)) + 
      geom_density(alpha = .5) + 
      scale_x_continuous(limits = c(0, 50)) + # limites d'apres divers essais. poissons tres legers
      coord_flip() + 
      theme(legend.position = "none") 
    
    #arrange the plots together, with appropriate height and width for each row and column
    grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  

    ######################0000000000000########################
    
#o# Répartition des régimes au niveau des trois stations les plus étudiées (Crique Chien, Crique Nouvelle-france et 3 Sauts)    
    
  ### Repartition des regimes pr chaque groupe de stations pour BDD_PME generale
    p1 <- ggplot(BDD_PME, aes(Groupe_station)) +
      geom_bar(aes(fill = Regime_alter), position = "fill")
     # repartition des regimes pr chaque groupe de stations (sans prendre en compte le nb d'individus)
    p2 <- ggplot(BDD_PME, aes(x = Groupe_station)) +
      geom_bar(aes(fill = Regime_alter))
     # repartition des regimes pr chaque groupe de stations (en prenant en compte le nb d'individus)
    grid.arrange(p1, p2, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))
    
  ### Repartition des regimes pr chaque groupe de stations pour subset BDD_PME : uniquement les échantillons ayant des éléments traces dosés
    p11 <- ggplot(sub_BDD_PME, aes(Groupe_station)) +
      geom_bar(aes(fill = Regime_alter), position = "fill")
     # repartition des regimes pr chaque groupe de stations (sans prendre en compte le nb d'individus)
    p22 <- ggplot(sub_BDD_PME, aes(x = Groupe_station)) +
      geom_bar(aes(fill = Regime_alter))
     # repartition des regimes pr chaque groupe de stations (en prenant en compte le nb d'individus)
    grid.arrange(p11, p22, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))
    
    
    ######################0000000000000########################
    
#o# Répartition des régimes au niveau des trois stations les plus étudiées (Crique Chien, Crique Nouvelle-france et 3 Sauts)
    
  ### Repartition des regimes des échantillons ayant du Hg dosés dans les muscles sur chaque station de l'ensemble de la BDD
    p10 <- ggplot(BDD.sansNA, aes(Code_Station)) +
      geom_bar(aes(fill = Regime_alter), position = "fill") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # graduations de l'axe x écrites verticalement
     # repartition des regimes sur chaque station (sans prendre en compte le nb d'individus)
    p20 <- ggplot(BDD.sansNA, aes(x = Code_Station)) +
      geom_bar(aes(fill = Regime_alter)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
     # repartition des regimes sur chaque station (en prenant en compte le nb d'individus)
    grid.arrange(p10, p20, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))    
    
    
    ######################0000000000000########################
    
    
#o# Répartition des régimes en fonction des pressions anthropiques exercées sur les stations
    
  ### Repartition des regimes des échantillons ayant du Hg dosés dans les muscles sur chaque station de l'ensemble de la BDD
    p10 <- ggplot(BDD.sansNA, aes(Pression_anthro)) +
      geom_bar(aes(fill = Regime_alter), position = "fill") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # graduations de l'axe x écrites verticalement
    # repartition des regimes sur chaque station (sans prendre en compte le nb d'individus)
    p20 <- ggplot(BDD.sansNA, aes(x = Pression_anthro)) +
      geom_bar(aes(fill = Regime_alter)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    # repartition des regimes sur chaque station (en prenant en compte le nb d'individus)
    grid.arrange(p10, p20, ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1))    
    
    
    ######################0000000000000########################
    
#o# Ensemble de la BDD
    
  ### Impact des pressions anthropiques
    
    BD <- select(BDD.sansNA, conc_Hg_muscle_ppm, Pression_anthro, Regime_principal, Regime_alter, Genre) # Subset plus simple a  manipuler
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD) # Il existe des differences significatives
    
    comparison <- kruskal(BD$conc_Hg_muscle_ppm, BD$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
    lettpos <- function(BD) boxplot(BD$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles de poissons selon les pressions anthropiques exercées sur les stations") +
      geom_hline(aes(yintercept = 2.5), color = "red")
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()

    
    #0000000000000#
  
  ### Impact des pressions anthropiques sur les régimes principaux
    
    BD.carn <- BD[BD$Regime_principal %in% "Carnivore",]
    BD.omni <- BD[BD$Regime_principal %in% "Omnivore",]
    # BD.herbi <- BD[BD$Regime_principal %in% "Herbivore",] # Trop peu d'échantillons, qui plus est inégalement répartis
    # BD.detri <- BD[BD$Regime_principal %in% "Detritivore",] # Uniquement 16 indiv donc pas assez de données
    
    # Carnivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.carn, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.carn) # Il existe des differences significatives
    
    comparison <- kruskal(BD.carn$conc_Hg_muscle_ppm, BD.carn$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.carn, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
    lettpos <- function(BD.carn) boxplot(BD.carn$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD.carn, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles de carnivores selon les pressions anthropiques exercées sur les stations") +
      geom_hline(aes(yintercept = 2.5), color = "red")
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique_carnivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
    # Omnivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.omni, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.omni) # Il existe des differences significatives
    
    comparison <- kruskal(BD.omni$conc_Hg_muscle_ppm, BD.omni$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.omni, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
    lettpos <- function(BD.omni) boxplot(BD.omni$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD.omni, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles d'omnivores selon les pressions anthropiques exercées sur les stations") +
      geom_hline(aes(yintercept = 2.5), color = "red")
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique_omnivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
    #0000000000000#
    
  ### Impact des pressions anthropiques sur les régimes détaillés
    
        
    BD.omn.inver <- BD[BD$Regime_alter %in% "Omnivore_Invertivore",]
    BD.car.inver <- BD[BD$Regime_alter %in% "Carnivore_Invertivore",]
    BD.car.pisc <- BD[BD$Regime_alter %in% "Carnivore_Piscivore",]
    # BD.car.insec <- BD[BD$Regime_alter %in% "Carnivore_Insectivore",] # pas de données partout

    
    # Omnivores Invertivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.omn.inver, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.omn.inver) # Il existe des differences significatives
    
    comparison <- kruskal(BD.omn.inver$conc_Hg_muscle_ppm, BD.omn.inver$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.omn.inver, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
    lettpos <- function(BD.omn.inver) boxplot(BD.omn.inver$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD.omn.inver, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles d'omnivores invertivores selon les pressions anthropiques exercées sur les stations") +
      geom_hline(aes(yintercept = 2.5), color = "red")
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique_omnivores-invertivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
    # Carnivores Invertivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.car.inver, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.car.inver) # Il existe des differences significatives
    
    comparison <- kruskal(BD.car.inver$conc_Hg_muscle_ppm, BD.car.inver$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.car.inver, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.08), color = "blue")
    lettpos <- function(BD.car.inver) boxplot(BD.car.inver$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD.car.inver, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles de carnivores invertivores selon les pressions anthropiques exercées sur les stations") +
      geom_hline(aes(yintercept = 2.5), color = "red")
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique_carnivores-invertivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    # Carnivores Piscivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.car.pisc, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.car.pisc) # Il existe des differences significatives
    
    comparison <- kruskal(BD.car.pisc$conc_Hg_muscle_ppm, BD.car.pisc$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.car.pisc, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      geom_hline(aes(yintercept = 2.5), color = "red") +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.15), color = "blue")
    lettpos <- function(BD.car.pisc) boxplot(BD.car.pisc$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD.car.pisc, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper - 0.5, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles de carnivores piscivores selon les pressions anthropiques exercées sur les stations")
      
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique_carnivores-piscivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    

    #0000000000000#
    
  ### Impact des pressions anthropiques chez Mohenkausia
    
    BD.moen <- BD[BD$Genre %in% "Moenkhausia",]
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.moen, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.moen) # Il existe des differences significatives
    
    comparison <- kruskal(BD.moen$conc_Hg_muscle_ppm, BD.moen$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.moen, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      geom_hline(aes(yintercept = 2.5), color = "red") +
      stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                   shape = 18, size = 3,show_guide = FALSE) + 
      geom_text(data = means.pression, aes(label = conc_Hg_muscle_ppm, y = conc_Hg_muscle_ppm + 0.15), color = "blue")
    lettpos <- function(BD.moen) boxplot(BD.moen$conc_Hg_muscle_ppm, plot = FALSE)$stats[5,] # determination d'un emplacement > a  la "moustache" du boxplot
    test <- ddply(BD.moen, .(Pression_anthro), lettpos) # Obtention de cette information pour chaque facteur (ici, Date)
    test_f <- merge(test, posthoc, by.x = "Pression_anthro", by.y = "trt") # Les 2 tableaux sont reunis par rapport aux valeurs row.names
    colnames(test_f)[2] <- "upper"
    colnames(test_f)[4] <- "signif"
    test_f$signif <- as.character(test_f$signif) # au cas ou, pour que l'affichage se produise correctement. Pas forcement utile.
    p0 <- p0 + geom_text(aes(Pression_anthro, upper - 0.5, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence",  "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles de Moenkhausia selon les pressions anthropiques exercées sur les stations")
    
    
    pdf("Graph/Pression_anthropique/Hg-muscle_pression-anthropique_moenkhausia.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
    ######################0000000000000########################
    
    
  ### Contamination en Hg selon régime alimentaire et d15N sur toute la BDD
    
    # [Hg] muscle
    
    pl1 <- ggplot(BDD[!(is.na(BDD$conc_Hg_muscle_ppm)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
      # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
      geom_point(data = df.reg.org, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.reg.org, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = c("Piscivore", "Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Périphytophage", "Herbivore","Phyllophage")), hjust=1.02, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.reg.org, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
      geom_errorbar(data = df.reg.org, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                       labels = c("Carnivore Piscivore", "Carnivore Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Herbivore Périphytophage", "Herbivore","Herbivore Phyllophage")) +
      ylab("[Hg] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans le muscle de poissons en fonction de ", delta^{15},"N selon les régimes trophiques")))
    
    pdf("Graph/Hg_isotopie/Hg-muscle_d15N_regime.pdf", width = 12, height = 9)
    print(pl1)    
    dev.off()
    
    # [Hg] foie
    
    pl2 <- ggplot( BDD[!(is.na(BDD$conc_Hg_foie_ppm)), ], aes(x = d15N, y = conc_Hg_foie_ppm)) +
      # geom_point(aes(color = Regime_alter), alpha = 0.65) +
      geom_point(data = df.reg.org, aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.reg.org, aes(x = d15N_mean, y = Hg_foie_mean, color = Regime_alter, label = c("Piscivore", "Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Périphytophage", "Herbivore","Phyllophage")), hjust=1.02, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.reg.org, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_foie_mean, x = d15N_mean, colour = Regime_alter), height = .05) + 
      geom_errorbar(data = df.reg.org, aes(ymin = Hg_foie_mean - Hg_foie_se, ymax = Hg_foie_mean + Hg_foie_se, x = d15N_mean, y = Hg_foie_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                           labels = c("Carnivore Piscivore", "Carnivore Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Herbivore Périphytophage", "Herbivore","Herbivore Phyllophage")) +
      ylab("[Hg] dans le foie de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans le foie de poissons en fonction de ", delta^{15},"N selon les régimes trophiques")))
    
    pdf("Graph/Hg_isotopie/Hg-foie_d15N_regime.pdf", width = 12, height = 9)
    print(pl2)    
    dev.off()  
    
    # [Hg] branchie
    
    pl3 <- ggplot( BDD[!(is.na(BDD$conc_Hg_branchie_ppm)), ], aes(x = d15N, y = conc_Hg_branchie_ppm)) +
      #  geom_point(aes(color = Regime_alter), alpha = 0.65) +
      geom_point(data = df.reg.org, aes(x = d15N_mean, y = Hg_branchie_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.reg.org, aes(x = d15N_mean, y = Hg_branchie_mean, color = Regime_alter, label = c("Piscivore", "Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Périphytophage", "Herbivore","Phyllophage")), hjust=1.02, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.reg.org, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_branchie_mean, x = d15N_mean, colour = Regime_alter), height = .05) + 
      geom_errorbar(data = df.reg.org, aes(ymin = Hg_branchie_mean - Hg_branchie_se, ymax = Hg_branchie_mean + Hg_branchie_se, x = d15N_mean, y = Hg_branchie_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                           labels = c("Carnivore Piscivore", "Carnivore Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Herbivore Périphytophage", "Herbivore","Herbivore Phyllophage")) +
      ylab("[Hg] dans les branchies de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans les branchies de poissons en fonction de ", delta^{15},"N selon les régimes trophiques")))
   
    
    pdf("Graph/Hg_isotopie/Hg-branchies_d15N_regime.pdf", width = 12, height = 9)
    print(pl3)    
    dev.off()
    
    
    
    # Association des graphiques
    
    ggplot(df, aes(x = Regime_alter, y = value, color = variable)) + geom_point() # Muscle = organe le plus concentré sauf chez Carnivores indéfinis
    
    
    legend <- g_legend(pl1)
    
    grid.arrange(pl1, pl2, pl3, ncol = 1, nrow = 3) # Basic
    
    grid.arrange(arrangeGrob(pl1 + theme(legend.position="none"),
                             pl2 + theme(legend.position="none"),
                             pl3 + theme(legend.position="none"),
                             ncol = 1),
                 legend, ncol = 2, nrow = 1, widths = c(9, 1), heights = c(1, 1))
    
 
    pdf("Graph/Hg_isotopie/Hg-d15N_regime.pdf", width = 20, height = 15) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(grid.arrange(arrangeGrob(pl1 + theme(legend.position="none"),
                                   pl2 + theme(legend.position="none"),
                                   pl3 + theme(legend.position="none"),
                                   ncol = 1),
                       legend, ncol = 2, nrow = 1, widths = c(9, 1), heights = c(1, 1)))
    dev.off()
    
    
    #0000000000000#
    
    ### Contamination en Hg selon régime alimentaire et d15N sur Chien, 3 sauts et Nouvelle France
    
    
     # 3 Sauts : pas d'isotopie réalisée ; pas de graphique
    
    
     # Crique Chien
    
    
      ## Non contaminée
    
    pCnC <- ggplot(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
      # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
      geom_point(data = df.chien.nonconta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.chien.nonconta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = c("Piscivore", "Insectivore", "Carnivore Invertivore", "Charognard", "Omnivore Invertivore", "Détritivore", "Périphytophage")), hjust= 0, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.chien.nonconta, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
      geom_errorbar(data = df.chien.nonconta, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                           labels = c("Carnivore Piscivore", "Carnivore Insectivore", "Carnivore Invertivore", "Carnivore Charognard", "Omnivore Invertivore", "Détritivore", "Herbivore Périphytophage")) +
      ylab("[Hg] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans le muscle des poissons de la zone non contaminée de Crique Chien en fonction de ", delta^{15},"N selon les régimes trophiques")))
    
    pdf("Graph/Hg_isotopie/Hg-muscle_d15N_regime_Chien-nonconta.pdf", width = 16.5, height = 9)
    print(pCnC)    
    dev.off()
    
    # sort(table(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "Chien_non_conta", ]$Regime_alter),decreasing=TRUE)
    
    
    
      ## Contaminée
    
    pCC <- ggplot(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
      # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
      geom_point(data = df.chien.conta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.chien.conta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = c("Piscivore", "Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Périphytophage")), hjust=1.02, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.chien.conta, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
      geom_errorbar(data = df.chien.conta, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                           labels = c("Carnivore Piscivore", "Carnivore Insectivore", "Carnivore Invertivore", "Carnivore", "Omnivore Invertivore", "Omnivore Herbivore", "Détritivore", "Herbivore Périphytophage")) +
      ylab("[Hg] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans le muscle des poissons de la zone contaminée de Crique Chien en fonction de ", delta^{15},"N selon les régimes trophiques")))
    
    pdf("Graph/Hg_isotopie/Hg-muscle_d15N_regime_Chien-conta.pdf", width = 13, height = 9)
    print(pCC)    
    dev.off()
    
   # sort(table(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "Chien_conta", ]$Regime_alter),decreasing=TRUE)
    
    #0000000000000#
    
    # Crique Nouvelle France
    
    
      ## Non Contaminée
    
    pNFnC <- ggplot(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
      # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
      geom_point(data = df.NF.nonconta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.NF.nonconta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = c("Piscivore", "Insectivore", "Carnivore Invertivore", "Scaliphage", "Charognard", "Omnivore Invertivore",  "Périphytophage", "Herbivore","Phyllophage")), hjust=1.02, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.NF.nonconta, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
      geom_errorbar(data = df.NF.nonconta, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                           labels = c("Carnivore Piscivore", "Carnivore Insectivore", "Carnivore Invertivore", "Carnivore Scaliphage", "Carnivore Charognard", "Omnivore Invertivore", "Herbivore Périphytophage", "Herbivore","Herbivore Phyllophage")) +
      ylab("[Hg] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans le muscle des poissons de la zone non contaminée de Crique Nouvelle France en fonction de ", delta^{15},"N selon les régimes trophiques")))
    
    pdf("Graph/Hg_isotopie/Hg-muscle_d15N_regime_NF-nonconta.pdf", width = 14, height = 9)
    print(pNFnC)    
    dev.off()
    
    # sort(table(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "NF_non_conta", ]$Regime_alter),decreasing=TRUE)
    
    
    
      ## Contaminée
    
    
    pNFC <- ggplot(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
      # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
      geom_point(data = df.NF.conta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 4) +
      geom_text(data = df.NF.conta, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = c("Piscivore", "Carnivore Invertivore", "Omnivore Invertivore", "Périphytophage")), hjust= 0, vjust=-1, size = 6.5) +
      geom_errorbarh(data = df.NF.conta, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
      geom_errorbar(data = df.NF.conta, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05) +
      scale_color_discrete(name = "Régime trophique",
                           labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore Invertivore", "Herbivore Périphytophage")) +
      ylab("[Hg] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab(expression(paste(delta^{15},'N'))) +
      ggtitle(expression(paste("[Hg] dans le muscle des poissons de la zone contaminée de Crique Nouvelle France en fonction de ", delta^{15},"N selon les régimes trophiques")))
    
    pdf("Graph/Hg_isotopie/Hg-muscle_d15N_regime_NF-conta.pdf", width = 13.8, height = 9)
    print(pNFC)    
    dev.off()
    
    # sort(table(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)) & BDD_PME$Groupe_station %in% "NF_conta", ]$Regime_alter),decreasing=TRUE)
    
    # Informations sur le nombre d'individus dans chaque condition
    ggplot(BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)),], aes(x = Regime_alter, y = conc_Hg_muscle_ppm)) +
      geom_point(position="jitter") + facet_wrap(~ Groupe_station)
    
    View(ftable(xtabs(~ Groupe_station + Regime_alter, data = BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)),])))

    ftable(xtabs(~ Groupe_station + Regime_principal, data = BDD_PME[!(is.na(BDD_PME$conc_Hg_muscle_ppm)) & !(is.na(BDD_PME$d15N)),]))

    
    ######################0000000000000########################
    
    
    # Analyse des éléments traces : Camopi, Nouvelle france et 3 Sauts
    
    
    # sub_BDD_PME
    
    
    ### ACP
    # As enlevé car il y a définitivement un problème avec les [c] initiales
    # Ensuite, problème avec Se car aucun échantillon de Trois Sauts n'a de valeur
    
    df.trace.Se <- sub_BDD_PME[,c(53:57, 59:62)]
    df.trace.sansSe <- sub_BDD_PME[,c(53:57, 60:62)]
    
    # en cas d'imputation, le Se se détache clairement. Mais vu qu'une hypothèse
    # Aurait pu être un lien entre Se et Hg et que l'imputation ne peut pas en tenir compte, est ce vraiment pertinent de la réaliser ?
    # Toutes données sans Se
    res.pca <- PCA(df.trace.sansSe, scale.unit=TRUE)
    nb <- estim_ncpPCA(df.trace.sansSe, ncp.min = 0, ncp.max = 5)
    res.impute <- imputePCA(df.trace.sansSe, ncp = 2)
    res.acp <- PCA (res.impute$completeObs)
    
       ## Matrice corrélation de l'ACP
    
    mcor <- cor(res.impute$completeObs)
    res1 <- cor.mtest(res.impute$completeObs, 0.95)
    
    corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05)
    corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05, insig = "pch", method="shade", shade.col=NA, tl.col="black", tl.srt=45,  addCoef.col="black", addcolorlabel="no", order="FPC")
    
    
    
    # res.MI <- MIPCA(df.elt.trace, scale = TRUE, ncp = 2)
    # plot(res.MI) # problème de mémoire ?
    
    # Données sans 3 Sauts mais avec Se
    df <- sub_BDD_PME[,c(53:57, 59:62)]
    df.sans3sauts <- df[!(is.na(df[,'Se_ppm'])),]
    res.impute <- imputePCA(df.sans3sauts, ncp = 2)
    res.acp <- PCA(df.sans3sauts)
    
        ## Matrice corrélation de l'ACP
    
    mcor <- cor(res.impute$completeObs)
    res1 <- cor.mtest(res.impute$completeObs, 0.95)
    
    corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05)
    corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05, insig = "pch", method="shade", shade.col=NA, tl.col="black", tl.srt=45, addCoef.col="black", addcolorlabel="no", order="FPC")
    
    
    #0000000000000#
    
    ## Se
    
#   ggplot(sub_BDD_PME2, aes(x = Groupe_station, y = Se_ppm, color = Regime_principal)) +
#      geom_boxplot() +
#      scale_x_discrete(limits = c("Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique", 
#                           labels = c("Carnivore", "Omnivore", "Herbivore")) +
#      ylab("[Se] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Se] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    
    Se <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Se_ppm, color = Regime_alter)) +
      geom_boxplot() +
      scale_x_discrete(limits = c("Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Se] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Se] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Se.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Se)
    dev.off()
    
    
    
    dcast(sub_BDD_PME4, Groupe_station ~ Regime_alter, length)
    
    ## Ni
    
#   ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Ni_ppm, color = Regime_principal)) +
#      geom_boxplot() +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Ni] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Ni] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    
    Ni <-   ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Ni_ppm, color = Regime_alter)) +
      geom_boxplot() +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Ni] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Ni] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
  
    
    pdf("Graph/Elements_traces/Ni.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Ni)
    dev.off()
    
      
    
      ### MCA sur Ni
         
    Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Ni_ppm)
    
    elt.trace('Ni_ppm') 
    
    MCA_Ni <- p + ggtitle("Analyse des correspondances multiples : Ni") +
      xlab("Dimension 1. 17,3 % de variance expliquée") +
      ylab("Dimension 2. 14,8 % de variance expliquée") +
      scale_colour_discrete(name = "Variable", label = c("Intervalle [Ni] en mg/kg ps", "Groupe de stations", "Régime trophique"))
    #+ geom_text(label = c ("Chien contaminée", "Chien non contaminée", "NF contaminée", "NF non contaminée", "Trois Sauts", "Carnivore Piscivore", "Carnivore Invertivore", rownames(mca1_vars_df[8:14, ])))
    
    
    pdf("Graph/Elements_traces/MCA_Ni.pdf", width = 14, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(MCA_Ni)
    dev.off()
    
    
    ## Cu
    
#    ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cu_ppm, color = Regime_principal)) +
#      geom_boxplot() +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Cu] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Cu] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
      
      
    
    Cu <-  ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Cu_ppm, color = Regime_alter)) +
      geom_boxplot() +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Cu] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Cu] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Cu.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Cu)
    dev.off()    
    
    
    
      ## MCA
    
    
    Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Cu_ppm)
        
    elt.trace('Cu_ppm') 
    
    MCA_Cu <- p + ggtitle("Analyse des correspondances multiples : Cu") +
      xlab("Dimension 1. 16,3 % de variance expliquée") +
      ylab("Dimension 2. 15,6 % de variance expliquée") +
      scale_colour_discrete(name = "Variable", label = c("Intervalle [Cu] en mg/kg ps", "Groupe de stations", "Régime trophique"))
    #+ geom_text(label = c ("Chien contaminée", "Chien non contaminée", "NF contaminée", "NF non contaminée", "Trois Sauts", "Carnivore Piscivore", "Carnivore Invertivore", rownames(mca1_vars_df[8:14, ])))
    
    
    pdf("Graph/Elements_traces/MCA_Cu.pdf", width = 14, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(MCA_Cu)
    dev.off()
    
    
    
    ## Zn
    
#   ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Zn_ppm, color = Regime_principal)) +
#      geom_boxplot() +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Zn] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Zn] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
      
    
    Zn <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Zn_ppm, color = Regime_alter)) +
      geom_boxplot() +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Zn] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Zn] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Zn.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Zn)
    dev.off()    
    
    
       ## MCA
    
    
    Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Zn_ppm)
        
    elt.trace('Zn_ppm') 
    
    MCA_Zn <- p + ggtitle("Analyse des correspondances multiples : Zn") +
      xlab("Dimension 1. 13,6 % de variance expliquée") +
      ylab("Dimension 2. 12 % de variance expliquée") +
      scale_colour_discrete(name = "Variable", label = c("Intervalle [Zn] en mg/kg ps", "Groupe de stations", "Régime trophique"))
    
    
    pdf("Graph/Elements_traces/MCA_Zn.pdf", width = 14, height = 9)
    print(MCA_Zn)
    dev.off()
    
    
    ########
    
    Bd$elt_qual <- quantcut(Bd[,'Zn_ppm'], q = seq(0, 1, by = 0.2))
    Bd$elt_qual <- as.factor(Bd$elt_qual)
    Bd2 <- Bd[,- 3]
    
    
    # cats <- NULL
    cats <- apply(Bd2, 2, function(x) nlevels(as.factor(x)))
    
    mca1 <- MCA(Bd2)
    
    #mca1_vars_df <- NULL
    mca1_vars_df <<- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
    
    # data frame with observation coordinates
    # mca1_obs_df <- NULL
    mca1_obs_df <<- data.frame(mca1$ind$coord)
    
    # MCA plot of observations and categories
    ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
      geom_hline(yintercept = 0, colour = "gray70") +
      geom_vline(xintercept = 0, colour = "gray70") +
      geom_point(colour = "gray50", alpha = 0.7) +
      geom_density2d(colour = "gray80") +
      geom_text(data = mca1_vars_df, 
                aes(x = Dim.1, y = Dim.2, 
                    label = rownames(mca1_vars_df), colour = Variable)) +
      scale_colour_discrete(name = "Variable")
    
    
    
        # Clustering on principal components
    
  
    #http://factominer.free.fr/classical-methods/hierarchical-clustering-on-principal-components.html
  
    # Pb actuel : ne prend pas en compte le vrai jeu de données
    
    res.hcpc <- HCPC(mca1, method = "ward.D2") # Création des groupes automatique, si besoin précision ac argument nb.clust
    
    res.hcpc$desc.var$test.chi2 # Variables qui caractérisent le mieux la séparation entre les groupes
    res.hcpc$desc.var$category # Pr chq cluster, informations sur sa composition
    res.hcpc$desc.ind # indiv caractéristiques de chaque groupe & indiv de chq les plus éloignés des autres groupes
    
    # Comment faire apparaître les cluster sur le graphe ?
  
    
    
    ## As
    
   As <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = As_ppm, color = Regime_alter)) +
      geom_boxplot() +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[As] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[As] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/As.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(As)
    dev.off()    
    
    
    ## Co
    
#    ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Co_ppm, color = Regime_principal)) +
#      geom_boxplot() +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Co] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Co] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    
    Co <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Co_ppm, color = Regime_alter)) +
      geom_boxplot() +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Co] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Co] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Co.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Co)
    dev.off()    
    
    
       ## MCA
    
    
    Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Co_ppm)
    
    elt.trace('Co_ppm') 
    
    MCA_Co <- p + ggtitle("Analyse des correspondances multiples : Co") +
      xlab("Dimension 1. 16,9 % de variance expliquée") +
      ylab("Dimension 2. 14,1 % de variance expliquée") +
      scale_colour_discrete(name = "Variable", label = c("Intervalle [Co] en mg/kg ps", "Groupe de stations", "Régime trophique"))
    
    
    pdf("Graph/Elements_traces/MCA_Co.pdf", width = 20, height = 9)
    print(MCA_Co)
    dev.off()
    
    ## Cd
    
#   ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cd_ppm, color = Regime_principal)) +
#      geom_boxplot() + geom_hline(aes(yintercept = 0.5)) +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Cd] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Cd] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    
    Cd <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Cd_ppm, color = Regime_alter)) +
      geom_boxplot() + geom_hline(aes(yintercept = 0.5))  +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Cd] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Cd] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Cd.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Cd)
    dev.off()    
    
    
       ## MCA
    
    # Aucun intérêt ici, pas de conta
    
    #Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Cd_ppm)
    
    #elt.trace('Cd_ppm') + ggtitle("MCA plot of Cd")    
    
    
    
    ## Pb
    
#   ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Pb_ppm, color = Regime_principal)) +
#      geom_boxplot() + geom_hline(aes(yintercept = 1.5)) +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Pb] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Pb] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    
    Pb <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Pb_ppm, color = Regime_alter)) +
      geom_boxplot() + geom_hline(aes(yintercept = 2.5))  +
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Pb] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Pb] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Pb.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Pb)
    dev.off()    
    
       ## MCA
    
    
    Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Pb_ppm)
    
    elt.trace('Pb_ppm') 
    
    MCA_Pb <- p + ggtitle("Analyse des correspondances multiples : Pb") +
      xlab("Dimension 1. 17,3 % de variance expliquée") +
      ylab("Dimension 2. 15,5 % de variance expliquée") +
      scale_colour_discrete(name = "Variable", label = c("Intervalle [Pb] en mg/kg ps", "Groupe de stations", "Régime trophique"))
    
    
    pdf("Graph/Elements_traces/MCA_Pb.pdf", width = 14, height = 9)
    print(MCA_Pb)
    dev.off()
    
    ## Cr
    
    
#   ggplot(sub_BDD_PME, aes(x = Groupe_station, y = Cr_ppm, color = Regime_principal)) +
#      geom_boxplot() +
#      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
#                       labels = c("Trois Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
#      scale_color_discrete(name = "Régime trophique",
#                           labels = c("Carnivore", "Omnivore", "Détritivore", "Herbivore")) +
#      ylab("[Cr] dans le muscle de poissons, en mg/kg de poids sec") +
#      xlab("Groupe de stations") +
#      ggtitle(expression(paste("[Cr] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    
    Cr <- ggplot(sub_BDD_PME4, aes(x = Groupe_station, y = Cr_ppm, color = Regime_alter)) +
      geom_boxplot()+
      scale_x_discrete(limits = c("Trois_Sauts", "Chien_non_conta", "Chien_conta", "NF_non_conta", "NF_conta"),
                       labels = c("Trois_Sauts", "Chien non contaminée", "Chien contaminée", "Nouvelle France non contaminée", "Nouvelle France contaminée")) +
      scale_color_manual(name = "Régime trophique", labels = c("Carnivore Piscivore", "Carnivore Invertivore", "Omnivore", "Herbivore"), values = color) +
      ylab("[Cr] dans le muscle de poissons, en mg/kg de poids sec") +
      xlab("Groupe de stations") +
      ggtitle(expression(paste("[Cr] dans le muscle de poissons en fonction des groupes de stations et des régimes trophiques")))
    
    pdf("Graph/Elements_traces/Cr.pdf", width = 13, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(Cr)
    dev.off()    
    
        ## MCA
    
    
    Bd <- select(sub_BDD_PME4, Groupe_station, Regime_alter, Cr_ppm)
    
    elt.trace('Cr_ppm') 
    
    
    MCA_Cr <- p + ggtitle("Analyse des correspondances multiples : Cr") +
      xlab("Dimension 1. 16,3 % de variance expliquée") +
      ylab("Dimension 2. 11,5 % de variance expliquée") +
      scale_colour_discrete(name = "Variable", label = c("Intervalle [Cr] en mg/kg ps", "Groupe de stations", "Régime trophique"))
    
    
    pdf("Graph/Elements_traces/MCA_Cr.pdf", width = 20, height = 10)
    print(MCA_Cr)
    dev.off()
    
    
    ### MCA sur Hg
    
    Bd <- na.omit(select(sub_BDD_PME4, Groupe_station, Regime_alter, Hg_ppm))
    
    elt.trace('Hg_ppm') + ggtitle("MCA plot of Hg")
    