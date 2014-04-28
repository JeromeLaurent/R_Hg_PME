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
require(gtable) # alternative pour arranger graphe ensembles
require(missMDA)
require(agricolae)

source("Scripts/functions.R")
source("Scripts/data_cleaning.R")
    

    
    #### SOMMAIRE ####
    
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
    
    
#o# Ensemble de la BDD
    
  ### Impact des pressions anthropiques
    
    BD <- select(BDD.sansNA, conc_Hg_muscle_ppm, Pression_anthro, Regime_principal, Regime_alter) # Subset plus simple a  manipuler
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD) # Il esxiste des differences significatives
    
    comparison <- kruskal(BD$conc_Hg_muscle_ppm, BD$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
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
    p0 <- p0 + geom_text(aes(Pression_anthro, upper + 0.1, label = signif), size = 10, data = test_f, vjust = -2, color = "red") +
      scale_x_discrete(limits = c( "Reference", "Agriculture", "Deforestation", "Piste", "Orpaillage_ancien", "Orpaillage_illegal", "Barrage"),
                       labels = c("Référence", "Agriculture", "Déforestation", "Piste", "Orpaillage ancien",  "Orpaillage illégal récent", "Barrage")) +
      labs( y = "[Hg] dans les muscles de poissons, en mg/kg de poids sec",
            x = "Pression anthropique", title = "[Hg] dans les muscles de poissons selon les pressions anthropiques exercées sur les stations") +
      geom_hline(aes(yintercept = 2.5), color = "red")
    
    
    pdf("Graph/Hg-muscle_pression-anthropique.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()

  
  ### Impact des pressions anthropiques sur les régimes principaux
    
    BD.carn <- BD[BD$Regime_principal %in% "Carnivore",]
    BD.omni <- BD[BD$Regime_principal %in% "Omnivore",]
    # BD.herbi <- BD[BD$Regime_principal %in% "Herbivore",] # Trop peu d'échantillons, qui plus est inégalement répartis
    # BD.detri <- BD[BD$Regime_principal %in% "Detritivore",] # Uniquement 16 indiv donc pas assez de données
    
    # Carnivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.carn, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.carn) # Il esxiste des differences significatives
    
    comparison <- kruskal(BD.carn$conc_Hg_muscle_ppm, BD.carn$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.carn, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      #scale_colour_brewer(palette="Set3") +
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
    
    
    pdf("Graph/Hg-muscle_pression-anthropique_carnivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
    # Omnivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.omni, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.omni) # Il esxiste des differences significatives
    
    comparison <- kruskal(BD.omni$conc_Hg_muscle_ppm, BD.omni$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.omni, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      #scale_colour_brewer(palette="Set3") +
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
    
    
    pdf("Graph/Hg-muscle_pression-anthropique_omnivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
  ### Impact des pressions anthropiques sur les régimes détaillés
    
        
    BD.omn.inver <- BD[BD$Regime_alter %in% "Omnivore_Invertivore",]
    BD.car.inver <- BD[BD$Regime_alter %in% "Carnivore_Invertivore",]
    BD.car.pisc <- BD[BD$Regime_alter %in% "Carnivore_Piscivore",]
    # BD.car.insec <- BD[BD$Regime_alter %in% "Carnivore_Insectivore",] # pas de données partout

    
    # Omnivores Invertivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.omn.inver, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.omn.inver) # Il esxiste des differences significatives
    
    comparison <- kruskal(BD.omn.inver$conc_Hg_muscle_ppm, BD.omn.inver$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.omn.inver, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      #scale_colour_brewer(palette="Set3") +
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
    
    
    pdf("Graph/Hg-muscle_pression-anthropique_omnivores-invertivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    
    # Carnivores Invertivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.car.inver, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.car.inver) # Il esxiste des differences significatives
    
    comparison <- kruskal(BD.car.inver$conc_Hg_muscle_ppm, BD.car.inver$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.car.inver, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      #scale_colour_brewer(palette="Set3") +
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
    
    
    pdf("Graph/Hg-muscle_pression-anthropique_carnivores-invertivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    
    # Carnivores Piscivores
    
    means.pression <- aggregate(conc_Hg_muscle_ppm ~  Pression_anthro, BD.car.pisc, mean)
    means.pression$conc_Hg_muscle_ppm <- round(means.pression$conc_Hg_muscle_ppm, digits = 2)
    
    kruskal.test(conc_Hg_muscle_ppm ~ Pression_anthro, data = BD.car.pisc) # Il esxiste des differences significatives
    
    comparison <- kruskal(BD.car.pisc$conc_Hg_muscle_ppm, BD.car.pisc$Pression_anthro, alpha = 0.05, p.adj = "holm")
    
    posthoc <- comparison[['groups']]
    posthoc$trt <- gsub(" ","",posthoc$trt) # Tous les espaces apres le nom doivent etre supprimes pour pouvoir merge par la suite
    
    
    p0 <- ggplot(BD.car.pisc, aes(x = Pression_anthro , y = conc_Hg_muscle_ppm)) +
      geom_boxplot() +
      #scale_colour_brewer(palette="Set3") +
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
      
    
    
    pdf("Graph/Hg-muscle_pression-anthropique_carnivores-piscivores.pdf", width = 12, height = 9) # la fction pdf enregistre directement ds le dossier et sous format pdf
    print(p0)
    dev.off()
    

    
