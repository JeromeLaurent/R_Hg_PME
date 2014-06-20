#Impact taille sur [Hg]

# valeur médianes
df <- ddply(BDD, .(Pression_anthro2), summarise, moyen = median(pds_g, na.rm = TRUE) )


nrow(BDD[!(is.na(BDD$pds_g)) & !(is.na(BDD$conc_Hg_muscle_ppm)),]) #2581

nrow(BDD[BDD$pds_g < 100 & !(is.na(BDD$conc_Hg_muscle_ppm)),]) #2554, 98 % des poissons 
#font moins de 100g

db <- BDD[BDD$pds_g < 100 & !(is.na(BDD$conc_Hg_muscle_ppm)),]
median(db$pds_g, na.rm = TRUE)

cor.test(db$pds_g, db$conc_Hg_muscle_ppm)

##### Hg fonction poids

#Scatter
scatter <- ggplot(BDD, aes(y = conc_Hg_muscle_ppm, x = pds_g)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 100)) +
  xlab("Poids en grammes") + ylab("[Hg] muscle des poissons, mg/kg ps")

#marginal density of x - plot on top
plot_top <- ggplot(BDD, aes(pds_g)) + 
  geom_density(alpha = .5) + 
  scale_x_continuous(limits = c(0, 100)) + # pour apercu plus detaille de la distrib de la majorite des poissons
  theme(legend.position = "none") + theme(axis.title.x = element_blank())

#marginal density of y - plot on the right
plot_right <- ggplot(BDD, aes(conc_Hg_muscle_ppm)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(0, 50)) + # limites d'apres divers essais. poissons tres legers
  coord_flip() + 
  theme(legend.position = "none") + theme(axis.title.y = element_blank())

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

pdf("Graph/Hg_fonction_masse.pdf", width = 9, height = 6) # la fction pdf enregistre directement ds le dossier et sous format pdf
print(grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4)))
dev.off()



cor.test(BDD[BDD$pds_g < 100 & !(is.na(BDD$conc_Hg_muscle_ppm)),][['pds_g']],
         BDD[BDD$pds_g < 100 & !(is.na(BDD$conc_Hg_muscle_ppm)),][['conc_Hg_muscle_ppm']], alternative = "greater", method = "spearman")
# p-value = 2.2e-16, cor 0.16 -> il existe une corrélation mais elle est minime



Bd <- BDD.sansNA

levels(Bd$Regime_alter) <- sub("^Carnivore_Charognard$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Insectivore$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Scaliphage$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Herbivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Piscivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Herbivore$", "Herbivore_Phyllophage", levels(Bd$Regime_alter))

kruskal.test(conc_Hg_muscle_ppm ~ Regime_alter, data = Bd)
comparaison <- kruskal(Bd$conc_Hg_muscle_ppm, Bd$Regime_alter, alpha = 0.05, p.adj = "holm")

pl <- ggplot(Bd, aes(y = conc_Hg_muscle_ppm, x = Regime_alter)) +
  geom_boxplot(aes(fill = Regime_alter)) +
  scale_y_continuous(limits = c(0,2.5)) +
  scale_fill_manual(values = colo8) +
  ylab("[Hg] muscle des poissons, mg/kg ps") +
  scale_x_discrete(name = "Régime trophique", labels = c("Piscivore", "(autres)", "Invertivore", "(autres)", "Invertivore", "Détritivore", "Périphytophage", "Phyllophage", "NA")) +
  theme(legend.position = "none")
# version alternative : scale_x_discrete(name = "Régime trophique", labels = c("Carnivore\nPiscivore", "Carnivore\n(autres)", "Carnivore\nInvertivore", "Omnivore\n(autres)", "Omnivore\nInvertivore", "Détritivore", "Herbivore\nPériphytophage", "Herbivore\nPhyllophage", "NA")) +



pdf("Graph/Hg_fonction_regime.pdf", width = 9, height = 6) # la fction pdf enregistre directement ds le dossier et sous format pdf
print(pl)
dev.off()



###### d15N fction d13C

Bd <- BDD

levels(Bd$Regime_alter) <- sub("^Carnivore_Charognard$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Insectivore$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Scaliphage$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Herbivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Piscivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Insectivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Periphytophage$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Herbivore$", "Herbivore_Phyllophage", levels(Bd$Regime_alter))



#Scatter
scatter <- ggplot(Bd, aes(y = d15N, x = d13C, color = Regime_alter)) +
        geom_point(alpha = 0.5) +
        scale_color_manual(values = colo8) +
        xlab(expression(paste(delta^{13},'C'))) + ylab(expression(paste(delta^{15},'N'))) +
        theme(legend.position = "none")

#boxplot on top
plot_top <- ggplot(Bd, aes(y = d13C, x = Regime_alter, color = Regime_alter)) + 
        geom_boxplot(outlier.shape = NA) + 
        scale_color_manual(values = colo8) +
        scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
        coord_flip() +
        xlab("Régimes") +
        theme(legend.position = "none") + theme(axis.title.x = element_blank())

#boxplot on the right
plot_right <- ggplot(Bd, aes(y = d15N, x = Regime_alter, color = Regime_alter)) + 
        geom_boxplot(outlier.shape = NA) +
        scale_color_manual(values = colo8) +
        scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
        xlab("Régimes") +
        theme(legend.position = "none") + theme(axis.title.y = element_blank())

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

pdf("Graph/d13C_d15N.pdf", width = 9, height = 7) # la fction pdf enregistre directement ds le dossier et sous format pdf
print(grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(2, 2), heights = c(2, 2)))
dev.off()


##### Isotopie #####


Bd <- BDD

levels(Bd$Regime_alter) <- sub("^Carnivore_Charognard$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Insectivore$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Carnivore_Scaliphage$", "Carnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Herbivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Piscivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Insectivore$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Omnivore_Periphytophage$", "Omnivore", levels(Bd$Regime_alter))
levels(Bd$Regime_alter) <- sub("^Herbivore$", "Herbivore_Phyllophage", levels(Bd$Regime_alter))


df.reg.muscle <- Bd[!(is.na(Bd$conc_Hg_muscle_ppm)) & !(is.na(Bd$d15N)), ] %.% # Selection BDD globale
        group_by(Regime_alter) %.% # Sélection par régime
        filter(Regime_alter != "Carnivore_Scaliphage" & Regime_alter != "Herbivore") %.% # régimes mineurs ou trop flous retirés
        summarise(Hg_muscle_mean = mean(na.omit(conc_Hg_muscle_ppm)), d15N_mean = mean(na.omit(d15N)), Hg_muscle_se = se(na.omit(conc_Hg_muscle_ppm)),
                  d15N_se = se(na.omit(d15N)), d13C_se = se(na.omit(d13C)), d13C_mean = mean(na.omit(d13C))) # Sélection des données à calculer

df.reg.muscle <- na.omit(df.reg.muscle)

pl <- ggplot(Bd[!(is.na(Bd$conc_Hg_muscle_ppm)) & !(is.na(Bd$d15N)), ], aes(x = d15N, y = conc_Hg_muscle_ppm)) +
        # geom_point(data = df.sp.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, fill = Code), show_guide = FALSE) +
        geom_point(data = df.reg.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter), size = 2) +
        geom_text(data = df.reg.muscle, aes(x = d15N_mean, y = Hg_muscle_mean, color = Regime_alter, label = c("1", "2", "3", "4", "5", "6", "7", "8")), hjust = 1.3, vjust = 1.3, size = 6.5) +
        geom_errorbarh(data = df.reg.muscle, aes(xmin = d15N_mean + d15N_se, xmax = d15N_mean - d15N_se, y = Hg_muscle_mean, x = d15N_mean, colour = Regime_alter), height = .025) + 
        geom_errorbar(data = df.reg.muscle, aes(ymin = Hg_muscle_mean - Hg_muscle_se, ymax = Hg_muscle_mean + Hg_muscle_se, x = d15N_mean, y = Hg_muscle_mean, colour = Regime_alter), width = .05) +
        #scale_x_continuous(limits = c(7.5, 11.8)) +
        scale_color_manual(name = "Régime trophique",
                           labels = c("1 : Carnivore Piscivore, n=91", "2 : Carnivore (autres), n=77", "3 : Carnivore Invertivore, n=324", "4 : Omnivore (autres), n=18", "5 : Omnivore Invertivore, n=437", "6 : Détritivore, n=14", "7 : Herbivore Périphytophage, n=50","8 : Herbivore Phyllophage, n=8"),
                           values = colo8) +
        ylab("[Hg] dans le muscle de poissons, en mg/kg de poids sec") +
        xlab(expression(paste(delta^{15},'N, niveau trophique'))) +
        guides(colour = guide_legend(override.aes = list(size = 10)))+
        theme(legend.key=element_rect(fill=NA)) +
        ggtitle(expression(paste("[Hg] dans le muscle de poissons en fonction de ", delta^{15},"N selon les régimes trophiques")))

pdf("Graph/Hg_isotopie/Hg-muscle-only_d15N_regime3.pdf", width = 8, height = 6)
print(pl)    
dev.off()
