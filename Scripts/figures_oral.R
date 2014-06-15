#Impact taille sur [Hg]

# valeur médianes
df <- ddply(BDD, .(Pression_anthro2), summarise, moyen = median(pds_g, na.rm = TRUE) )


nrow(BDD[!(is.na(BDD$pds_g)) & !(is.na(BDD$conc_Hg_muscle_ppm)),]) #2581

nrow(BDD[BDD$pds_g < 100 & !(is.na(BDD$conc_Hg_muscle_ppm)),]) #2554, 98 % des poissons 
#font moins de 100g

#Scatter
scatter <- ggplot(BDD, aes(y = conc_Hg_muscle_ppm, x = pds_g)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 100))

#marginal density of x - plot on top
plot_top <- ggplot(BDD, aes(pds_g)) + 
  geom_density(alpha = .5) + 
  scale_x_continuous(limits = c(0, 100)) + # pour apercu plus detaille de la distrib de la majorite des poissons
  theme(legend.position = "none")

#marginal density of y - plot on the right
plot_right <- ggplot(BDD_PME, aes(conc_Hg_muscle_ppm)) + 
  geom_density(alpha = .5) + 
  #scale_x_continuous(limits = c(0, 50)) + # limites d'apres divers essais. poissons tres legers
  coord_flip() + 
  theme(legend.position = "none") 

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))





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

ggplot(Bd, aes(y = conc_Hg_muscle_ppm, x = Regime_alter)) +
  geom_boxplot() +
  scale_x_discrete(name = "Régime trophique",
                   labels = c("Carnivore Piscivore", "Carnivore (autres)", "Carnivore Invertivore", "Omnivore (autres)", "Omnivore Invertivore", "Détritivore", "Herbivore Périphytophage", "Herbivore Phyllophage", "NA"))
                    
