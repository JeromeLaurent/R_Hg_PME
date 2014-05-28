##############################################
#### Fonctions et objets utiles a appeler ####
##############################################



## IMPORTANT

## assigner un objet avec <<- permet de le créer en dehors de la fonction !


######################0000000000000########################

# Permet de créer un graph vide comprenant uniquement un point blanc, qui pourra ensuite être utilisé dans les graphes
# représentant les distributions marginales de nuages de points

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


######################0000000000000########################


# Extraire la légende d'un graph pour éviter les répétitions par exemple

# a.gplot est un graph produit par ggplot

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


######################0000000000000########################


# Boxcox

# x un vecteur, lambda un coefficient

box.cox <- function(x, lambda) {
  if (lambda==0) log(x) else ((x)^lambda - 1)/lambda
}


######################0000000000000########################


# se

# x étant un vecteur

se <- function(x) sqrt(var(x)/length(x))


######################0000000000000########################


# Test de visualidation ; provient du package Teaching Demos

require(TeachingDemos)

# avec x un vecteur dont on veut comparer la distribution à des générations aléatoires de distributions normales
# afin d'estimer si il existe des différences visuelles ou non

test.visu <- function (x) {
  if(interactive()) {
    vis.test(x, vt.qqnorm)
    vis.test(x, vt.normhist)
  }
}


######################0000000000000########################


# Affichage de matrice de corréaltions. Uniquement les correlations statistiquement significatives

require(corrplot)

#mcor <- cor((BDD[,c(40, 41, 56:65)]), use="complete.obs")
#mat <- (BDD[,c(40, 41, 56:65)])
# Print mcor and round to 2 digits
#round(mcor, digits=2)
#col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,  col=col(200), addCoef.col="black", addcolorlabel="no", order="FPC")


## Avec "mat" une matrice de corrélation obtenue à partir de la fonction cor()

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


#res1 <- cor.mtest(mat, 0.95)
## specialized the insignificant value according to the significant level
#corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05)
#corrplot(mcor, p.mat = res1[[1]], sig.level = 0.05, insig = "pch", method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", addcolorlabel="no", order="FPC")


### OU ###

require(corrgram)

# Se renseigner, je ne suis pas tout à fait sûr de comprendre exactement son fonctionnement.


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
#corrgram(BDD, type="data", lower.panel=panel.shadeNtext,  upper.panel=NULL)


######################0000000000000########################


elt.trace <- function(element){

Bd$elt_qual <- quantcut(Bd[,element], q = seq(0, 1, by = 0.2))
Bd$elt_qual <- as.factor(Bd$elt_qual)
Bd2 <<- Bd[,- 3]


# cats <- NULL
cats <<- apply(Bd2, 2, function(x) nlevels(as.factor(x)))

mca1 <<- MCA(Bd2)

#mca1_vars_df <- NULL
mca1_vars_df <<- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

#(rownames(mca1_vars_df[1,])) <- "Chien contaminée"

# data frame with observation coordinates
# mca1_obs_df <- NULL
mca1_obs_df <<- data.frame(mca1$ind$coord)

# MCA plot of observations and categories
p <<- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  scale_colour_discrete(name = "Variable")

}

######################0000000000000########################

# Obtention du mode d'un vecteur

Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}


######################0000000000000########################

# Conversion de nombreuses colonnes d'une dataframe.
# source : http://stackoverflow.com/questions/11261399/function-for-converting-dataframe-column-type

convert.magic <- function(obj, type){
        FUN1 <- switch(type,
                       character = as.character,
                       numeric = as.numeric,
                       factor = as.factor)
        out <- lapply(obj, FUN1)
        as.data.frame(out)
}


# df[, c("x", "y")] <- convert.magic(df[, c("x", "y")], "factor") # converti les colonnes x et y en facteur