# geobs dataviz


# objectif : 
library("doBy", lib.loc="C:/R-3.5.0/library")
library("gridExtra", lib.loc="C:/R-3.5.0/library")
library(forcats) 

# 00 charge lib -----------------------------------------------------------
library(sf)
library(spatstat)
library(sp)
library(maptools)
library(raster)
library(cartography)
library(SpatialPosition)
library(ggplot2)
library(ggplotgui)
library(doBy)
library(gghighlight)
library(dplyr)
library(rgdal)
library(remotes)
library(readr)
library("gghighlight", lib.loc="C:/R-3.5.0/library")
library(ggridges)
library("viridis", lib.loc="C:/R-3.5.0/library")
library(GGally)
library(RColorBrewer)
library(colorRamps)
library(ggrepel)
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggpubr)
library(reshape2)
library(readr)
library(doBy)
library(ggplot2)
library(ggthemes)
library(viridis)
require(RPostgreSQL)
library(dplyr)
library(forcats) # pour ordonner les graphes autoamtiquement
library(waffle)
library ("FactoMineR")
library(ade4)


# dossier de travail ------------------------------------------------------
setwd("G:/00_data_ref/data_gouv_fr/geobs")


# URL SOURCE --------------------------------------------------------------
# URL SOURCE https://www.data.gouv.fr/fr/organizations/umr-5319-passages/

geobs_animidg_2017 <- read_delim("G:/00_data_ref/data_gouv_fr/geobs/GEOBS_AnimationsIDG2017.csv", 
                                           ";", escape_double = FALSE, trim_ws = TRUE)


View(geobs_animidg_2017)

ggplot(geobs_animidg_2017)

# 1er graphe fait avec esquisse (ggplot builder)
ggplot(data = geobs_animidg_2017) +
  aes(x = `Formation au catalogage (visualisation / saisie de métadonnées)`, fill = NIVEAU) +
  geom_bar() +
  labs(title = "Formations au Catalogage") +
  theme_igray() +
  theme(legend.position = 'bottom') +
  facet_wrap(vars(NIVEAU))


# isoler les formations
grep("^formation$", colnames(geobs_animidg_2017))

# sous ensemble pour les formations
df_form <- geobs_animidg_2017[c(1:8)]

melt_form <- melt(df_form, id='IDG')
library(ggplot2)



ggplot(data = melt_form_reg) +
  aes(x = variable, fill = value) +
  geom_bar() +
  theme_minimal() +
  coord_flip()

ggplot(data = melt_form_reg) +
  aes(x = variable, fill = value) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Type de formation", y="nombre", caption = "GéOBS") +  theme_calc() +
  coord_flip()

#dcast(subset(melt_form,variable!='NIVEAU'), melt_form$variable, length)

# sous ensemble animation
df_form
# https://stats.stackexchange.com/questions/9937/finding-the-column-index-by-its-name-in-r
# grep("^3D$*", colnames(geobs_animidg_2017))
df_thema <- geobs_animidg_2017[c(1,2,19:54)]

melt_thema <- melt(df_thema, id=c('IDG','NIVEAU'))

df_thema[df_thema=="N/A"]<-0
melt_thema[melt_thema=="N/A"]<-0
df_thema<-type.convert(df_thema)
df_thema$nb_gt <- rowSums(df_thema[,3:38])

str(df_thema)

melt_thema_comp <- melt(df_thema, id=c('IDG','NIVEAU','nb_gt'))

df_thema

# df_thema[,3:29] <- strtoi(df_thema[,3:29])
str(df_thema)
x <- factor(sample(4:8,10,replace=T))
as.numeric(levels(x))[x]
# problematique de chagement de structure de character to numeric
# https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type
#  df_thema[,3] <- as.numeric(df_thema[,3]) NE MARCHE PAS 
# https://stat.ethz.ch/R-manual/R-devel/library/utils/html/type.convert.html
test<-type.convert(df_thema)


library(ggplot2)


# Fig 1 : Nombre d'IDG impliqué par GT ------------------------------------
ggplot(data = subset(melt_thema, value==1)) +
  aes(fill = value, x = fct_rev(variable)) +
  geom_bar(aes(fill=NIVEAU)) + coord_flip() +
  theme_minimal() + labs(fill = "Echellon des IDG", x="Thématique",y="Nombre d'IDG") + theme(legend.position = c(0.8, 0.2))



# Fig 11 : Graphique ordonnée ---------------------------------------------
# utilisation de la librarie forcats
# PERMET  DE REPRESENTER LES GROUPES DE TRAVAIL LES PLUS DEMANDES EN FR
ggplot(data = subset(melt_thema, value==1)) +
  aes(fill = value, x = fct_rev(fct_infreq(variable))) +
  geom_bar(aes(fill=NIVEAU)) + coord_flip() + theme_minimal() + scale_y_continuous(limits = c(0,15), breaks = c(0,1,2,3,4,5,10,15)) +
   labs(fill = "Echellon des IDG", x="Thématique",y="Nombre d'IDG") + theme(legend.position = c(0.8, 0.2))
  
  # https://stackoverflow.com/questions/45846965/reorder-factors-by-increasing-frequency



# Nombre de GT par IDG ----------------------------------------------------
View(df_thema)
# Heat table --------------------------------------------------------------

summary(df_thema$nb_gt)
hist(df_thema$nb_gt)

parts <- c(80, 30, 20, 10)
waffle(parts, rows=8)
# waffle(df_thema$nb_gt,rows=8)

ggplot(data = df_thema) +geom_bar(aes(df_thema$nb_gt, fill=df_thema$NIVEAU)) + scale_x_continuous(breaks = seq(0,25,1)) +  scale_y_continuous(breaks = seq(0,15,1)) +
  theme_classic2() +  theme(legend.position = c(0.8, 0.8))+
  labs(fill = "Echellon des IDG", x="Nombres de Groupes",y="Nombre d'IDG", caption="Source : Geobs")



# Heatmap montrant le detail des GT par IDG -------------------------------
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# http://www.roymfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r/

# + options(axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))
# https://stackoverflow.com/questions/5713713/order-data-inside-a-geom-tile
# https://www.statmethods.net/management/sorting.html
df_thema_ord <- df_thema[order(-df_thema$nb_gt),]

melt_thema_ord <- melt_thema[order(rep(-df_thema$nb_gt, each = 39)),]

x$V1 <- factor(x$V1, levels=(x$V1)[order(x$V3)])
# melt_thema$IDG <- factor(melt_thema_ord$IDG , levels=(melt_thema_ord$IDG )[order(rep(-df_thema$nb_gt, each = 39))])




outputVec <- rep(df_thema$nb_gt, each = 39)

# il faut repeter l ordre n fois pour le melt
# https://stackoverflow.com/questions/36423766/repeat-factor-elements

length(unique(df_thema$IDG))
length(melt_thema_ord$IDG)
dim(melt_thema_ord)
dim(melt_thema)
length(unique(melt_thema$variable))
1443/37


# Graphique heatmap -------------------------------------------------------
p <- ggplot(subset(melt_thema_ord, variable!='nb_gt'), aes(x=variable, IDG, fill = value )) + geom_tile(aes(fill = value),colour = "gray40") +
  scale_fill_gradient(low = "white",high = "steelblue")

p + theme_grey(base_size = 9) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + theme_minimal() + 
  scale_y_discrete(expand = c(0, 0)) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=8))




# AFC : Analyse -----------------------------------------------------------
library ("FactoMineR")
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/74-afc-analyse-factorielle-des-correspondances-avec-r-l-essentiel/
# https://pbil.univ-lyon1.fr/ade4/ade4-html/dudi.coa.html
# http://edu.mnhn.fr/pluginfile.php/8213/mod_resource/content/0/ED_MNHN_Analyse_des_donnees_3_AFC_R.pdf
melti <- subset(melt_thema_ord, variable!='nb_gt')
# afc <- dudi.coa(df_thema[,3:38], scannf = FALSE, nf = 3)
# You can reproduce this result non-interactively with: 
#   dudi.coa(df = df_thema[, 3:38], scannf = FALSE, nf = 3)
# donne une classification sur les GT pas sur les IDG
cast_thema <- dcast(melti, IDG~variable)
row.names(cast_thema) <- cast_thema$IDG
# IL  FAUT FARIE RENTRER LES NOMS DES IDG EN TANT QUE ROWNAMES ET SUPPRIMER LA COLOMNE POUR EVITER D ETRE ANALYSE DANS L AFC
afc <- dudi.coa(cast_thema[,2:dim(cast_thema)[2]], scannf = FALSE, nf = 2)

afc$tab

afc$co # affiche les coordonnees calcule par l AFC
typeof(afc)

df_coord <- as.data.frame(afc$li)
df_coord$names <- row.names(df_coord)

ggplot(df_coord,aes(x=Axis1, y=Axis2, label=row.names(df_coord))) + geom_point() + geom_text_repel()


afc$li
afc$co
names(melti)
rownames(melti)
View(df_thema[,3:38])
res.ca <- CA(df_thema[,3:38], graph = FALSE)
print(res.ca)



ggplot(data = df_thema) +geom_dotplot(aes(df_thema$nb_gt, fill=df_thema$NIVEAU)) 

length(unique(melt_thema$IDG))
as.data.frame(table(melt_thema$IDG,melt_thema$NIVEAU))

  
# https://stackoverflow.com/questions/16961921/plot-data-in-descending-order-as-appears-in-data-frame
# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph/9231857

# https://community.rstudio.com/t/reverse-order-of-categorical-y-axis-in-ggridges-ggplot2/2273/3


ggplot(data = melt_thema) +
  aes(fill = value, x = variable, order=value) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme_minimal() +   facet_grid(vars(NIVEAU)) +  coord_flip()
# geom_bar(position = position_stack(reverse = FALSE))
# https://stackoverflow.com/questions/19835987/display-frequency-instead-of-count-with-geom-bar-in-ggplot




# faire le oui / non par plateforme
ggplot(data = melt_form_reg) +
  aes(fill = value, x = variable) + geom_tile(aes(fill = value)) +  coord_flip()
