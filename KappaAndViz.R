rm(list=ls())
# install.packages("tidyverse")
# install.packages("irr")
# install.packages ("gridExtra")
# install.packages ("RColorBrewer")
# install.packages ("ggrepel")
library (tidyverse)
library (gridExtra)
library(irr)
library(RcolorBrewer)
library(ggrepel)


setwd("~/Documents/Projets R/Caillois Box")


## 1. Visualiser les distributions sur chaque axe
d <- read_csv("A15P16P17P18.ALL.csv")

g1 <- ggplot (data = d, aes (x = Mimilinx))+
  geom_bar() +
  xlab ("Ilinx <-> Mimicry") +
  ggtitle(paste("Scores Ilinx - Mimicry (",nrow(d)," codages)", sep=""))

g2 <- ggplot (data = d, aes (x = Agal))+
  geom_bar() +
  xlab ("Alea <-> Agôn") +
  ggtitle(paste("Scores Agôn - Alea (",nrow(d)," codages)", sep=""))

g3 <- ggplot (data = d, aes (x = Lupai))+
  geom_bar() + 
  xlab ("Paidia <-> Ludus") +
  ggtitle(paste("Scores Paidia - Ludus (",nrow(d)," codages)", sep=""))

grid.arrange(g1, g2, g3, nrow=3)

## 2. Kappa.Fleiss
d <- read.csv2("A15P16P17P18.ALL.csv", sep=",")

table (d$Groupe) # On ne retient que les groupes qui ont codé le plus de jeux en commun

GroupeALL <- data.frame (Jeux = NA, Score = NA)
for (i in c(1,5,8,9,13,16,18,24,25,27)+1){ #Max Raters 114/3=38 jeux / 10 raters : 0.828
  subgroup <- subset (d,d$Groupe==levels (d$Groupe)[i]) # filter (Groupe == "Barbier)
  JeuxMimil <- paste (subgroup$Jeux,"Mimil", sep="") # prépare un mutate avec colonne JeuxMimil
  JeuxAgal <- paste (subgroup$Jeux,"Agal", sep="")
  JeuxLupai <- paste (subgroup$Jeux,"Lupai", sep="")
  ListeJeux <-  c (JeuxMimil,JeuxAgal,JeuxLupai)
  Groupe <- data.frame (Jeux = ListeJeux, Score = c(subgroup$Mimilinx,subgroup$Agal,subgroup$Lupai))
  GroupeALL <- merge (GroupeALL,Groupe, by ="Jeux", all=TRUE)
}
dataKappa <- GroupeALL [1:nrow(GroupeALL)-1,c(3:ncol(GroupeALL))]

icc(dataKappa, model="twoway", type="agreement", unit="single")


## 3.1 Plot graphe à 3 variables : jeux et jeu vidéo
d <- read_csv("A15P16P17P18.ALL.csv")
d <- filter (d, Groupe != "ADDON")

d.agreg <- d %>% # moyennes par jeu
  group_by(Jeux, JeuVideo) %>%
  summarise (
    Mimilinx = mean (Mimilinx),
    Agal = mean (Agal),
    Lupai = mean (Lupai)
  )

ggplot (d.agreg, aes (Mimilinx, Agal, Jeux)) + 
  geom_point (aes(x = Mimilinx, y = Agal, 
                  shape = JeuVideo, color = Lupai, size = 6), alpha = 0.75) +
  scale_shape_manual(values = c(21,16), name = "Jeu vidéo", labels = c("Faux","Vrai")) +
  scale_color_gradient(low = "red", high = "blue", name = "Ludus - \nPaidia") +
  geom_label_repel(aes(Mimilinx, Agal, label = Jeux),
    box.padding = 0.35, point.padding = 0.8, max.iter = 2000,
    alpha = 0.75,
    segment.color = 'grey50') +
  guides (size = "none") +
  xlab ("Ilinx <-> Mimicry") +
  ylab ("Alea <-> Agôn") +
  scale_x_continuous (breaks = c (-5:5), limits = c(-5.5,5.5)) +
  scale_y_continuous (breaks = c (-5:5), limits = c(-5.5,5.5)) +
  ggtitle ("Classification selon les catégories de Caillois + jeux vidéo") +
  labs(caption = "(moyenne des notes issues des groupes de codage)")

ggsave ("CailloisBox.png", width = 10, height = 8)


## 3.2 Graphe jeux traditionnels
d <- read_csv("A15P16P17P18.ALL.csv")
d <- d %>%
  filter (Groupe != "ADDON") %>%
  filter (JeuVideo == FALSE)

d.agreg <- d %>% # moyennes par jeu
  group_by(Jeux) %>%
  summarise (
    Mimilinx = mean (Mimilinx),
    Agal = mean (Agal),
    Lupai = mean (Lupai)
  )

ggplot (d.agreg, aes (Mimilinx, Agal, Jeux)) + 
  geom_point (aes(x = Mimilinx, y = Agal, 
                  color = Lupai, size = 6), alpha = 0.75) +
  scale_color_gradient(low = "red", high = "blue", name = "Ludus - \nPaidia") +
  geom_label_repel(aes(Mimilinx, Agal, label = Jeux),
                   box.padding = 0.35, point.padding = 0.8, max.iter = 2000,
                   alpha = 0.75,
                   segment.color = 'grey50') +
  guides (size = "none") +
  xlab ("Ilinx <-> Mimicry") +
  ylab ("Alea <-> Agôn") +
  scale_x_continuous (breaks = c (-5:5), limits = c(-5.5,5.5)) +
  scale_y_continuous (breaks = c (-5:5), limits = c(-5.5,5.5)) +
  labs(caption = "(moyenne des notes issues des groupes de codage)") +
  ggtitle ("Classification des jeux selon les catégories de Caillois")

ggsave ("CailloisTrad.png", width = 10, height = 8)
