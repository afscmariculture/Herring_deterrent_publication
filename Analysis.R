# Herring analyses_v2
# Jordan A. Hollarsmith
# 8/29/2022
# some code modified from Ky Friedman


setwd("~/GitHub/Herring_deterrent_publication")

library(ggplot2)
library(Rmisc)


### loading data ####


bubbles <- read.csv("Bubble_data.csv")
Pinger <- read.csv("Pinger_data.csv")
Lights <- read.csv("Light_data.csv")
Static <- read.csv("Static_flasher_data.csv")
Moving1 <- read.csv("Moving_flasher_1row_data.csv")
Moving2 <- read.csv("Moving_flasher_2row_data.csv")


### Analyses ####

lm.bubble <- lm(dist_insult ~ Treatment_State + as.numeric(Block), 
                bubbles)

lm.pinger <- lm(dist_insult ~ Treatment_State + Block, 
                Pinger)

lm.light <- lm(dist_insult ~ Treatment_State + Block, 
               Lights)

lm.static <- lm(dist_insult ~ Treatment_State + Block, 
                Static)

lm.moving1 <- lm(dist_insult ~ Treatment_State + Block, 
            Moving1)

lm.moving2 <- lm(dist_insult ~ Treatment_State + Block, 
            Moving2)


#number observations
#summary stats
samples <- summarySE(tracks_for_plots2, measurevar = "dist_insult",
                     groupvars = c("Replicate", "Treatment_Type", "Treatment_State"))
samples_N <- summarySE(samples, measurevar = "N",
                       groupvars = c("Treatment_Type", "Treatment_State"))
colnames(samples_N)[3] <- "N.2"

ggplot(samples_N, aes(Treatment_Type, N, fill = Treatment_State))+
  geom_pointrange(aes(ymin=N-se, ymax=N+se, 
                      shape = Treatment_State, 
                      fill=Treatment_State), 
                  position=position_dodge(0.3),
                  size=1)+
  scale_shape_manual(values = c(21, 21))+
  scale_fill_manual(values = c("#FFFFFF", "#000000"))+
  theme_bw()+
  labs(x = "Deterrent", y = "Observed fish tracks")


