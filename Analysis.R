# Herring analyses_v2
# Jordan A. Hollarsmith
# 8/29/2022
# some code modified from Ky Friedman


setwd("~/Herring/HerringProject/Results_V2/Flexview")

library(dplyr)
library(ggplot2)
library(Rmisc)
library(stringr)
library(chron)

### loading data ####
#bubbles block 1 rest messed up
#static block 5 rest messed up

#9/9/22: bubbles block 1 rest still messed up; static fixed

#partitioned data
time.data <- read.csv("Coalesced_Data\\All_Partitions.csv")
time.data$block <- as.character(time.data$block)
time.data.trim <- subset(time.data, partition_num>3 & partition_num<27)

#all tracks
tracks <- read.csv("Coalesced_Data\\All_Tracks.csv")
tracks$Block <- as.character(tracks$Block)
tracks.trim <- subset(tracks, partition_num>3 & partition_num<27)
tracks.trim$Treatment_Type <- factor(tracks.trim$Treatment_Type, 
                                     levels = c("Bubbles", "Pinger", "Lights", "Static", 
                                                "SingleDynamic", "DoubleDynamic"))

### New flasher data ####
setwd("C:\\Users\\Jordan.Hollarsmith\\Work\\Herring\\Flasher2")
#Block4_DD_On_far <- read.csv("Block4_DoubleDynamic(on)_FarFishTracks_RangeBeyondFarLine.csv")
#Block4_DD_On_near <- read.csv("Block4_DoubleDynamic(on)_NearFishTracks_RangeToNearLine.csv", sep="\t")
Block4_DD_On_far <- read.csv("Block4_DoubleDynamic(on)_FarFishTracks_RangeBeyondFarLine.csv")
Block4_DD_On_near <- read.csv("Block4_DoubleDynamic(on)_NearFishTracks_RangeToNearLine.csv", sep="\t")

Block4_SD_On_far <- read.csv("Block4_SingleDynamic(on)_FarFishTracks_RangeBeyondFarLine.csv")
Block4_SD_On_near <- read.csv("Block4_SingleDynamic(on)_NearFishTracks_RangeToNearLine.csv")

Block5_DD_On_far <- read.csv("Block5_DoubleDynamic(on)_FarFishTracks_RangeBeyondFarLine.csv")
Block5_DD_On_near <- read.csv("Block5_DoubleDynamic(on)_NearFishTracks_RangeToNearLine.csv", sep=",")

Block5_SD_On_far <- read.csv("Block5_SingleDynamic(on)_FarFishTracks_RangeBeyondFarLine.csv")
Block5_SD_On_near <- read.csv("Block5_SingleDynamic(on)_NearFishTracks_RangeToNearLine.csv", sep=",")


#combine datasets
Block4_DD_On <- bind_rows(Block4_DD_On_far, Block4_DD_On_near)
Block4_SD_On <- bind_rows(Block4_SD_On_far, Block4_SD_On_near)
Block5_DD_On <- bind_rows(Block5_DD_On_far, Block5_DD_On_near)
Block5_SD_On <- bind_rows(Block5_SD_On_far, Block5_SD_On_near)

#add replicate and treatment columns
Block4_DD_On$Replicate = 1
Block4_SD_On$Replicate = 1
Block5_DD_On$Replicate = 2
Block5_SD_On$Replicate = 2

Block4_DD_On$Treatment = "DoubleDynamic"
Block4_SD_On$Treatment = "SingleDynamic"
Block5_DD_On$Treatment = "DoubleDynamic"
Block5_SD_On$Treatment = "SingleDynamic"

#write.csv(Block4_DD_On, "Block4_DD_On.csv")
#write.csv(Block4_SD_On, "Block4_SD_On.csv")
#write.csv(Block5_DD_On, "Block5_DD_On.csv")
#write.csv(Block5_SD_On, "Block5_SD_On.csv")

# trying again to do time
library(lubridate)
Block4_DD_On$hms <- hms(Block4_DD_On$Time_M)
Block4_DD_On$minTime <- hour(Block4_DD_On$hms)*60 + minute(Block4_DD_On$hms)
Block4_DD_On$partition_num <- Block4_DD_On$minTime - 955
#trim first and last 3 min
Block4_DD_On_trim <- subset(Block4_DD_On, partition_num > 3 & partition_num < 21)

Block4_SD_On$hms <- hms(Block4_SD_On$Time_M)
Block4_SD_On$minTime <- hour(Block4_SD_On$hms)*60 + minute(Block4_SD_On$hms)
Block4_SD_On$partition_num <- Block4_SD_On$minTime - 890
Block4_SD_On_trim <- subset(Block4_SD_On, partition_num > 3 & partition_num < 18)

Block5_DD_On$hms <- hms(Block5_DD_On$Time_M)
Block5_DD_On$minTime <- hour(Block5_DD_On$hms)*60 + minute(Block5_DD_On$hms)
Block5_DD_On$partition_num <- Block5_DD_On$minTime - 1176 
Block5_DD_On_trim <- subset(Block5_DD_On, partition_num > 3 & partition_num < 21)


Block5_SD_On$hms <- hms(Block5_SD_On$Time_M)
Block5_SD_On$minTime <- hour(Block5_SD_On$hms)*60 + minute(Block5_SD_On$hms)
Block5_SD_On$partition_num <- Block5_SD_On$minTime - 1117 
Block5_SD_On_trim <- subset(Block5_SD_On, partition_num > 3 & partition_num < 20)


#combine all
flasher_on <- bind_rows(Block4_DD_On_trim, Block4_SD_On_trim, Block5_DD_On, Block5_SD_On_trim) #no 5_DD_On_Trim yet
flasher_on$Replicate = as.factor(flasher_on$Replicate)
#write.csv(flasher, "flashers.csv")

#combine with old data
flashers <- subset(tracks, Treatment=="DoubleDynamic(Off)"|
                     Treatment=="SingleDynamic(Off)"|
                     Treatment_Type=="Static")
flashers$Replicate <- ifelse(flashers$Block==4, 1, 2)
#I only need treatment_type, treatment_state, replicate, distance to deterrent, partition num
flashers2 <- flashers[,c(5,6,9,15,16)]
flasher_on2 <- flasher_on[,c(23,44,45,48)]
flasher_on2$Treatment_State <- "On"
names(flasher_on2)[names(flasher_on2)=='Distance_To_Insult'] <- 'dist_insult'
names(flasher_on2)[names(flasher_on2)=='Treatment'] <- 'Treatment_Type'
flashers2$Replicate <- as.factor(flashers2$Replicate)

flashers_all <- bind_rows(flasher_on2, flashers2)
flashers_all$Replicate <- as.factor(flashers_all$Replicate)

# combine 'flashers_all' with other treatments
# change block to replicate

non.flashers <- subset(tracks, Treatment_Type=="Bubbles"|
                         Treatment_Type=="Lights"|
                         Treatment_Type=="Pinger")

names(non.flashers)[names(non.flashers)=='Block'] <- 'Replicate'
non.flashers2 <- non.flashers[,c(3,5,6,9,15)]
tracks_for_plots <- bind_rows(non.flashers2, flashers_all)

#rename flasher data
tracks_for_plots2 <- tracks_for_plots %>%
  mutate(Treatment_Type = recode(Treatment_Type, 
                                 "DoubleDynamic" = "Flashers-2Rows-Moving",
                                 "SingleDynamic" = "Flashers-1Row-Moving",
                                 "Static" = "Flashers-1Row-Static",
                                 "Pinger" = "Sound",
                                 "Lights" = "Light"))
#reorder
tracks_for_plots2$Treatment_Type <- factor(tracks_for_plots2$Treatment_Type, 
                                           levels = c("Bubbles", "Sound", "Light", "Flashers-1Row-Static", 
                                                      "Flashers-1Row-Moving", "Flashers-2Rows-Moving"))

# remake partition file: average by partition number, replicate, treatment_type, and treatment_state
time_for_plots <- 
  tracks_for_plots2 %>%
  group_by(partition_num, Treatment_Type, Treatment_State, Replicate) %>%
  summarise_at(vars("dist_insult"), mean)



### Analyses ####
#SHOULD THESE ALL BE BINOMIAL DISTRIBUTIONS? No, because y is continuous

lm.bubble <- lm(dist_insult ~ Treatment_State + as.numeric(Block), 
                subset(tracks.trim, Treatment_Type=="Bubbles"))
#treatment=0.95
#should I only analyze half the pen for bubbles?
lm.bubble.2 <- lm(dist_insult ~ Treatment_State + Block, 
                  subset(tracks.trim, Treatment_Type=="Bubbles" & avg_range<4))
#treatment=0.8; block=~0.2
#I don't think limiting avg_range like this halves the pen
lm.pinger <- lm(dist_insult ~ Treatment_State + Block, 
                subset(tracks.trim, Treatment_Type=="Pinger"))
#treatment=-0.1; block=-1.5
lm.light <- lm(dist_insult ~ Treatment_State + Block, 
               subset(tracks.trim, Treatment_Type=="Lights"))
#treatment=0.13; block=~-1.4

####
#SEE NEW CODE BELOW
lm.static <- lm(dist_insult ~ Treatment_State + Block, 
                subset(tracks.trim, Treatment_Type=="Static"))
#treatment=0.1; block=0.3
#r2=0.03 **very low**

lm.SD <- lm(dist_insult ~ Treatment_State + Block, 
            subset(tracks.trim, Treatment_Type=="SingleDynamic"))
#treatment=-0.04; block=0.2
#r2=0.02 **even lower***
lm.DD <- lm(dist_insult ~ Treatment_State + Block, 
            subset(tracks.trim, Treatment_Type=="DoubleDynamic"))
#treatment=0.04; block=-0.3
#r2=0.04 **still low***

#summary stats
samples <- summarySE(tracks.trim, measurevar = "dist_insult",
                     groupvars = c("Block", "Treatment_Type", "Treatment_State"))
samples_N <- summarySE(samples, measurevar = "N",
                       groupvars = c("Treatment_Type", "Treatment_State"))
colnames(samples_N)[3] <- "N.2"
samples_N$Treatment_Type <- factor(samples_N$Treatment_Type, 
                                   levels = c("Bubbles", "Pinger", "Lights", "Static", 
                                              "SingleDynamic", "DoubleDynamic"))

sample.graph <- ggplot(samples_N, aes(Treatment_Type, N, fill = Treatment_State))+
  geom_pointrange(aes(ymin=N-se, ymax=N+se, 
                      shape = Treatment_State, 
                      fill=Treatment_State), 
                  position=position_dodge(0.3),
                  size=1)+
  scale_shape_manual(values = c(21, 21))+
  scale_fill_manual(values = c("#FFFFFF", "#000000"))+
  theme_bw()+
  labs(x = "Deterrent", y = "Observed fish tracks")




#====analyses
lm.static <- lm(dist_insult ~ Treatment_State + Replicate, 
                subset(flashers_all, Treatment_Type=="Static"))
#state= 0.08; rep=0.3
#r2=0.03....soooo low
ml.static <- gls(dist_insult ~ Treatment_State + Replicate, 
                 data=subset(flashers_all, Treatment_Type=="Static"),
                 method="ML")
#same results as lm

lm.SD <- lm(dist_insult ~ Treatment_State + Replicate, 
            subset(flashers_all, Treatment_Type=="SingleDynamic"))
#state= -0.2; rep=0.08

lm.DD <- lm(dist_insult ~ Treatment_State + Replicate, 
            subset(flashers_all, Treatment_Type=="DoubleDynamic"))
#state= 0.05; rep= -0.2

### Plots ####
time.plot
box.plot.all
box.plot.simple
hist.plot.all
violin.plot
sample.graph

#time series
time.plot <- time.data.trim %>% 
  group_by(Replicate) %>% 
  ggplot(aes(x = partition_num, y = dist_to_insult, color = block)) +
  geom_path(aes(color=block, linetype=treatmentState), size=1) +
  scale_x_continuous(breaks = seq(0, 35, by=1), limits=c(0, 35)) +
  scale_y_continuous(breaks = seq(0, 4, by=2), limits=c(0,4)) + 
  labs(x = "Time (min)", y = "Distance from Deterrent (m)") + 
  facet_wrap(~treatmentType, ncol = 3) +
  theme_bw()

time.trends <- time.data.trim %>% 
  group_by(block) %>% 
  ggplot(aes(x = partition_num, y = dist_to_insult, color = block)) +
  geom_point(aes(color=block), size=1) +
  geom_smooth(method=lm, aes(linetype=treatmentState)) +
  scale_x_continuous(breaks = seq(0, 35, by=1), limits=c(0, 35)) +
  scale_y_continuous(breaks = seq(0, 4, by=2), limits=c(0,4)) + 
  labs(x = "Time (min)", y = "Distance from Deterrent (m)") + 
  facet_wrap(~treatmentType, ncol = 3) +
  theme_bw()

#tracks
box.plot.all <- tracks.trim %>% 
  group_by(Block) %>% 
  ggplot(aes(Block, dist_insult, fill = Treatment_State)) +
  theme(panel.background=element_rect(fill="white", color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid = element_line(color="#D6D6D6"))+
  geom_boxplot() + facet_wrap(~Treatment_Type, ncol = 2) + 
  labs(x = "Block (replicate)", y = "Distance from Insult (m)")

box.plot.simple <- tracks.trim %>% 
  group_by(Block) %>% 
  ggplot(aes(Treatment_State, dist_insult, fill = Treatment_State)) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid = element_line(color="#D6D6D6"))+
  geom_boxplot() + facet_wrap(~Treatment_Type, ncol = 2) + 
  labs(x = "Treatment State", y = "Distance from Insult (m)")

#histogram plots of raw location data
hist.plot.all <- tracks.trim %>% 
  group_by(Block) %>% 
  ggplot(aes(avg_range, fill = Treatment_State)) +
  geom_histogram() + 
  facet_wrap(~Block + Treatment_Type, ncol = 2) + 
  labs(x = "Distance from sonar", y = "Number of observations")+
  theme_bw() 

#violin plot
violin.plot <- tracks.trim %>% 
  group_by(Block) %>% 
  ggplot(aes(Block, dist_insult, fill = Treatment_State)) +
  theme(panel.background=element_rect(fill="white", color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid = element_line(color="#D6D6D6"))+
  geom_violin() + facet_wrap(~Treatment_Type, ncol = 2) + 
  labs(x = "Block (replicate)", y = "Distance from deterrent (m)")

violin.plot.raw <- tracks.trim %>% 
  group_by(Block) %>% 
  ggplot(aes(Block, avg_range, fill = Treatment_State)) +
  theme(panel.background=element_rect(fill="white", color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid = element_line(color="#D6D6D6"))+
  geom_violin() + facet_wrap(~Treatment_Type, ncol = 2) + 
  labs(x = "Block (replicate)", y = "Distance from sonar (m)")

violin.plot.comb <- 
  ggplot(tracks.trim, aes(Treatment_Type, dist_insult, fill=Treatment_State)) +
  theme(panel.background=element_rect(fill="white", color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid = element_line(color="#D6D6D6"))+
  geom_violin() + 
  labs(x = "Deterrent", y = "Distance from deterrent (m)")
#so ugly

#===visualize updated flasher data
ggplot(Block4_DD_On, aes(Distance_To_Insult))+
  geom_histogram() +
  theme_bw()

ggplot(Block4_DD_On, aes(Distance_To_Insult, fill=Region_class))+
  geom_histogram() +
  theme_bw()
#the near fish are being weird


ggplot(flashers_all, aes(Treatment_State, dist_insult, fill=Replicate))+
  geom_violin()+
  facet_wrap(~Treatment_Type)

ggplot(flashers_all, aes(Replicate, dist_insult, fill = Treatment_State)) +
  theme(panel.background=element_rect(fill="white", color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid = element_line(color="#D6D6D6"))+
  geom_violin() + facet_wrap(~Treatment_Type, ncol = 2) + 
  labs(x = "Replicate", y = "Distance from deterrent (m)")

### Manuscript figures ####

#all one
ggplot(tracks_for_plots2, aes(Treatment_Type, dist_insult, fill = Treatment_State)) +
  geom_violin() +  
  geom_boxplot(width=0.1, position=position_dodge(0.9), color="black")+
  labs(x = "Deterrent", y = "Distance from deterrent (m)")+
  theme_bw()+
  scale_fill_grey()

#over time 
time_for_plots %>% 
  group_by(Replicate) %>% 
  ggplot(aes(x = partition_num, y = dist_insult, color = Replicate)) +
  geom_point(aes(color=Replicate), size=1) +
  geom_smooth(method=lm, aes(linetype=Treatment_State)) +
  scale_x_continuous(breaks = seq(0, 35, by=1), limits=c(0, 35)) +
  scale_y_continuous(breaks = seq(0, 4, by=2), limits=c(0,4)) + 
  labs(x = "Time (min)", y = "Distance from Deterrent (m)") + 
  facet_wrap(~Treatment_Type, ncol = 3) +
  theme_bw()

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


### Flashers - old code ####


#From Ky:
## This function returns the "partition" that a given track appears in within
## the overall trial. The number of partitions is determined by the inputted
## spacing defined globally in this document.
## If, for example spacing = 5, then if a track appears between minutes
## 0 and 5, it would be in partition "one". If it appears between minutes
## 20 and 25, it would be in partition "five". From this, we can then construct
## a picture of the average location of the fish throughout the trial.
Block4_DD_On$Chron_time = chron(times=Block4_DD_On$Time_M)

getPartition <- function(minTime, time, spacing){
  ## Converts inputs to purely seconds
  minTime = 3600*hours(minTime) + 60*minutes(minTime) + seconds(minTime);
  time = 3600*hours(time) + 60*minutes(time) + seconds(time);
  
  minuteMark = (time - minTime)/60;
  return( floor(minuteMark/spacing) )
}

minTime = min(Block4_DD_On$Chron_time)
median_time = median(Block4_DD_On$Chron_time)
PARTITION_SIZE = 1
Block4_DD_On$Partition <- getPartition(minTime, median_time, PARTITION_SIZE) #NOT QUITE RIGHT


## This function takes in a ping_time (HH:MM:SS.SSSS) and returns a double 
## representation of the second time stamp of the given ping. This allows for 
## subsequent calculation of a given track's temporal length with +/- 0.001s.
getSecondsFromPing <- function(ping){
  seconds <- NULL
  for(str in ping){
    sec <- strsplit(str, ":")[[1]][3]
    seconds <- rbind(seconds, sec)
  }
  return(seconds)
}
Block4_DD_On$Seconds <- getSecondsFromPing(Block4_DD_On$Time_M)


setwd("C:\\Users\\Jordan.Hollarsmith\\Work\\Herring\\Flashers")

Block4_On_near <- read.csv("Block4_DoubleDynamic(On)_near_fishtracks.csv")
# Target_range_mean: position of each fish track
# Region_bottom_altitude_mean: distance from the edge of the flasher treatment
# Fish_track_change_in_range: behavior, moving towards or away from teh treatment (neg=away from flashers)
summary(Block4_On_near$Region_bottom_altitude_mean)
#some crazy outliers, basically zero
Block4_On_near2 <- subset(Block4_On_near, Region_bottom_altitude_mean>0.00000001)
summary(Block4_On_near2$Region_bottom_altitude_mean)

Block4_On_far <- read.csv("Block4_DoubleDynamic(On)_far_fishtracks.csv")
# Target_range_mean: position of each fish track
Far.depth <- read.csv("Far.depth.csv", sep="\t")
# in order to determine the proximity to the flashers you will need to import the line file I also provided (Far.depth) 
# which represents the other side of the treatment interval
# pair up the line position (using time stamp) to the Far fish tracks (using time stamp) 
# and then subtract the Target_range_mean from the line Depth.


#THIS DOESN"T WORK:
# make time stamp comparable
Far.depth$Time_M1 <- paste(Far.depth$Depth_time, Far.depth$Depth_milliseconds, sep=".")
Far.depth$Time_M <- str_pad(Far.depth$Time_M1, width=13, side="right", pad="0")

# combine datasets
Block4_On_far_full <- full_join(Block4_On_far, Far.depth, by="Time_M")
