require(ggplot2)

#### read in monteiro tracks
monteiro_enviro <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro2.csv")

#### subset birds where dates were wrong format and therefore got the wrong environmental factors
monteiro <- subset(monteiro_enviro, ring != 'D32920' & ring != 'D47069')

###### plot habitat selection relative to the pseudoabsence
##### depth

palette <- c("grey80", "#9c88ff")

ggplot()+
  geom_density(data = monteiro, aes(depth, fill = as.factor(pres)), alpha = 0.8)+
  scale_fill_manual(name = "Presence", values = palette)+
  xlab("Depth (m)")+ylab("Density")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/niche_depth.jpg", width = 10, height = 8)


##### Seamount distance

ggplot()+
  geom_density(data = monteiro, aes(min_seamount_dist, fill = as.factor(pres)), alpha = 0.8)+
  scale_fill_manual(name = "Presence", values = palette)+
  xlab("Min. seamount distance (km)")+ylab("Density")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/niche_seamount.jpg", width = 10, height = 8)

##### SST

ggplot()+
  geom_density(data = monteiro, aes(SST, fill = as.factor(pres)), alpha = 0.8)+
  scale_fill_manual(name = "Presence", values = palette)+
  xlab("SST")+ylab("Density")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/niche_sst.jpg", width = 10, height = 8)

###### chlorophyll a
ggplot()+
  geom_density(data = monteiro, aes(log(Chla), fill = as.factor(pres)), alpha = 0.8)+
  scale_fill_manual(name = "Presence", values = palette)+
  xlab("Chl a")+ylab("Density")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/niche_chla.jpg", width = 10, height = 8)

