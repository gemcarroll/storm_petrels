require(ggnewscale)
require(ggplot2)

monteiro_tracks <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_clean.csv")
monteiro_hmm <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_hmm.csv")
prediction_grid <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_hmm.csv")

#### load azores map
load('/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/azores_map.RData')
plot(azores.map, add = TRUE)

#### plot all tracks with forging locations
bathy_palette <- c("#487eb0", "#45aaf2")

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2018), aes(lon, lat), fill = "blue", size = 0.7, alpha = 0.6)+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2019), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2020), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  geom_path(data = subset(monteiro_tracks), aes(lon, lat), colour = "grey30", size = 1)+
  geom_point(data = subset(monteiro, pres == 1 & state ==1), aes(x, y), fill = "white", size = 2,  shape = 21, colour = "#fbc531")+
  #geom_density_2d(data = subset(monteiro, state == 1 & pres == 1), aes(lon, lat), colour = "black", size = 1, alpha = 0.7)+
  #geom_point(data = subset(seamounts_pt), aes(lon, lat), colour = "red", size = 3)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-42,-16), ylim = c(31, 50), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  guides(fill=FALSE)+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/all_tracks.jpg", width = 10, height = 8)


#### incubation and chick-rearing kernels 
ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation"), aes(x, y), colour = "grey20", size = 0.5, fill = "white", geom="polygon", alpha = 0.6) +
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "chick"), aes(x, y, fill = stat(level)), size = 0.5, colour = "grey30", fill =  "#fbc531", geom="polygon", alpha = 0.6) +
  xlab("Longitude")+ylab("Latitude")+
  geom_sf(data = azores.map, fill = 'grey80')+
  guides(fill=FALSE)+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-42,-16), ylim = c(31, 50), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme_classic()+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))


ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/kud_chick_incub.jpg", width = 10, height = 8)

#### incubation and chick-rearing kernels 
ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation"), aes(x, y), colour = "grey20", size = 0.5, fill = "white", geom="polygon", alpha = 0.6) +
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "chick"), aes(x, y, fill = stat(level)), size = 0.5, colour = "grey30", fill =  "#fbc531", geom="polygon", alpha = 0.6) +
  xlab("Longitude")+ylab("Latitude")+
  geom_sf(data = azores.map, fill = 'grey80')+
  guides(fill=FALSE)+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-42,-16), ylim = c(31, 50), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme_classic()+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))


ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/kud_chick_incub.jpg", width = 10, height = 8)



ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "chick" & year == 2020), aes(x, y, fill = stat(level)), colour = "grey30", geom="polygon") +
  scale_fill_gradient(low = '#6ab04c', high = "#badc58") +
  guides(fill=FALSE)+
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "chick" & year == 2019), aes(x, y, fill = stat(level)), colour = "grey30", geom="polygon") +
  scale_fill_gradient(low = '#e55039', high = "#ff5e57") +
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "chick" & year == 2018), aes(x, y, fill = stat(level)), colour = "grey30", geom="polygon") +
  scale_fill_gradient(low = '#ffa801', high = "#ffd32a") +
  guides(fill=FALSE)+
  xlab("Longitude")+ylab("Latitude")+
  geom_sf(data = azores.map, fill = 'grey80')+
  guides(fill=FALSE)+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-42,-16), ylim = c(31, 50), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme_classic()+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/kud_chick.jpg", width = 10, height = 8)

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation" & year == 2020), aes(x, y, fill = stat(level)), colour = "grey30", geom="polygon") +
  scale_fill_gradient(low = '#6ab04c', high = "#badc58") +
  guides(fill=FALSE)+
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation" & year == 2019), aes(x, y, fill = stat(level)), colour = "grey30", geom="polygon") +
  scale_fill_gradient(low = '#e55039', high = "#ff5e57") +
  new_scale_fill()+
  stat_density2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation" & year == 2018), aes(x, y, fill = stat(level)), colour = "grey30", geom="polygon") +
  scale_fill_gradient(low = '#ffa801', high = "#ffd32a") +
  guides(fill=FALSE)+
  xlab("Longitude")+ylab("Latitude")+
  geom_sf(data = azores.map, fill = 'grey80')+
  guides(fill=FALSE)+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-42,-16), ylim = c(31, 50), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme_classic()+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/kud_incub.jpg", width = 10, height = 8)


#### male and female kernels 
ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  new_scale_fill()+
  stat_density2d(data = subset(monteiro_hmm, state == 1 & sex == "M"), aes(x, y, fill = stat(level)), size = 0.5, colour = "grey30", fill =  "#cd84f1", geom="polygon", alpha = 0.6) +
  stat_density2d(data = subset(monteiro_hmm, state == 1 & sex == "F"), aes(x, y), colour = "grey20", size = 0.5, fill = "#33d9b2", geom="polygon", alpha = 0.6) +
  xlab("Longitude")+ylab("Latitude")+
  geom_sf(data = azores.map, fill = 'grey80')+
  guides(fill=FALSE)+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-42,-16), ylim = c(31, 50), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme_classic()+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))


ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/kud_m_f.jpg", width = 10, height = 8)

#### incubation across years
ggplot()+
  theme_void()+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2018), aes(lon, lat), fill = "blue", size = 0.7, alpha = 0.6)+
  geom_density_2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation" & year == 2018), aes(lon, lat), colour = "red", size = 0.7, alpha = 0.7)+
  geom_density_2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation" & year == 2019), aes(lon, lat), colour = "blue", size = 0.7, alpha = 0.3)+
  geom_density_2d(data = subset(monteiro, state == 1 & pres == 1 & bstage == "incubation" & year == 2020), aes(lon, lat), colour = "green", size = 0.7, alpha = 0.3)+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2019), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2020), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  #geom_point(data = subset(monteiro, state == 1 & pres == 1 & year == 2019), aes(lon, lat), size = 0.7, alpha = 0.6)+
  geom_point(data = subset(seamounts_pt), aes(lon, lat), colour = "red", size = 3)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

ggplot()+
  theme_void()+
  geom_tile(data = bathy_df, aes(x=x, y=y, fill=layer)) +
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2018), aes(lon, lat), fill = "blue", size = 0.7, alpha = 0.6)+
  geom_density_2d(data = subset(monteiro, state == 1 & pres == 1), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2019), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  #geom_density_2d_filled(data = subset(monteiro, state == 1 & pres == 1 & year == 2020), aes(lon, lat), colour = "white", size = 0.7, alpha = 0.6)+
  #geom_point(data = subset(monteiro, state == 1 & pres == 1 & year == 2019), aes(lon, lat), size = 0.7, alpha = 0.6)+
  geom_point(data = subset(seamounts_pt), aes(lon, lat), colour = "red", size = 3)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

+
  facet_wrap(~year)
