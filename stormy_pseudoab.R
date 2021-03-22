require(ggplot2)
require(sf)

#### read monteiro file 
monteiro <- read.csv(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_hmm.csv")

#### load azores map
load('/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/azores_map.RData')

### load prediction grid
prediction_grid <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv")

#### select only foraging locations 
monteiro_forage <- monteiro[monteiro$state == 1,]

#### add indication that these are presences
monteiro_forage$pres <- 1

##### find maximum foraging distance
max(monteiro$dist_colony)

#### create buffer around Praia Islet corresponding to max foraging distance 
praia <- data.frame(lon = -27.955, lat = 39.056667)
praia = st_as_sf(praia, coords = c("lon", "lat"))
st_crs(praia) <- 4326
praia2 = st_transform(praia, crs = 2188)
praia_buffer <- st_buffer(praia2, dist = 963963)
praia_buffer2 <- st_transform(praia_buffer, crs = 4326)

save(praia_buffer2, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/praia_buffer2.RDS")
st_write(praia_buffer2, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/praia_buffer2.shp")

#### generate pseudoabsences from within the buffer, 10x the number of presences
abs <- st_sample(praia_buffer2, size = nrow(monteiro_forage)*10, type = 'random')
#abs_pts = st_transform(abs,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
abs_pts <- data.frame(st_coordinates(abs))
names(abs_pts) <- c("x", "y")

###### create absence dataframe
monteiro_abs <- monteiro_forage[rep(seq_len(nrow(monteiro_forage)), each = 10),]

monteiro_abs <- subset(monteiro_abs, select = -c(x,y))
monteiro_abs <- cbind(monteiro_abs, abs_pts)
monteiro_abs$pres <- 0

#### final data set with presences and pseudoabsences
monteiro_forage <- rbind(monteiro_forage, monteiro_abs)

#### plot all tracks with forging locations
bathy_palette <- c("#487eb0", "#45aaf2")

##### plot presences and pseudoabsences with buffer 
ggplot()+
  theme_void()+
  geom_tile(data = prediction_grid, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  geom_sf(data = praia_buffer2)+
  #geom_point(data = subset(monteiro_forage, pres == 0), aes(x, y), colour = "grey70", size = 0.7, alpha = 0.6)+
  geom_point(data = subset(monteiro_forage, pres == 0 & state ==1), aes(x, y), fill = "grey70", size = 2,  shape = 21, colour = "black")+
  geom_point(data = subset(monteiro_forage, pres == 1 & state ==1), aes(x, y), fill = "white", size = 2,  shape = 21, colour = "#fbc531")+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  guides(fill=FALSE)+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.text=element_blank(), legend.title=element_blank(), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/monteiro_pseudoabs.jpg", width = 10, height = 8)

write.csv(monteiro_forage, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_pseudoabs.csv", row.names = F)

