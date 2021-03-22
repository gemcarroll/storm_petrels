require(ggplot2)
require(dplyr)
require(raster)
require(sf)
require(stringr)
require(ncdf4)

#### create a spatial and temporal grid for predictions/mapping 

#### use the extent of the buffer the size of the largest foraging distance (madeiran) to clip the area
extent <- as(extent(-46.06-0.1, -9.81+0.1, 25.04-0.1, 52.79+0.1), "SpatialPolygons")
proj4string(extent) <- "+proj=longlat +ellps=WGS84"

### create a regular grid for spatial analyses - high resolution 
grid_highres <- spsample(extent, cellsize = c(0.1, 0.1), type = "regular")
gridded(grid_highres) = TRUE
plot(grid_highres)

#### load azores map
load('/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/azores_map.RData')
plot(azores.map, add = TRUE)

grid.df_highres <- data.frame(grid_highres)
names(grid.df_highres) <- c("lon", "lat")

#### create a low resolution grid for some variables 
### create a regular grid for spatial analyses - low resolution 
grid_lowres <- spsample(extent, cellsize = c(1, 1), type = "regular")
gridded(grid_lowres) = TRUE
plot(grid_lowres)
plot(azores.map, add = TRUE)

grid.df_lowres <- data.frame(grid_lowres)
names(grid.df_lowres) <- c("lon", "lat")

##### extract bathymetry to grid
#----Bathymetry----
bathy <- raster(x = "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Bathymetry/ETOPO1_Bed_g_gdal.grd")

latlon <- data.frame(cbind(grid.df_highres$lon, grid.df_highres$lat))
rasValue <- raster::extract(bathy, latlon)
grid.df_highres$depth <- rasValue
grid.df_highres$depth[grid.df_highres$depth >-1] <- NA

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)

#### read in monteiro tracks
monteiro <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_forage.csv")

#### work out when most of the tracks are collected in each season
monteiro$month <- str_sub(monteiro$date, 6,7)
monteiro$day <- str_sub(monteiro$date, 9,10)

sample_monteiro <- monteiro %>% group_by(year, month, bstage) %>% summarise(n = length(unique(id)))

#### read in seamount locations
seamounts <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Bathymetry/seamounts.csv")

bathy_palette <- c("#273c75", "#2c7fb8", "#7fcdbb", "#edf8b1")

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = depth)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Depth (m)")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(data = subset(seamounts), aes(lon, lat), shape = 21, colour = "black", fill = "#fbc531", size = 2)+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_bathy.jpg", width = 10, height = 8)

##### extract Chl a to grid
#----Chla----

dates <- unique(str_sub(monteiro$date, 1, 11))
dates <- as.Date(dates)
dates <- dates[as.numeric(format(dates, "%m")) < 9]
dates_2018 <- dates[grep("2018", dates)]
dates_2019 <- dates[grep("2019", dates)]
dates_2020 <- dates[grep("2020", dates)]

chla_all <- raster()

for (i in 1:length(dates)){
  year <- format(dates[i], "%Y")
  month <- format(dates[i], "%m")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Chl a/Daily"
  files <- list.files(paste(dir,year,month, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'-')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,1], format="%Y%m%d")
  file_index <- which.min(abs(dates[i] - dtname))
  ras <- paste(dir,year,month,files[file_index],sep = '/')
  chla_tmp <- raster(ras)
  chla_tmp <- crop(chla_tmp, extent(extent))
  chla_all <- stack(chla_all, chla_tmp)
}

mean_chla <- calc(chla_all, fun = mean, na.rm = T)

latlon <- cbind(grid.df_highres$lon, grid.df_highres$lat)
rasValue <- raster::extract(mean_chla, latlon)
grid.df_highres$chla_all <- rasValue

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = log(chla_all))) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "log(Chla)")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_chla.jpg", width = 10, height = 8)

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)

###### make grid for SST
sst_all <- raster()

for (i in 1:length(dates)){
  year <- format(dates[i], "%Y")
  month <- format(dates[i], "%m")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/SST"
  files <- list.files(paste(dir,year,month, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'-')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,1], format="%Y%m%d")
  file_index <- which.min(abs(dates[i] - dtname))
  ras <- paste(dir,year,month,files[file_index],sep = '/')
  sst_tmp <- raster(ras)
  sst_tmp <- crop(sst_tmp, extent(extent))
  sst_all <- stack(sst_all, sst_tmp)
}

mean_sst <- calc(sst_all, fun = mean)

latlon <- cbind(grid.df_highres$lon, grid.df_highres$lat)
rasValue <- raster::extract(mean_sst, latlon)
grid.df_highres$sst_all <- rasValue- 273.15

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = sst_all)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "SST")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_sst.jpg", width = 10, height = 8)

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)

###### make grid for wind speed and direction
windspeed_all <- raster()
windeast_all <- raster()
windnorth_all <- raster()

for (i in 1:length(dates)){
  year <- format(dates[i], "%Y")
  month <- format(dates[i], "%m")
  day <- format(dates[i], "%d")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Wind"
  files <- list.files(paste(dir,year,month,day,"/", sep = "/"),pattern=".nc")
  parsename <- matrix(unlist(strsplit(files,'_')),nrow=length(files), byrow=T)
  parsename <- matrix(unlist(strsplit(parsename[,1],'b')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename, format="%Y%m%d")
  file_index <- which.min(abs(dates[i] - dtname))
  ras <- paste(dir, year, month, day, files[file_index], sep = "/")
  windspeed_tmp <- raster(ras, varname = 'wind_speed')
  windeast_tmp <- raster(ras, varname = 'eastward_wind')
  windnorth_tmp <- raster(ras, varname = 'northward_wind')
  windspeed_tmp <- crop(windspeed_tmp, extent(extent))
  windeast_tmp <- crop(windeast_tmp, extent(extent))
  windnorth_tmp <- crop(windnorth_tmp, extent(extent))
  windspeed_all <- stack(windspeed_all, windspeed_tmp)
  windeast_all <- stack(windspeed_all, windeast_tmp)
  windnorth_all <- stack(windnorth_all, windnorth_tmp)
}

mean_windspeed <- calc(windspeed_all, fun = mean, na.rm = T)
mean_windnorth <- calc(windnorth_all, fun = mean)
mean_windeast <- calc(windeast_all, fun = mean)

latlon <- cbind(grid.df_highres$lon, grid.df_highres$lat)
rasValue <- raster::extract(mean_windspeed, latlon)
grid.df_highres$windspeed_all <- rasValue

##### caluclate wind direction in degrees from east and north speeds for plotting only (low res grid)
latlon_lowres <- cbind(grid.df_lowres$lon, grid.df_lowres$lat)
rasValue_east <- raster::extract(mean_windeast, latlon_lowres)
rasValue_north <- raster::extract(mean_windnorth, latlon_lowres)
rasValue_speed <- raster::extract(mean_windspeed, latlon_lowres)
grid.df_lowres$winddir_all <- atan2(rasValue_north, rasValue_east)
grid.df_lowres$windspeed_all <- rasValue_speed

#### transform from radians to degrees
rad2deg <- function(rad) {(rad * 180) / (pi)}
grid.df_lowres$winddir_all <-rad2deg(grid.df_lowres$winddir_all)

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = windspeed_all)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "Wind speed\n(m/s)")+
  geom_spoke(data = grid.df_lowres, aes(lon, lat, angle = winddir_all, radius = scales::rescale(windspeed_all, c(.1, .8))), arrow = arrow(ends = "last", length = unit(0.1, "cm")), colour = "white") + 
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_wind.jpg", width = 11.5, height = 8)

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)

###### make grid for FSLE
fsle_all <- raster()

for (i in 1:length(dates)){
  year <- format(dates[i], "%Y")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/FSLE"
  files <- list.files(paste(dir,year, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'_')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,6], format="%Y%m%d")
  file_index <- which.min(abs(dates[i] - dtname))
  ras <- paste(dir,year,files[file_index],sep = '/')
  fsle_tmp <- raster(ras)
  fsle_rot <- rotate(fsle_tmp)
  fsle_tmp_crop <- crop(fsle_rot, extent(extent))
  fsle_all <- stack(fsle_all, fsle_tmp_crop)
  #rm(fsle_tmp, fsle_rot, fsle_tmp_crop)
  }

mean_fsle <- calc(fsle_all, fun = mean, na.rm = T)

latlon <- cbind(grid.df_highres$lon, grid.df_highres$lat)
rasValue <- raster::extract(mean_fsle, latlon)
grid.df_highres$fsle_all <- rasValue

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = fsle_all)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "FSLE")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_fsle.jpg", width = 10, height = 8)

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)

###### make grid for SSH & SLA
ssh_all <- raster()
sla_all <- raster()

for (i in 1:length(dates)){
  year <- format(dates[i], "%Y")
  month <- format(dates[i], "%m")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/SSH"
  files <- list.files(paste(dir,year,month, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'_')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,6], format="%Y%m%d")
  file_index <- which.min(abs(dates[i] - dtname))
  ras <- paste(dir,year,month,files[file_index],sep = '/')
  dataset <- parsename[,1]
  ssh_tmp <- raster(ras, varname = "adt")
  sla_tmp <- raster(ras, varname = "sla")
  if(dataset[file_index] == "dt"){
  ssh_rot <- rotate(ssh_tmp)  
  sla_rot <- rotate(sla_tmp) 
  ssh_tmp_crop <- crop(ssh_rot, extent(extent))
  sla_tmp_crop <- crop(sla_rot, extent(extent))
  } 
  else {ssh_tmp_crop <- crop(ssh_tmp, extent(extent))
  sla_tmp_crop <- crop(sla_tmp, extent(extent))
  }
  ssh_all <- stack(ssh_all, ssh_tmp_crop)
  sla_all <- stack(sla_all, sla_tmp_crop)
}

mean_ssh <- calc(ssh_all, fun = mean, na.rm = T)
mean_sla <- calc(sla_all, fun = mean, na.rm = T)

latlon <- cbind(grid.df_highres$lon, grid.df_highres$lat)
rasValue <- raster::extract(mean_ssh, latlon)
grid.df_highres$ssh_all <- rasValue


ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = ssh_all)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "SSH")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_ssh.jpg", width = 10, height = 8)

rasValue <- raster::extract(mean_sla, latlon)
grid.df_highres$sla_all <- rasValue

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = sla_all)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "SLA")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/tracks_sla.jpg", width = 10, height = 8)

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)


######### Seamounts

seamounts_pt <- read.csv("~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Bathymetry/seamounts.csv")

#### calculate distance between each point and the closest seamount
dist_seamount <- geodist(x = cbind(seamounts_pt$lon, seamounts_pt$lat), y = cbind(grid.df_highres$lon, grid.df_highres$lat), measure = 'haversine')/1000

### find distance between each point and all named sea mounts
names <- unique(as.character(seamounts_pt$Specific.Term))
mount_list <- setNames(data.frame(matrix(ncol = length(names), nrow = nrow(grid.df_highres))), names)

for(i in 1:nrow(seamounts_pt)) {
  print(i)
  seamount <- seamounts_pt[i,] 
  name <- as.character(seamounts_pt$Specific.Term)
  coords <- cbind(lon = seamount$lon, lat = seamount$lat)
  dist <- as.numeric(geodist(x = coords, y = cbind(grid.df_highres$lon, grid.df_highres$lat), measure = 'haversine')/1000)
  mount_list[[i]] <- dist
}

### find minimum distance
grid.df_highres$min_seamount_dist <- apply(mount_list, 1, FUN=min)

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = min_seamount_dist)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "seamount_dist")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)


#### Distance from colony

#### calculate distance to colony for each location
praia <- data.frame(lon = -27.955, lat = 39.056667)

dist_colony <- geodist(x = praia, y = cbind(grid.df_highres$lon, grid.df_highres$lat), measure = 'haversine')/1000
grid.df_highres$dist_colony <- as.numeric(dist_colony)

ggplot()+
  theme_void()+
  geom_tile(data = grid.df_highres, aes(lon, lat, fill = dist_colony)) +
  scale_fill_gradientn(colours = bathy_palette, na.value = "grey80", name = "dist_colony")+
  geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  geom_sf(data = azores.map)+
  coord_sf(xlim = c(-46.25445,-12.1), ylim = c(25, 52.98277), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

write.csv(grid.df_highres, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv", row.names = F)
