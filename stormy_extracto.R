require(ncdf4)
require(R.utils)
require(stringr)
require(lubridate)
require(raster)
require(dplyr)
require(sf)
library(geodist)
require(ggplot2)

#### code to extract environmental covariates along Azores storm petrel tracks

#### read monteiro file 
data <- read.csv(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_pseudoabs.csv")
data$date <- as.Date(data$date)
data$year <- as.Date(data$year, format = "%Y")
names(data)[4] <- 'lon'
names(data)[5] <- 'lat'

desired.resolution = 0.1
desired.resolution = desired.resolution/2

#----SST----
print('EXTRACTING SST')
data$SST <- NA
data$SST_SD <- NA

for (i in 1:nrow(data)){
  print(paste("SST",i, sep = " "))
  year <- format(data$date[i], "%Y")
  month <- format(data$date[i], "%m")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/SST"
  files <- list.files(paste(dir,year,month, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'-')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,1], format="%Y%m%d")
  file_index <- which.min(abs(data$date[i] - dtname))
  nc <- paste(dir,year,month,files[file_index],sep = '/')
  nc.data <- nc_open(nc)
  lat <- ncvar_get(nc.data,'lat')
  lon <- ncvar_get(nc.data,'lon') #in 180 degrees
  nrows <- length(lat); ncols <- length(lon)
  c <- which.min(abs(lon-data$lon[i]))
  c_low <- which.min(abs(lon-(data$lon[i]-desired.resolution)))
  c_up <- which.min(abs(lon-(data$lon[i]+desired.resolution)))
  r <- which.min(abs(lat-data$lat[i]))
  r_low <- which.min(abs(lat-(data$lat[i]-desired.resolution)))
  r_up <- which.min(abs(lat-(data$lat[i]+desired.resolution)))
  numcols=abs(c_up-c_low)+1; numrows=abs(r_up-r_low)+1
  
  data.var1  <-  ncvar_get(nc.data,'analysed_sst',start=c(c_low,r_low,1), count=c(numcols,numrows,1),verbose=FALSE)
  data.var1 <- data.var1 - 273.15
  
  data$SST[i] <- mean(data.var1,na.rm=T)
  data$SST_SD[i] <- sd(data.var1,na.rm=T)
  
  nc_close(nc.data)
}

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro2.csv", row.names = F)

#----Chla: 4km and 8day----
#Chlorophyll-a at 4km over 8day

data$Chla <- NA
data$Chla_SD <- NA
for (i in 1:nrow(data)){
  print(paste("Chla",i, sep = " "))
  year <- format(data$date[i], "%Y")
  month <- format(data$date[i], "%m")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Chl a/Daily"
  files <- list.files(paste(dir,year,month, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'-')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,1], format="%Y%m%d")
  file_index <- which.min(abs(data$date[i] - dtname))
  nc <- paste(dir,year,month,files[file_index],sep = '/')
  nc.data <- nc_open(nc)
  lat <- ncvar_get(nc.data,'lat')
  lon <- ncvar_get(nc.data,'lon') #180 degrees
  nrows <- length(lat); ncols <- length(lon)
  c <- which.min(abs(lon-data$lon[i]))
  c_low <- which.min(abs(lon-(data$lon[i]-desired.resolution)))
  c_up <- which.min(abs(lon-(data$lon[i]+desired.resolution)))
  r <- which.min(abs(lat-data$lat[i]))
  r_low <- which.min(abs(lat-(data$lat[i]-desired.resolution)))
  r_up <- which.min(abs(lat-(data$lat[i]+desired.resolution)))
  numcols=abs(c_up-c_low)+1; numrows=abs(r_up-r_low)+1
  
  #Variable 1: CHL
  data.var1  <-  ncvar_get(nc.data,'CHL',start=c(c_low,r_low,1), count=c(numcols,numrows,1),verbose=FALSE)
  data$Chla[i] <- mean(data.var1,na.rm=T)
  data$Chla_SD[i] <- sd(data.var1,na.rm=T)
  
  nc_close(nc.data)
}

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro.csv", row.names = F)

#-----AVISO FSLE-------
data$FSLE <- NA
data$FSLE_SD <- NA
data$theta_max <- NA

for (i in 1:nrow(data)){
  print(paste("FSLE",i, sep = " "))
  year <- format(data$date[i], "%Y")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/FSLE"
  files <- list.files(paste(dir,year, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'_')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,6], format="%Y%m%d")
  file_index <- which.min(abs(data$date[i] - dtname))
  file_name <- paste(dir,year,files[file_index], sep="/")
  nc.data <- nc_open(file_name)
  lat <- ncvar_get(nc.data,'lat')
  lon <- ncvar_get(nc.data,'lon') #in 360 degrees
  lon <- ifelse(lon>180,-180+(lon-180),lon) 
  nrows <- length(lat); ncols <- length(lon)
  c <- which.min(abs(lon-data$lon[i]))
  c_low <- which.min(abs(lon-(data$lon[i]-desired.resolution)))
  c_up <- which.min(abs(lon-(data$lon[i]+desired.resolution)))
  r <- which.min(abs(lat-data$lat[i]))
  r_low <- which.min(abs(lat-(data$lat[i]-desired.resolution)))
  r_up <- which.min(abs(lat-(data$lat[i]+desired.resolution)))
  numcols=abs(c_up-c_low)+1; numrows=abs(r_up-r_low)+1
  
  data.var1  <-  ncvar_get(nc.data,'fsle_max',start=c(c_low,r_low,1), count=c(numcols,numrows,1),verbose=FALSE)
  data.var2  <-  ncvar_get(nc.data,'theta_max',start=c(c_low,r_low,1), count=c(numcols,numrows,1),verbose=FALSE)
  
  data$FSLE[i] <- mean(data.var1,na.rm=T)
  data$FSLE_SD[i] <- sd(data.var1,na.rm=T)
  data$theta_max[i] <- mean(data.var2,na.rm=T)
  
  nc_close(nc.data)
}

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro.csv", row.names = F)


#----SSH, SLA, U, and V----
print('EXTRACTING SSH')
#Native resolution is 0.25 so extracting point value

data["SSH"] <- NA
data["SLA"] <- NA
data["U"] <- NA
data["V"] <- NA

for (i in 1:nrow(data)){
  print(paste("SSH",i, sep = " "))
  #Figure out which file to grab
  year <- format(data$date[i], "%Y")
  month <- format(data$date[i], "%m")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/SSH"
  files <- list.files(paste(dir,year,month, sep = "/"))
  parsename <- matrix(unlist(strsplit(files,'_')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename[,6], format="%Y%m%d")
  file_index <- which.min(abs(data$date[i] - dtname))
  nc <- paste(dir,year,month,files[file_index], sep = "/")
  nc.data <- nc_open(nc, write=FALSE)
  lat <- ncvar_get(nc.data,'latitude')
  lon <- ncvar_get(nc.data,'longitude') #360 degrees
  dataset <- parsename[,1]
  if(dataset[file_index] == "dt"){
    lon <- ifelse(lon>180,-180+(lon-180),lon)   
  } 
  nrows <- length(lat); ncols <- length(lon)
  c <- which.min(abs(lon-data$lon[i]))
  r <- which.min(abs(lat-data$lat[i]))
  #Variable 1: SLA
  data.var1 <-  ncvar_get(nc.data,'sla',start=c(c,r,1),  count=c(1,1,1),verbose=FALSE)
  data$SSH[i] <- data.var1
  #Variable 2: SSH
  data.var2 <-  ncvar_get(nc.data,'adt',start=c(c,r,1),count=c(1,1,1),verbose=FALSE)
  data$SLA[i] <- data.var2
  #Variable 3: U
  data.var3  <-  ncvar_get(nc.data,'ugos',start=c(c,r,1),count=c(1,1,1),verbose=FALSE)
  data$U[i] <- data.var3
  #Variable 4: V
  data.var4  <-  ncvar_get(nc.data,'vgos',start=c(c,r,1),count=c(1,1,1),verbose=FALSE)
  data$V[i] <- data.var4
  nc_close(nc.data)
} 

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro.csv", row.names = F)

#----iFremer WIND----
print('EXTRACTING WIND')
data$wind_speed <- NA
data$eastward_wind <- NA
data$northward_wind <- NA
data$wind_stress <- NA


for (i in 1:nrow(data)){
  print(paste("wind",i, sep = " "))
  year <- format(data$date[i], "%Y")
  month <- format(data$date[i], "%m")
  day <- format(data$date[i], "%d")
  dir <- "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Wind"
  files <- list.files(paste(dir,year,month,day,"/", sep = "/"),pattern=".nc")
  parsename <- matrix(unlist(strsplit(files,'_')),nrow=length(files), byrow=T)
  parsename <- matrix(unlist(strsplit(parsename[,1],'b')),nrow=length(files), byrow=T)
  dtname <- as.Date(parsename, format="%Y%m%d")
  file_index <- which.min(abs(data$date[i] - dtname))
  file_name <- paste(dir, year, month, day, files[file_index], sep = "/")
  if(any(grepl("bz2", file_name))){
    bzip <- bunzip2(file_name, overwrite=FALSE, remove=FALSE)
    nc.data <- nc_open(bzip)
  } else{
    nc.data <- nc_open(file_name)
  }
  lat <- ncvar_get(nc.data,'latitude')
  lon <- ncvar_get(nc.data,'longitude') #in 180 degrees
  nrows <- length(lat); ncols <- length(lon)
  c <- which.min(abs(lon-data$lon[i]))
  r <- which.min(abs(lat-data$lat[i]))
  #Variable 1: wind speed
  data.var1 <-  ncvar_get(nc.data,'wind_speed',start=c(c,r,1,1),
                          count=c(1,1,1,1),verbose=FALSE)
  data.var2 <-  ncvar_get(nc.data,'eastward_wind',start=c(c,r,1,1),
                          count=c(1,1,1,1),verbose=FALSE)
  data.var3 <-  ncvar_get(nc.data,'northward_wind',start=c(c,r,1,1),
                          count=c(1,1,1,1),verbose=FALSE)
  data.var4 <-  ncvar_get(nc.data,'wind_stress',start=c(c,r,1,1),
                          count=c(1,1,1,1),verbose=FALSE)
  data$wind_speed[i] <- data.var1
  data$eastward_wind[i] <- data.var2
  data$northward_wind[i] <- data.var3
  data$wind_stress[i] <- data.var4
  
  nc_close(nc.data)
}

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro.csv", row.names = F)


#----Bathymetry----
bathy <- raster(x = "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Bathymetry/ETOPO1_Bed_g_gdal.grd")

bathy_plot <- crop(bathy, extent(min(data$lon)-1, max(data$lon)+1, min(data$lat)-1, max(data$lat)+1))
plot(bathy_plot)
plot(azores.map, add = T)

bathy_df <- as.data.frame(bathy_plot, xy = TRUE)

ggplot()+
  theme_void()+
  geom_tile(data = bathy_df, aes(x=x, y=y, fill=layer)) +
  #geom_point(data = subset(data, pres == 1), aes(lon, lat), size = 0.7, alpha = 0.6, type = 21, colour = "black", fill = "white")+
  #geom_point(data = subset(seamounts_pt), aes(lon, lat), colour = "white", size = 2)+
  #geom_path(data = subset(midatlrid), aes(x, y), colour = "white", size = 1.5, linetype = 2, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  #xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()


##### extract bathymetry to tracks
latlon <- cbind(data$lon, data$lat)
rasValue=extract(bathy, latlon)
data$depth <- rasValue

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro.csv", row.names = F)

### distance to seamount 
#### read seamount file
seamounts <- read.csv("~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Bathymetry/features.csv")
seamounts_pt <- seamounts %>% filter(Generic.Term == "Seamount" | Generic.Term == "Seamounts") %>% filter(grepl("POINT", Coordinates))
latlon <- str_sub(seamounts_pt$Coordinates, 8, -2)
latlon <- strsplit(latlon,' ')
seamounts_pt$lon <- as.numeric(do.call( rbind, latlon)[,1])
seamounts_pt$lat <- as.numeric(do.call( rbind, latlon)[,2])
seamounts_li <- seamounts %>% filter(Generic.Term == c("Seamount", "Seamounts")) %>% filter(!grepl("POINT", Coordinates))
latlon <- str_sub(seamounts_li$Coordinates, 13, -2)
latlon <- strsplit(latlon,' ')

seamounts_pt <- seamounts_pt[, c(1, 13:14)]

write.csv(seamounts_pt, "~/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/Bathymetry/seamounts.csv", row.names = F)

#### calculate distance between each point and the closest seamount
dist_seamount <- geodist(x = cbind(seamounts_pt$lon, seamounts_pt$lat), y = cbind(data$lon, data$lat), measure = 'haversine')/1000

### find distance between each point and all names sea mounts
names <- unique(as.character(seamounts_pt$Specific.Term))
mount_list <- setNames(data.frame(matrix(ncol = length(names), nrow = nrow(data))), names)

for(i in 1:nrow(seamounts_pt)) {
  print(i)
  seamount <- seamounts_pt[i,] 
  name <- as.character(seamount$Specific.Term)
  coords <- cbind(lon = seamount$lon, lat = seamount$lat)
  dist <- as.numeric(geodist(x = coords, y = cbind(data$lon, data$lat), measure = 'haversine')/1000)
  mount_list[[i]] <- dist
}

### find minimum distance
data$min_seamount_dist <- apply(mount_list, 1, FUN=min)

write.csv(data, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro.csv", row.names = F)

x#### mid-Atlantic ridge
x <- c(-27.28, -28.24, -29.07, -30.19, -32.57, -36.26, -39.33, -43.4)
y <- c(46.75, 44.25, 41.72, 39.48, 37.28, 35.05, 32.51, 28.37)
midatlrid <- data.frame(x =x, y=y)

ggplot()+
  theme_void()+
  geom_point(data = subset(data, pres == 1), aes(lon, lat, colour = dist_colony), size = 0.7, alpha = 0.6)+
  geom_point(data = subset(seamounts_pt), aes(lon, lat), colour = "red", size = 3)+
  geom_path(data = subset(midatlrid), aes(x, y), colour = "red", size = 1.5, type = 4, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  #xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()



