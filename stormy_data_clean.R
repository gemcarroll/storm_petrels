require(stringr)
require(mapdata)
require(ggplot2)
require(dplyr)
require(mapdata)
require(sf)
require(zoo)
library(geodist)

#### cleaning Azores storm petrel GPS data collected by Verónica Neves
#### Jan 2021

#### Monteiro's storm petrel

#### import 2018/2019
monteiro <- read.table("~/Google Drive/NOAA/Projects/Azores/Storm petrels/tracking data/monteiro/Monteiro's Azores tracks 2018 & 2019 (2).csv", sep = ";")

monteiro <- monteiro[2:nrow(monteiro),]
monteiro$date <- as.POSIXct(monteiro$V3, format = "%d/%m/%Y %H:%M")
monteiro$species <- "Hydrobates monteiroi"
monteiro$site <- "Praia"
monteiro$lat <- str_replace(monteiro$V4, ",", ".")
monteiro$lon <- str_replace(monteiro$V5, ",", ".")

monteiro <- monteiro[, c(15, 16, 14, 17, 18, 6, 7, 8, 9, 10, 11, 12, 13)]
names(monteiro) <- c("species", "site", "date", "lat", "lon", "ring", "nest", "bstage", "sex", "age", "trips", "tag", "interval")

#### import 2020
monteiro_2020 <- read.table("~/Google Drive/NOAA/Projects/Azores/Storm petrels/tracking data/monteiro/2020 GPS data for H monteiroi.csv", sep = ";", skip = 1)

date1 <- as.POSIXct(monteiro_2020$V3, format = "%Y-%m-%d %H:%M:%S")
date2 <- as.POSIXct(monteiro_2020$V3, format = "%d/%m/%Y %H:%M")
date2[is.na(date2)] <- date1[!is.na(date1)]
monteiro_2020$date <- date2
monteiro_2020$species <- "Hydrobates monteiroi"
monteiro_2020$site <- "Praia"
monteiro_2020$lat <- str_replace(monteiro_2020$V4, ",", ".")
monteiro_2020$lon <- str_replace(monteiro_2020$V5, ",", ".")

monteiro_2020 <- monteiro_2020[, c(17, 18, 16, 19, 20, 6, 7, 8, 9, 10, 11, 12, 13)]
names(monteiro_2020) <- c("species", "site", "date", "lat", "lon", "ring", "nest", "bstage", "sex", "age", "trips", "tag", "interval")

monteiro <- rbind(monteiro, monteiro_2020)
monteiro$lat <- as.numeric(monteiro$lat)
monteiro$lon <- as.numeric(monteiro$lon)
monteiro$sex <- as.character(monteiro$sex)
monteiro$sex[monteiro$sex == "m"] <- "M"
monteiro$sex[monteiro$sex == ""] <- "unknown"
monteiro$year <- str_sub(monteiro$date, 1,4)
monteiro$ring <-  as.factor(monteiro$ring)
monteiro$id <- paste(monteiro$ring, monteiro$tag, sep = "_")

#### number of birds tagged 
length(unique(monteiro$ring))

#### number of deployments 
length(unique(monteiro$id))

### remove na values 
monteiro <- monteiro[!is.na(monteiro$lon),]

### remove 0 values 
monteiro <- monteiro[!monteiro$lon == 0,]

##### breeding stage corrections - summarise to incubation & chick-rearing (as soon as birds have chicks they are chick-rearing)
monteiro$bstage <- as.character(monteiro$bstage)
monteiro$bstage[monteiro$bstage == "Egg" | monteiro$bstage == "egg" | monteiro$bstage == "late egg"] <- "incubation"
monteiro$bstage[grep(c("chick"), monteiro$bstage)] <- "chick"
monteiro$bstage[grep(c("Ch"), monteiro$bstage)] <- "chick"

##### correct bird where lat an lon are switched
D61387_NA <- subset(monteiro, id == "D61387_NA")
lon <- D61387_NA$lon 
lat <- D61387_NA$lat 

D61387_NA$lon <- lat
D61387_NA$lat <- lon

monteiro <- monteiro %>% filter(!id == "D61387_NA")
monteiro <- rbind(monteiro, D61387_NA)

azores.map <- st_read("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/Environment/EEA_Coastline_Polygon_Shape/EEA_Coastline_Polyline_Shape/Europe_coastline_raw_rev2017.shp")
azores.map <- azores.map[azores.map$Id]

azores.map <- st_transform(azores.map, crs = 4269)
azores.map <- st_crop(azores.map, extent(-46.06-1, -9.81+1, 25.04-1, 52.79+1))
praia <- c(-27.955, 39.056667)

save(azores.map, file = '/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/azores_map.RData')

##### plot tracks
ggplot()+
  theme_void()+
  geom_point(data = monteiro, aes(lon, lat, colour = year), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  #xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

##### check differences due to breeding stage
ggplot()+
  theme_void()+
  geom_point(data = monteiro, aes(lon, lat, colour = bstage), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()


##### check number of locations for each bird 
n_pts <- monteiro %>% group_by(id) %>% summarise(no_pts = length(lat))

#### remove short trip
monteiro <- monteiro[!monteiro$ring == "D50464",]

#### check time intervals
difftime <- monteiro %>% group_by(id) %>% summarise(difftime = mean(diff(date))/60, interval = interval[1])

#### make sure tracks are in the correct order
monteiro <- monteiro[with(monteiro, order(id, date)), ]

#### as most tracks are 1 hour interval but a few are 2 or 3 hours, we should perform linear interpolation so all tracks are 1 hr
bird_list <- NULL

for(i in unique(monteiro$id)) {
  print(i)
  bird <- subset(monteiro, id == i) 
  z <- zoo(bird,order.by=bird$date)
  st <- bird$date[1]
  end <- bird$date[nrow(bird)]
  g <- seq(st, end, by = "2 hour") # grid
  lat <- na.approx(z$lat, xout = g)
  lon <- na.approx(z$lon, xout = g)
  bird_df <- data.frame(cbind(lon = as.numeric(lon), lat = as.numeric(lat)))
  bird_df$date <- index(lon)
  #bird_df$date <- as.POSIXct(row.names(bird_df), formt = "%Y-%m-%d %H:%M:%S")
  rownames(bird_df) <- NULL
  bird_df$id <- i
  bird_list <- rbind(bird_list, bird_df)
}

monteiro2 <- subset(monteiro, select = -c(date, lat, lon))
monteirox  <- unique(monteiro2)

monteiro <- left_join(bird_list, monteirox, by = "ID")

##### check tracks again
ggplot()+
  theme_void()+
  geom_point(data = monteiro, aes(lon, lat, colour = bstage), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

#### calculate distance to colony for each location
praia <- data.frame(lon = -27.955, lat = 39.056667)

dist_colony <- geodist(x = praia, y = cbind(monteiro$lon, monteiro$lat))/1000
monteiro$dist_colony <- as.numeric(dist_colony)
  
monteiro <- monteiro[monteiro$dist_colony > 0.5,]

ggplot()+
  theme_void()+
  geom_point(data = subset(data, pres == 1), aes(lon, lat, colour = dist_colony), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

#### get summary stats for sample sizes
monteiro %>% group_by(year, bstage, sex) %>% summarise(length(unique(tag)))

#### calculate mean max distance from colony and trip duration
ste <- function(x) sd(x)/sqrt(length(x))

monteiro %>% group_by(id, year, bstage, sex) %>% summarise(max_dist = max(dist_colony)) %>% group_by(year, bstage) %>% summarise(mean_max_dist = mean(max_dist), se_max_dist = ste(max_dist))

#### find single trips to calculate duration
monteiro$trips[grep(c("one", "one  "), monteiro$trips)] <- "one"

trip_durat <- monteiro %>% filter(trips == "one") %>% filter(id != "D47069_NA")  %>% group_by(id, year, bstage, sex) %>% summarise(trip_dur = date[1] - date[length(date)]) %>% group_by(year, bstage) %>% summarise(mean_dur = mean(trip_dur), se_dur = ste(trip_dur))


#### save monteiro file 
write.csv(monteiro, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_clean.csv", row.names = F)


##### Madeiran storm petrel 
#### import 2018/2019
madeiran <- read_csv("~/Google Drive/NOAA/Projects/Azores/Storm petrels/tracking data/madeiran /FINAL 2018-2020 GPS data for Hydrobates castro Azores.csv")
madeiran <- data.frame(madeiran)

names(madeiran) <- c("species", "site", "date", "lat", "lon", "ring", "nest", "bstage", "sex", "age", "trips", "tag", "interval", "id")
madeiran$date <- as.POSIXct(madeiran$date)
madeiran$species <- "Hydrobates castro"
madeiran$sex[madeiran$sex == "N"] <- "unknown"
madeiran$year <- str_sub(madeiran$date, 1,4)
madeiran$ring <-  as.factor(madeiran$ring)
madeiran$id <- paste(madeiran$ring, madeiran$tag, sep = "_")

#### number of birds tagged 
length(unique(madeiran$ring))

#### number of deployments 
length(unique(madeiran$id))

### remove na values 
madeiran <- madeiran[!is.na(madeiran$lon),]
### remove 0 values 
madeiran <- madeiran[!madeiran$lon == 0,]

##### breeding stage corrections - summarise to incubation & chick-rearing (as soon as birds have chicks they are chick-rearing)
madeiran$bstage <- as.character(madeiran$bstage)
madeiran$bstage[madeiran$bstage == "Egg" | madeiran$bstage == "egg" | madeiran$bstage == "EGG"] <- "incubation"
madeiran$bstage[grep(c("CHICK"), madeiran$bstage)] <- "chick"

##### plot tracks
ggplot()+
  theme_void()+
  geom_point(data = madeiran, aes(lon, lat, colour = year), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  #xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

##### check differences due to breeding stage
ggplot()+
  theme_void()+
  geom_point(data = madeiran, aes(lon, lat, colour = bstage), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()


##### check number of locations for each bird 
n_pts <- madeiran %>% group_by(id) %>% summarise(no_pts = length(lat))

#### calculate distance to colony for each location
praia <- data.frame(lon = -27.955, lat = 39.056667)

dist_colony <- geodist(x = praia, y = cbind(madeiran$lon, madeiran$lat))/1000
madeiran$dist_colony <- as.numeric(dist_colony)

madeiran <- madeiran[madeiran$dist_colony > 0.5,]

#### save monteiro file 
write.csv(madeiran, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/madeiran_clean.csv", row.names = F)
