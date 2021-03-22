require(ggplot2)
require(dplyr)
require(mapdata)
require(foieGras)

##### note that state space modeling may not be necessary for these data, this is just in case track regularisation becomes important during analysis for some reason. Using the SSMs will probably result in losing some tracks that don't need to be thrown out if we use raw GPS


########### run ssm for gps tracks 
madeiran_gps <- read.csv('/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/tracking data/madeiran.csv')

azores.map = map('worldHires', fill=T, col='black', plot=F, ylim = c(35,57))

ggplot()+
  theme_void()+
  geom_point(data = madeiran_gps, aes(location.long, location.lat), col = "blue")+
  geom_path(data = azores.map, aes(long, lat, group = group))+
  xlim(c(-45,-10)) +ylim(c(20, 55))+
  theme_classic()

#### clean up data sets 
madeiran_gps$timestamp <- as.POSIXct(madeiran_gps$timestamp)

### add location class for state space model
madeiran_gps$lc <- 3

### need to create a truly unique identifier because birds take multiple trips sometimes with the same logger
madeiran_gps$id <- madeiran_gps$individual.local.identifier

madeiran_gps <- madeiran_gps[, c("id", "timestamp", "lc", "location.long", "location.lat")] 
names(madeiran_gps) <- c("id", "date", "lc", "lon", "lat")

ggplot()+
  geom_point(data = madeiran_gps, aes(lon, lat, col = id))+
  facet_wrap(~id)

### remove short Madeiran tracks where the bird was sitting on the water around the island
x <- madeiran_gps %>% group_by(id) %>% summarise(max_lon = max(lon), max_lat = max(lat), min_lon = min(lon), min_lat = min(lat), lon_diff = max_lon - min_lon, lat_diff = max_lat - min_lat) %>% filter(lon_diff > 1 | lat_diff > 1)

madeiran_gps <- madeiran_gps[madeiran_gps$id %in% x$id,]

ggplot()+
  geom_point(data = madeiran_gps, aes(lon, lat, col = id))+
  facet_wrap(~id)

ggplot()+
  geom_path(data = subset(madeiran_gps, id == "D47417_20728"), aes(lon, lat, col = id))

#x <- subset(madeiran_gps, id == "D55405")

#### fit ssm to Madeiran tracks
fit_madeiran_gps <- fit_ssm(madeiran_gps, model = "crw", time.step = 2)

#### 4 models did not converge with 2 hour time step, try again with 1 hour
unconverged <- fit_madeiran_gps$id[fit_madeiran_gps$converged == "FALSE"] 
madeiran_retry <- madeiran_gps[madeiran_gps$id %in% unconverged,]
fit_madeiran_retry <- fit_ssm(madeiran_retry, model = "crw", time.step = 1)

plot(fit_madeiran_retry, what = "predicted", type = 2)

#### 2 models still did not converge due to large gaps when the bird is not moving, leave them out for now 

preds_madeiran1 <- grab(fit_madeiran_gps, what = "predicted", as_sf = FALSE)
preds_madeiran2 <- grab(fit_madeiran_retry, what = "predicted", as_sf = FALSE)

### remove every 2nd observation from 1 hour tracks so that time step is consistently 2 hours
preds_madeiran2 <- preds_madeiran2 %>% filter(row_number() %% 2 != 1)

madeiran_locs <- rbind(preds_madeiran1, preds_madeiran2)

ggplot()+
  geom_point(data = madeiran_locs, aes(lon, lat, col = id))

ggplot()+
  geom_point(data = madeiran_locs, aes(lon, lat, col = id))+
  facet_wrap(~id)

##### problem tracks with large implausible loops
probs <- c("D47456","D47417","D47428")
x <- subset(madeiran_gps, id == id %in% probs)


#### remove tracks that seem implausible
madeiran_locs <- subset(madeiran_locs, id != "D47456")
madeiran_locs <- subset(madeiran_locs, id != "D47417")
madeiran_locs <- subset(madeiran_locs, id != "D47428") ### note, this is a good track, the ssm just didn't estimate well

ggplot()+
  theme_void()+
  geom_point(data = madeiran_locs, aes(lon, lat), col = "blue")+
  geom_path(data = azores.map, aes(long, lat, group = group))+
  xlim(c(-45,-10)) +ylim(c(20, 55))+
  theme_classic()

write.csv(madeiran_locs, 'madeiranlocs_ssm.csv', row.names = F)



######## Monteiro's storm petrel

monteiro_gps <- read.csv('/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/tracking data/monteiros.csv')

#### remove the pseudoabsence points form the original monteiro data set
monteiro_gps <- monteiro_gps[!monteiro_gps$individual.local.identifier == "PA",]
monteiro_gps$lc <- 3

### need to create a truly unique identifier because birds take multiple trips sometimes with the same logger
monteiro_gps$id <- paste(monteiro_gps$individual.local.identifier, monteiro_gps$tag.local.identifier, sep = "_")

### clean up data set
monteiro_gps <- monteiro_gps[, c("id", "timestamp", "lc", "location.long", "location.lat")] 

names(monteiro_gps) <- c("id", "date", "lc", "lon", "lat")

##### remove tracks where location could not be estimated
monteiro_gps <- subset(monteiro_gps, !is.na(lon))

#### work out difference in time between locations
monteiro_gps$date <- as.POSIXct(monteiro_gps$date)

#### correct bird # 2 which has two separate deployments with the same GPS tag
monteiro_gps$id[monteiro_gps$id == "2_20642" & monteiro_gps$date > as.POSIXct("2018-08-01 00:00:00")] <- "2_20642_2"

ggplot()+
  geom_point(data = monteiro_gps, aes(lon, lat, col = id))+
  facet_wrap(~id)

### remove short tracks where the bird was sitting on the water around the island
x <- monteiro_gps %>% group_by(id) %>% summarise(max_lon = max(lon), max_lat = max(lat), min_lon = min(lon), min_lat = min(lat), lon_diff = max_lon - min_lon, lat_diff = max_lat - min_lat) %>% filter(lon_diff > 1 | lat_diff > 1)

monteiro_gps <- monteiro_gps[monteiro_gps$id %in% x$id,]

ggplot()+
  geom_point(data = monteiro_gps, aes(lon, lat, col = id))+
  facet_wrap(~id)

monteiro_gps <- monteiro_gps[, 1:5]

#### fit ssm to monteiro tracks
fit_monteiro_gps <- fit_ssm(monteiro_gps, model = "crw", time.step = 2)

#### 4 models did not converge with 2 hour time step, try again with 1 hour
unconverged <- fit_monteiro_gps$id[fit_monteiro_gps$converged == "FALSE"] 
monteiro_retry <- monteiro_gps[monteiro_gps$id %in% unconverged,]
fit_monteiro_retry <- fit_ssm(monteiro_retry, model = "crw", time.step = 1)

plot(fit_monteiro_retry, what = "predicted", type = 2)

#### 2 models still did not converge due to large gaps when the bird is not moving, leave them out for now 

preds_monteiro1 <- grab(fit_monteiro_gps, what = "predicted", as_sf = FALSE)
preds_monteiro2 <- grab(fit_monteiro_retry, what = "predicted", as_sf = FALSE)

### remove every 2nd observation so that time step is consistently 2 hours
preds_monteiro2 <- preds_monteiro2 %>% filter(row_number() %% 2 != 1)

monteiro_locs <- rbind(preds_monteiro1, preds_monteiro2)

ggplot()+
  geom_point(data = monteiro_locs, aes(lon, lat, col = id))+
  geom_path(data = azores.map, aes(long, lat, group = group))+
  xlim(c(-45,-10)) +ylim(c(20, 55))+
  theme_classic()

ggplot()+
  geom_point(data = subset(monteiro_locs, id == "2_20642"), aes(lon, lat, col = id))+
  facet_wrap(~id)


ggplot()+
  theme_void()+
  geom_point(data = madeiran_locs, aes(lon, lat), col = "red")+
  geom_point(data = madeiran_gps, aes(lon, lat), col = "blue")+
  geom_path(data = azores.map, aes(long, lat, group = group))+
  xlim(c(-45,-10)) +ylim(c(20, 55))+
  theme_classic()

write.csv(madeiran_locs, 'madeiranlocs_ssm.csv', row.names = F)


#### combine all petrel tracks together
madeiran_locs$species <- "madeiran"
monteiro_locs$species <- "monteiros"

petrel_locs <- rbind(madeiran_locs, monteiro_locs)

write.csv(petrel_locs, file = "petrel_locs_ssm.csv", row.names = F)

#### combine all petrel tracks together
madeiran_gps$species <- "madeiran"
monteiro_gps$species <- "monteiros"

petrel_locs <- rbind(madeiran_gps, monteiro_gps)

write.csv(petrel_gps, file = "petrel_locs.csv", row.names = F)




