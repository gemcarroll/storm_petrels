require(ggplot2)
require(mgcv)
require(beepr)
require(tidymv)
require(sf)

monteiro_enviro <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_enviro2.csv")
monteiro_enviro$depth[monteiro_enviro$depth > 0] <- NA
monteiro_enviro <- subset(monteiro_enviro, ring != 'D32920' & ring != 'D47069')

#### read prediction grid 
pred_grid <- read.csv("/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/prediction_grid.csv")

names(pred_grid) <- c("lon", "lat", "depth", "Chla", "SST", "wind_speed", "FSLE", "SSH", "SLA", "min_seamount_dist", "dist_colony")
pred_grid$ID <- 'D22802_NA'

##### recreate Praia buffer
#### create buffer around Praia Islet corresponding to max foraging distance 
praia <- data.frame(lon = -27.955, lat = 39.056667)
praia = st_as_sf(praia, coords = c("lon", "lat"))
st_crs(praia) <- 4326
praia2 = st_transform(praia, crs = 2188)
praia_buffer <- st_buffer(praia2, dist = 963963)
praia_buffer2 <- st_transform(praia_buffer, crs = 4326)

#### load azores map
load('/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/azores_map.RData')

##### build generalised additive mixed models for storm petrel foraging in relation to the following candidate environmental covariates:

#### NULL model (bstage, id)
### Chla
### bathymetry
### distance to seamount
### SST
### SSH
### FSLE
### wind speed

#### single variable models:

gam_null <- gam(pres ~ bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_depth <- gam(pres ~ s(depth, k = 5) + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_seamount <- gam(pres ~ s(min_seamount_dist, k = 5)  + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_chla <- gam(pres ~ s(Chla, k = 5)  + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_SST <- gam(pres ~ s(SST, k = 5)  + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_SSH <- gam(pres ~ s(SSH, k = 5)  + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_wind <- gam(pres ~ s(wind_speed, k = 5)  + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_fsle <- gam(pres ~ s(FSLE, k = 5)  + bstage + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')

AIC(gam_depth, gam_seamount, gam_chla, gam_SST, gam_SSH, gam_wind, gam_fsle)

gam_depth_plot <- gam(pres ~ s(depth, k = 5), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_seamount_plot <- gam(pres ~ s(min_seamount_dist, k = 5), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_sst_plot <- gam(pres ~ s(SST, k = 5), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_chla_plot <- gam(pres ~ s(Chla, k = 5), family = "binomial", data = monteiro_enviro)
beep('ping')

jpeg(file="/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/depth_gam.jpeg")
plot(gam_depth_plot, rug = TRUE, col = "#487eb0", lwd = 2, shade=TRUE, shade.col="grey80", xlab = "Depth", ylab = "s(Depth)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

jpeg(file="/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/seamount_gam.jpeg")
plot(gam_seamount_plot, rug = TRUE, col = "#487eb0", lwd = 2, shade=TRUE, shade.col="grey80", xlab = "Min. Distance to Seamount (km)", ylab = "s(Seamount_dist)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

jpeg(file="/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/sst_gam.jpeg")
plot(gam_sst_plot, rug = TRUE, col = "#487eb0", lwd = 2, shade=TRUE, shade.col="grey80", xlab = expression("Sea surface temperature "^o~C), ylab = "s(SST)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

jpeg(file="/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/chla_gam.jpeg")
plot(gam_seamount_plot, rug = TRUE, col = "#487eb0", lwd = 2, shade=TRUE, shade.col="grey80", xlab = expression("Chl a concentration mg m"^-3), ylab = "s(Chl a)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

#### predictive models:
gam_null <- gam(pres ~ bstage + dist_colony + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_depth <- gam(pres ~ bstage + s(depth, k = 5) + s(min_seamount_dist, k = 5) + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')
gam_depth_chlasst <- gam(pres ~ bstage  + s(depth, k = 20) + s(min_seamount_dist, k = 20) + s(SST, k = 20) + s(Chla, k = 20) + s(ID, bs = "re"), family = "binomial", data = monteiro_enviro)
beep('ping')

summary(gam_depth_chlasst)

#### make spatial model predictions for incubation period
pred_grid$bstage <- "incubation"
pred_grid$pred_incubation <- predict.gam(gam_depth_chlasst, newdata = pred_grid, type = "response")

pred_palette <- c("#f7f1e3", "#fad390", "#ff793f", "#c44569", "#6D214F", "#182C61") 

ggplot()+
  theme_void()+
  geom_tile(data = pred_grid, aes(lon, lat, fill = pred_incubation)) +
  scale_fill_gradientn(colours = pred_palette, na.value = "grey60", name = "Prediction")+
  #geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 2.5)+
  geom_sf(data = azores.map)+
  geom_sf(data = praia_buffer2, colour = "grey70", fill = NA, size = 1.8, linetype = 2, alpha = 0.8)+
  coord_sf(xlim = c(-39.7,-16.3), ylim = c(30, 48), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))

ggsave(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/Figures/prediction.jpg", width = 10, height = 8)

##### make spatial model predictions for chick-rearing
pred_grid$bstage <- 'chick'
pred_grid$pred_chick <- predict.gam(gam_depth_chlasst, newdata = pred_grid, type = "response")

ggplot()+
  theme_void()+
  geom_tile(data = pred_grid, aes(lon, lat, fill = pred_chick)) +
  scale_fill_gradientn(colours = pred_palette, na.value = "grey80", name = "Prediction")+
  #geom_point(data = subset(monteiro, pres == 1), aes(x, y), size = 1.8, shape = 21, colour = "grey30", fill = "grey95")+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 2.5)+
  geom_sf(data = azores.map)+
  geom_sf(data = praia_buffer2, colour = "grey90", fill = NA, size = 1.8, linetype = 2, alpha = 0.8)+
  coord_sf(xlim = c(-39.7,-16.3), ylim = c(30, 48), expand = F, crs = "+proj=longlat +ellps=WGS84")+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  theme(axis.title = element_text(size=24), legend.title=element_text(size=24), legend.text=element_text(size=16), axis.text = element_text(size = 18))
