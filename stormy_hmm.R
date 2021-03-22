require(moveHMM)
require(ggplot2)

#### read monteiro file 
monteiro <- read.csv(file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_clean.csv")

### reorder dataframe to deal with issue of some observations not being contiguous
monteiro$date <- as.POSIXct(monteiro$date)
names(monteiro)[15] <- "ID"
monteiro <- monteiro[with(monteiro, order(ID, date)), ]

#### prep data for HMM 
monteiro_hmm <- prepData(monteiro, type="LL",coordNames=c("lon","lat"))
monteiro_hmm <- monteiro_hmm[!monteiro_hmm$step == 0,]
monteiro_hmm <- monteiro_hmm[!is.na(monteiro_hmm$angle),]
monteiro_hmm <- monteiro_hmm[!is.na(monteiro_hmm$step),]

plot(monteiro_hmm)

# Params
stepDist <- "gamma"
angleDist <- "vm"
nbStates <- 2

# Starting values for the step length parameters
stepMean0 <- c(1, 7) # initial means (one for each state)
stepSD0 <- c(1, 7) # initial standard deviations (one for each state)
stepPar0 <- c(stepMean0, stepSD0)

# Starting values for the turning angle parameters
angleMean0 <- c(0,0)
kappa0 <- c(1,1)
angleCon0 <- c(1,10)
anglePar0 <- c(kappa0)

m <- fitHMM(formula=~1,
            data=monteiro_hmm,nbStates=nbStates,stepPar0=stepPar0,
            anglePar0=anglePar0,stepDist=stepDist,
            angleDist=angleDist,angleMean=angleMean)

#### check range of starting values to assess numerical stability 
# Number of tries with different starting values
niter <- 25
# Save list of fitted models
allm <- list()
for(i in 1:niter) {
  print(i)
  # Step length mean
  stepMean0 <- runif(2,
                     min = c(0.5, 5),
                     max = c(20, 100))
  # Step length standard deviation
  stepSD0 <- runif(2,
                   min = c(0.5, 5),
                   max = c(20, 100))
  # Turning angle mean
  #angleMean0 <- c(pi, 0)
  # Turning angle concentration
  angleCon0 <- runif(2,
                     min = c(0.1, 1),
                     max = c(1, 50))
  # Fit model
  stepPar0 <- c(stepMean0, stepSD0)
  anglePar0 <- c(angleMean0, angleCon0)
  allm[[i]] <- fitHMM(data = monteiro_hmm, nbStates = 2, stepPar0 = stepPar0,
                      anglePar0 = anglePar0)
}

# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))
allnllk


#### extract behaviour state predictions at each time step
monteiro_hmm$state <- viterbi(m)

ggplot()+
  theme_void()+
  geom_point(data = monteiro_hmm, aes(x, y, colour = state), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()

ggplot()+
  theme_void()+
  geom_point(data = subset(monteiro_hmm, state == 1), aes(x, y, colour = bstage), size = 0.7, alpha = 0.6)+
  geom_sf(data = azores.map, fill = 'grey80')+
  geom_point(aes(x = -27.955, y = 39.056667), colour = "red", shape = 18, size = 4)+
  xlim(c(-40,-15)) +ylim(c(35, 50))+
  theme_classic()


write.csv(monteiro_hmm, file = "/Users/gemmacarroll/Google Drive/NOAA/Projects/Azores/Storm petrels/storm_petrels/monteiro_hmm.csv", row.names = F)
