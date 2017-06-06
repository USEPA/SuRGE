# SCRIPT USED TO ANALYZE GRTS SAMPLE OF NLA 2012 MAN MADE WATER BODIES.

# THIS SCRIPT SUPPOSES THAT THE DATA WILL LOOK LIKE THE DATA SET IN THIS EXAMPLE.
# A COUPLE THINGS TO NOTE:

# 1) THE NLA 2010 DATA SET IS ITSELF A SAMPLE OF WATER BODIES, 
# SO THE PROPORTION OF WATER BODIES THAT ARE RESERVOIRS IN THAT
# SAMPLE IS AN ESTIMATE OF THE TRUE PROPORTION -WE NEED TO ACCOUNT FOR THE
# UNCERTAINTY THERE.

# 2) EACH SAMPLED RESERVOIR WILL HAVE A GRTS DESIGN, WITH A RESULTING MEAN AND VARIANCE. 
# THESE VARIANCES NEED TO BE ACCOUNTED FOR IN A SENSIBLE WAY.


## Read in the GRTS sites selected
grtsRes <- readOGR("output/nrs.shp")

## Since there aren't results yet, let's make some up.
set.seed(12321)
# Methane for each sampled reservoir. This is lognormal, but associated with the AREA_HA column.
## The emission rates are similar in magnitude to the Beaulieu EST 2014 paper
## Rates should be in mg C m^(-2) d^(-1)
grtsRes$ch4Mn <- rlnorm(nrow(grtsRes), meanlog = log(150), sdlog = log(2.5))
# Reservoir-level GRTS variance
grtsRes$ch4Var <- (grtsRes$ch4Mn / 6)^2
# Make the 'oversample' results NA
grtsRes$ch4Mn[grtsRes$panel == "OverSamp"] <- NA
grtsRes$ch4Var[grtsRes$panel == "OverSamp"] <- NA



## Calculate the confidence interval for the proportion of lakes that are 
## reservoirs. This is the number of reservoirs in the NLA 2012 survey,
## divided by the number of total unique water bodies in the NLA 2012 survey.
## There are 1,000 water bodies identified, but some of these are repeat visits.
## We only know about the repeat visits at the reservoirs. These are subtracted
## from the sample size of 1000.

## Look at the surface area for each water body
## Grab each of the 9 'polygon' slots, grab the 'area' slot, and sum them up.
## I believe these are in m^2, since the projection string has units of meters.
wbAreasEco <- sapply(slot(ecoRegAlb, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))



#### Since this is a GRTS scheme within a GRTS scheme, we should account for the
#### model-based variance at each reservoir sampled. There aren't any developed
#### methods for doing this, but we can brute-force it. Assume normality at each 
#### reservoir, sample from the distribution of the mean methane emissions rate,
#### use the spsurvey total.est() function to get a total estimate. Repeat lots of times.
## Lots of up=front stuff to calculate
# Unique reservoirs from NLA 2012
nRes <- nrow(nla2012AlbUnique)
# Known unique water bodies in NLA 2012. This is a different number from the NLA report (1,038)
nWater <- length(unlist(wbAreasEco)) - (nrow(nla2012Alb) - nrow(nla2012AlbUnique)) 
# Confidence interval for proportion of water bodies that are reservoirs
resConf <- function(nr, nw, conf = 0.95){
  # nr is the number of reservoirs
  # nw is the total number of surveyed water bodies
  # alpha is confidence level
  p_hat <- nr / (nw)
  var_p_hat <- p_hat / (1-p_hat)
  p_conf <- p_hat + c(-1,1) * qt(1-(1-conf)/2, df = nw) * sqrt(var_p_hat / nw)
  return(c("Lwr" = p_conf[1], "Mean" = p_hat, "Upr" = p_conf[2]))
}
resPropCI <- resConf(nr = nRes,
                      nw = nWater,
                      conf = 0.95)
## Given this confidence interval, what is the implied standard deviation?
f <- function(x, conf = 0.95){
  # x is a vector of the confidence interval - lwr, mean, upr, and sd guess
  z <- (resPropCI[3] - qnorm(1-(1-conf)/2, mean = resPropCI[2], sd = x))^2
  return(z)
}
sdOptim <- optim(par = c(0.05), f)
sdProp <- sdOptim$par # Optimized 'sd' from the confidence interval above
# How many water bodies are there in the US? NLA 2012 page 5 says...
nWaterUSA <- 159652
# Subset the GRTS results
grtsResM <- subset(grtsRes, !is.na(ch4Mn))
strataTbl <- with(grtsResM@data, table(stratum))
# for loop stuff
nSamp <- 1000
pSims <- rnorm(nSamp, mean = pHat, sd = sdProp)
## Simulated methane emission rates
ch4MnsMat <- MASS::mvrnorm(n = 1000, mu = grtsResM$ch4Mn, Sigma = diag(grtsResM$ch4Var ))
methaneTots <- NULL
for(i in 1:nSamp){
  # i = 1
  # Need to scale up the grts design weights using the i'th simulated proportion
  wtMult <- ( pSims[i] * nWaterUSA ) / sum(grtsResM$wgt) 
  # The simulated means at each site * the area converted to square meters
  sampch4 <- ch4MnsMat[i,] * grtsResM$AREA_HA * 10000 
  overallMns <- total.est(z = sampch4, wgt = grtsResM$wgt * wtMult, 
                        x = grtsResM$xcoord, y = grtsResM$ycoord,
                        stratum = grtsResM$stratum,
                        vartype = "Local")
  methaneTots <- c(methaneTots, overallMns$Estimate[overallMns$Statistic == "Total"])
}

methaneTotDf <- data.frame("Total"=methaneTots)
## The methaneTots object is a vector of total methane emissions (not a rate)
## from reservoirs across the entire country. The units should be 
## mg C m^(-2) d^(-1). 
ggplot(methaneTotDf, aes(Total)) + geom_histogram(bins = 50) +
  xlab("CH4 Emissions [mg C m^(-2) d^(-1)]") + ylab("Frequency") + ggtitle("GRTS design-based estimate of CH4")

