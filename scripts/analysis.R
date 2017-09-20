#### Script to analyze hypothetical GRTS-based results from reservoir emissions study


## Read in the GRTS sites selected
grtsRes = readOGR("output/nrs.shp")

## Since there aren't results yet, let's make some up.
set.seed(12321)
## Methane for each sampled reservoir. 
## Per the GitHub Wiki entry by Jake, 32 reservoirs were sampled.
## The mean and standard error of the measured emission rates was 6.3 and
## 1.0 mg CH4 m-2 h-1, respectively. 
## Rates should be in mg C m^(-2) d^(-1), so we'll need to convert.
## m^(-2) h(-1) * [(24 h) / (1 day)]
mnMethaneRate = 6.3 * 24 ## See above conversion
sdMethaneRate = 1.0 * 24
## Reservoir-level simulated means
grtsRes$ch4Mn = rnorm(nrow(grtsRes), mean = mnMethaneRate, sd = sdMethaneRate)
# Reservoir-level GRTS variance
grtsRes$ch4Var = (grtsRes$ch4Mn / runif(nrow(grtsRes),3,6))^2
# Make the 'oversample' results NA
grtsRes$ch4Mn[grtsRes$panel == "OverSamp"] = NA
grtsRes$ch4Var[grtsRes$panel == "OverSamp"] = NA




######## Notes ########
#### There are a few ways to calculate total methane emissions. Two are included below.
#### First method - calculate a total at each reservoir, and scale up.
#### 1) Generate random emission draws for each reservoir sampled, assuming normality at the site.
#### 2) Use these new 'data', multiply by the known size of each reservoir, to get 
#### total emissions at sampled reservoirs (sum of random emission rate * reservoir size at each site)
#### 3) Generate random proportion of water bodies across the country that are man-made, and
#### scale up the totals using total.est(). This requires supplying new design weights, which are just
#### scaled versions of the existing design weights. They are scaled to account for the fact that our
#### sampling frame is itself a sample of the larger population of water bodies in the USA.
#### 4) Add up the totals, and separate by eco-region if necessary.

#### Second method - calculate mean emission rate in eco-region, and multiply by total surface area.
#### 1) Generate random emission draws for each reservoir sampled, assuming normality at the site.
#### 2) Use these new 'data', compute the within-stratum mean methane emission rates
#### 3) Generate random within-stratum reservoir surface area total estimates, using Karen's externally produced file.
#### 4) Multiply the within-stratum means by the within-stratum reservoir surface area totals, to get totals
#### for each stratum.
#### 5) Add up the totals




## Upfront stuff common to both methods
## Subset the GRTS results to only include sampled sites
grtsResM = subset(grtsRes, !is.na(ch4Mn))
strataTbl = with(grtsResM@data, table(stratum)) # Check numbers
## Number of simulations
nSamp = 1000
## Simulated methane emission rates
## This function produces nSamp rows of emission rate estimates, where each row is a draw
## from the 63 reservoirs sampled.
ch4MnsMat = MASS::mvrnorm(n = nSamp, mu = grtsResM$ch4Mn, Sigma = diag(grtsResM$ch4Var ))

## Choose which method you want
calcMethod = "Second"
# calcMethod = "First"

if(calcMethod == "First"){
  ######## Notes ########
  #### There are multiple layers of uncertainty here. 
  
  #### First level of uncertainty
  #### We don't know the proportion of water-bodies that are man-made reservoirs.
  #### The NLA 2012 survey contains both lakes and reservoirs, with repeat visits
  #### to some sites. So calculating that proportion, and the error in that proportion,
  #### requires a little bit of care. We only know about the repeat visits at the 
  #### reservoirs, since we have a file with the NLA 2012 results for all
  #### water-bodies classified as reservoirs.
  
  #### Second level of uncertainty
  #### Since this is a GRTS scheme within a GRTS scheme, we should account for the
  #### design-based uncertainty at each reservoir sampled. There aren't any developed
  #### methods for doing this, but we can brute-force it. Assume normality at each 
  #### reservoir, draw a value from the assumed distribution of the mean methane 
  #### emission rate at each one, treat those as the 'data' from the samples,
  #### and scale up accordingly.
  
  ######## End Notes ########

  ## Get a list of the water bodies in the ecoRegAlb object. Grab something unique
  ## from each polygon. The 'area' slot might be useful later, though we don't particularly
  ## care about it at the moment.
  wbAreasEco = sapply(slot(ecoRegAlb, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
  
  ## Unique reservoirs from NLA 2012
  nRes = nrow(nla2012AlbUnique)
  
  ## Known unique water bodies in NLA 2012. The NLA report puts this number at 1,038.
  ## We try to calculate it below, but get a different number. The difference isn't huge, 
  ## but needs to be looked at. It could be that the ecoRegAlb object, which comes from a
  ## GIS shapefile originally,contains both sites visited and sites that couldn't be accessed 
  ## or otherwise didn't meet the criteria. So there are two ways to find the unique water bodies number.
  ## The first calculation below is the the number of water bodies in wbAreasEco, minus the known 
  ## overcount in the reservoirs.
  nWater = length(unlist(wbAreasEco)) - (nrow(nla2012Alb) - nrow(nla2012AlbUnique)) 
  ## This calculation is just the number from the NLA report. This one might be safer.
  nWater = 1038
  
  ## Sample proportion and SE
  pHat = nRes / nWater
  sdProp = sqrt ( pHat * (1 - pHat) ) / sqrt(nWater) # This is just the sd of the data / sqrt(n)
  
  ## Total number of water bodies - from NLA 2012 report
  nWaterUSA = 159652
  
  ## Simulated proportion of water bodies that are man-made reservoirs.
  pSims = rnorm(nSamp, mean = pHat, sd = sdProp)
  methaneTots = NULL
  for(i in 1:nSamp){
    # i = 1
    # Need to scale up the grts design weights using the i'th simulated proportion
    wtMult = ( pSims[i] * nWaterUSA ) / sum(grtsResM$wgt) 
    # The simulated means at each site * the area converted to square meters
    sampch4 = ch4MnsMat[i,] * grtsResM$AREA_HA * 10000
    overallMns = total.est(z = sampch4, wgt = grtsResM$wgt * wtMult,
                            x = grtsResM$xcoord, y = grtsResM$ycoord,
                            stratum = grtsResM$stratum,
                            vartype = "Local")
    methaneTots = c(methaneTots, overallMns$Estimate[overallMns$Statistic == "Total"])
  }
  
  methaneTotDf1 = data.frame("Total"=methaneTots)
  ## The methaneTots object is a vector of total methane emissions (not a rate)
  ## from reservoirs across the entire country. The units should be 
  ## mg C d^(-1). 
}


##
if(calcMethod == "Second"){
  #### There are multiple layers of uncertainty here. 
  
  #### First level of uncertainty
  #### Since this is a GRTS scheme within a GRTS scheme, we should account for the
  #### design-based uncertainty at each reservoir sampled. There aren't any developed
  #### methods for doing this, but we can brute-force it. Assume normality at each 
  #### reservoir, draw a value from the assumed distribution of the mean methane 
  #### emission rate at each one, treat those as the 'data' from the samples,
  #### and scale up accordingly.
  
  #### Second level of uncertainty
  #### The total surface area of reservoirs in each stratum is estimated from the NLA folks
  #### who put together the initial sampling frame. Those estimates have uncertainity in them,
  #### which we treat as normal and simulate.
  
  ######## End Notes ########
  
  ## Read in the stratified surface area totals
  arealEst <- read.csv("inputData/nla2012/NLA12_Extent_Areal_Estimates_Target_20170809.csv",
                       stringsAsFactors = FALSE)
  ## The Category column needs to be set to 'Total' because the scaling up for surface area presumably is done on 
  ## the entire eco-region, not just the sites sampled. The Indicator column needs to be subset to avoid duplicate entries.
  ## We arbitrarily chose Indicator == "Evaluation_Status" instead of "Target_nonTarget"
  arealEcoReg <- subset(arealEst, Type == "WSA9_by_Lake_Origin" & grepl("Man_Made", arealEst$Subpopulation) &
                          Category == "Total" & Indicator == "Evaluation_Status")
  
  ## Assign a stratum designation
  arealEcoReg$stratum = substr(arealEcoReg$Subpopulation, 1,3)
  
  ## Generate random eco-region size reservoir surface area estimates
  ecoSAEst = MASS::mvrnorm(nSamp, mu = arealEcoReg$Estimate.U, Sigma = diag(arealEcoReg$StdError.U^2))
  
  ## The mvrnorm call above will allow for negative surface area estimates becasue the normal distribution does that.
  ## If there are any 0's, we need to get rid of them because negative surface area is unrealistic.
  ## Get rid of 0's
  for(i in 1:ncol(ecoSAEst)){
    # i = 1
    zeroInd = which(ecoSAEst[,i] <= 0)
    ## Choose the median for replacement because it's probably reasonable.
    ecoSAEst[zeroInd,i] = median(ecoSAEst[,i])
  }
  
  ## I believe that the eco-region total surface area estimates are in hectares, so convert to m^2
  ecoSAEst = ecoSAEst * 10000
  
  
  ## For loop to get mean emission rates by stratum, and total surface area by stratum
  ## Storage data frame
  methaneTotDf2 = data.frame("CPL" = as.numeric(NULL),
                             "NAP" = as.numeric(NULL),
                             "NPL" = as.numeric(NULL),
                             "SAP" = as.numeric(NULL),
                             "SPL" = as.numeric(NULL),
                             "TPL" = as.numeric(NULL),
                             "UMW" = as.numeric(NULL),
                             "WMT" = as.numeric(NULL),
                             "XER" = as.numeric(NULL))
  for(i in 1:nSamp){
    # i = 1
    # Aggregate the simulated means by eco-region, calclulate mean
    # The arithmetic mean for each stratum is an unbiased estimate. There is error
    # in that strata means, which we could account for using the local neighborhood
    # variance estimator. But that assumes a spatial structure among 7 sites,
    # which is probably unreasonable. So we ignore it for now.
    tmpMethMns = aggregate(ch4MnsMat[i,] ~ grtsResM$stratum, FUN = mean)
    names(tmpMethMns) = c("stratum", "ch4Mn")
    # Add on the estimated total surface area by eco-region
    tmpMethMns$sa = ecoSAEst[i,]
    methaneTotDf2[i,] = tmpMethMns$ch4Mn * tmpMethMns$sa
  }
  
  methaneTotDf2$Total = apply(methaneTotDf2, 1, sum)
  
}

## Plot results
ggplot(methaneTotDf2, aes(Total)) + geom_histogram(bins = 50) +
  xlab("CH4 Emissions [mg C d^(-1)]") + ylab("Frequency") + ggtitle("GRTS design-based estimate of CH4")

