#### Small toy example to get previous weights adjusted

## Define area.
xLim <- yLim <- c(0,50)

## Put points in the area
set.seed(54321)
n <- 500
allPts <- data.frame("x" = runif(n, 0, max(xLim)),
                     "y" = runif(n, 0, max(yLim)))

## Define strata by the two lines y = x + 10 and y = x - 15
allPts$yMinusx <- with(allPts, y - x)
allPts$region <- ifelse(allPts$yMinusx >= 10, "High",
                        ifelse(allPts$yMinusx < 10 & allPts$yMinusx >= -15,
                               "Middle","Low"))
ggplot(allPts, aes(x, y)) + geom_point(aes(colour = region)) +
  geom_abline(intercept = 10, slope = 1) + geom_abline(intercept = -15, slope = 1)

## Add reservoir/lake indicator
allPts$type <- "lake"
allPts$type[sample(1:n, size = 2/3*n, replace = FALSE)] <- "reservoir"

## Add acreage size for water bodies
allPts$size <- rlnorm(n, meanlog = log(15), sdlog = log(5))
# Fix anything less than 2 acre to be 2
allPts$size[allPts$size <= 2] <- 2

## Log-size might make more sense for weighting
allPts$logSize <- log(allPts$size)

## Columns for methane emissions per water body
allPts$ch4 <- sqrt(allPts$logSize)

## Make data set that is a sub-sample 
dat <- allPts[sample(x = 1:nrow(allPts), size = n/2),]

## Make both 'dat' and 'allPts' sp objects
coordinates(dat) = ~x+y
coordinates(allPts) = ~x+y

#### GRTS Schemes
## Sample 1 - stratified and equal probability
# Design list
dsgnList1 <- list(High = list(panel=c(PanelOne=15), seltype="Equal"),
                  Middle = list(panel=c(PanelOne=15), seltype="Equal"),
                  Low = list(panel=c(PanelOne=15), seltype="Equal"))
set.seed(6060)
dat1 <- subset(dat, type == "reservoir")
sites1 <- grts(design=dsgnList1,
               DesignID="STRATIFIED",
               type.frame="finite",
               src.frame="sp.object", 
               sp.object = dat1,
               att.frame=NULL,
               stratum="region")

print(sites1) # 45 sites, 15 per ecoregion

## Interesting tidbits here.
sum(sites1$wgt) # 166
# This is the number of 'reservoirs' in the sub-sampled sampling frame - dat1
# This is intentional in GRTS methodology. It also means the total estimate
# will be an inference on the sub-sampled sampling frame, not the larger 'true'
# population. See below.
dat1Tbl <- with(dat1@data, table(type,region))
total1Sub <- total.est(z = sites1$ch4, wgt = sites1$wgt, 
                      x = sites1$xcoord, y = sites1$ycoord,
                      stratum = sites1$stratum, vartype = "Local",
                      popsize = c("High"=dat1Tbl[1,1],
                                  "Middle" = dat1Tbl[1,3],
                                  "Low" = dat1Tbl[1,2]))
total1Sub # total = 267
sum(dat1$ch4) # 271, Pretty dang good! But it's the wrong level of the hierarchy.
# What happens if we change the population size?
allPtsTbl <- with(allPts@data, table(type,region))
total1Res <- total.est(z = sites1$ch4, wgt = sites1$wgt, 
                       x = sites1$xcoord, y = sites1$ycoord,
                       stratum = sites1$stratum,
                       vartype = "Local", 
                       popsize = c("High"=allPtsTbl[2,1], "Middle" = allPtsTbl[2,3],
                                   "Low" = allPtsTbl[2,2]))
total1Res # Nothing changed. Weird.
sum(subset(allPts@data, type == "reservoir")$ch4) 
# True population total emissions rate is much higher.
# What if we scale the weights manually so they add up to the number
# of known reservoirs in our world?
# A little bit of pseudo algebra:
# sum_i(x_i) = k
# Here, x_i are weights, and k is the current sum of the weights
# What constant do I multiply by to make sure that the sum(x_i) adds up to k + z instead?
# c*sum_i(x_i) = k + z
# Substitute the sum_i(x_i) for k
# c*k = k + z
# c = (k+z) / k
# So the constant we need is the total number of reservoirs in our world divided by the
# number of sub-sampled reservoirs in our sampling frame.
# i.e., 333 / 166
wtMult <- 333/166
total1Res <- total.est(z = sites1$ch4, wgt = sites1$wgt *wtMult, 
                       x = sites1$xcoord, y = sites1$ycoord,
                       stratum = sites1$stratum,
                       vartype = "Local", 
                       popsize = c("High"=allPtsTbl[2,1], "Middle" = allPtsTbl[2,3],
                                   "Low" = allPtsTbl[2,2]))
total1Res
sum(subset(allPts@data, type == "reservoir")$ch4)
# Less of a difference this time. Probably because the equal probability scheme
# missed some of the larger reservoirs. 


## Sample 2 - stratified and unequal probability
set.seed(3333)
dat2 <- subset(dat, type == "reservoir") # same as dat1, but that's OK. Keep naming convention.
# Cut the logSize into factor levels for the weighting.
# This probably needs tweaking based on knowledge of how ch4 varies with size.
brks <- c(0,1:4,9)
dat2$logSizeCat <- cut(dat2$logSize, breaks = brks)
# One weird part is that you have to specify the target sample sizes for the 
# 'multi density categories' - spsurvey's verbage for the binned categories
# of the unequal probability column. See page 12 of
# https://cran.r-project.org/web/packages/spsurvey/vignettes/Finite_Design.pdf
# This is specified in the 'caty.n' list item for each stratum in the design list.
dsgnList2 <- list("High" = list(panel=c(PanelOne=10), seltype="Unequal",
                                caty.n = c("(0,1]" = 1, "(1,2]" = 1, 
                                           "(2,3]" = 2, "(3,4]" = 2,
                                           "(4,9]" = 4)),
                  "Middle" = list(panel=c(PanelOne=10), seltype="Unequal",
                                  caty.n = c("(0,1]" = 1, "(1,2]" = 1, 
                                             "(2,3]" = 2, "(3,4]" = 2,
                                             "(4,9]" = 4)),
                  "Low" = list(panel=c(PanelOne=10), seltype="Unequal",
                               caty.n = c("(0,1]" = 1, "(1,2]" = 1, 
                                          "(2,3]" = 2, "(3,4]" = 2,
                                          "(4,9]" = 4)))
sites2 <- grts(design=dsgnList2,
               DesignID="STRATIFIED",
               type.frame="finite",
               src.frame="sp.object",
               sp.object = dat2,
               att.frame=dat2@data,
               stratum="region",
               mdcaty = "logSizeCat")
# See if the unequal probability weighting makes the total ch4 estimate
# closer to the truth
wtMult <- 333/sum(sites2$wgt)
total2Res <- total.est(z = sites2$ch4, wgt = sites2$wgt *wtMult, 
                       x = sites2$xcoord, y = sites2$ycoord,
                       stratum = sites2$stratum,
                       vartype = "Local", 
                       popsize = c("High"=allPtsTbl[2,1], "Middle" = allPtsTbl[2,3],
                                   "Low" = allPtsTbl[2,2]))
total2Res
sum(subset(as.data.frame(allPts@data), type == "reservoir")$ch4)
# still pretty good

# Sample 3 - stratified, unequal probability, continuous probability
dsgnList3 <- list("High" = list(panel=c(PanelOne=10), seltype="Continuous"),
                  "Middle" = list(panel=c(PanelOne=10), seltype="Continuous"),
                  "Low" = list(panel=c(PanelOne=10), seltype="Continuous"))
sites3 <- grts(design=dsgnList3,
               DesignID="STRATIFIED",
               type.frame="finite",
               src.frame="sp.object",
               sp.object = dat2,
               att.frame=dat2@data,
               stratum="region",
               mdcaty = "logSize")
# See how well it estimates the total ch4 emissions
wtMult <- 333/sum(sites3$wgt)
total3Res <- total.est(z = sites3$ch4, wgt = sites3$wgt *wtMult, 
                       x = sites3$xcoord, y = sites3$ycoord,
                       stratum = sites3$stratum,
                       vartype = "Local", 
                       popsize = c("High"=allPtsTbl[2,1], "Middle" = allPtsTbl[2,3],
                                   "Low" = allPtsTbl[2,2]))
total3Res
sum(subset(as.data.frame(allPts@data), type == "reservoir")$ch4)

