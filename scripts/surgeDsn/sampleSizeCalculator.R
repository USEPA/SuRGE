## Since there aren't results yet, let's make some up.
set.seed(12321)
## Methane for each sampled reservoir. 
## Per the GitHub Wiki entry by Jake, 34 reservoirs were sampled.
## The mean and standard deviation of the measured emission rates was 6.3 and
## 5.84 mg CH4 m-2 h-1, respectively. 
## Rates should be in mg C m^(-2) d^(-1), so we'll need to convert.
## m^(-2) h(-1) * [(24 h) / (1 day)]
mnMethaneRate = 6.3 * 24 ## See above conversion 
sdMethaneRate = 5.84 * 24

## Reservoir-level simulated means
## Simulate from a lognormal since mean and sd are relatively close.
## The arithmetic mean and standard deviation are related to the lognormal
## parameters 'mu' and 'sigma' via the formulas:
## mu = log( E[X]^2 / sqrt( Var[X] + E[X]^2) )
## sigma^2 = log( 1 + Var[X] / E[X]^2 )
mu <- log(mnMethaneRate^2 / sqrt(sdMethaneRate^2 + mnMethaneRate^2))
sigma <- sqrt(log(1 + sdMethaneRate^2 / mnMethaneRate^2))


# bootstrap confidence intervals for different sample sizes
# http://www.stat.ucla.edu/~rgould/110as02/bsci

ch4Mn = rlnorm(n = 110, meanlog = mu, sdlog = sigma)

bstrap.10 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 10, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.10 <- c(bstrap.10, bestimate)}


bstrap.20 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 20, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.20 <- c(bstrap.20, bestimate)}


bstrap.30 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 30, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.30 <- c(bstrap.30, bestimate)}

bstrap.40 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 40, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.40 <- c(bstrap.40, bestimate)}

bstrap.50 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 50, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.50 <- c(bstrap.50, bestimate)}

bstrap.60 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 60, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.60 <- c(bstrap.60, bestimate)}

bstrap.70 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 70, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.70 <- c(bstrap.70, bestimate)}

bstrap.80 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 80, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.80 <- c(bstrap.80, bestimate)}

bstrap.90 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 90, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.90 <- c(bstrap.90, bestimate)}

bstrap.100 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 100, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.100 <- c(bstrap.100, bestimate)}


bstrap.110 <- c()
for (i in 1:1000){
  # take sample
  bsample <- sample(ch4Mn, 100, replace = T)
  # calculate bootstrap estimate
  bestimate <- mean(bsample)
  bstrap.110 <- c(bstrap.110, bestimate)}

bstrap <- list(bstrap.10, bstrap.20, bstrap.30, bstrap.40, bstrap.50,
               bstrap.60, bstrap.70, bstrap.80, bstrap.90, bstrap.100, bstrap.110)

interval <- data.frame(n = seq(10, 110, 10),
                       upper = lapply(bstrap, function(x) quantile(x, 0.95)) %>% unlist(),
                       lower = lapply(bstrap, function(x) quantile(x, 0.05)) %>% unlist(),
                       mn = lapply(bstrap, function(x) mean(x)) %>% unlist())

ggplot(interval, aes(n, upper)) + geom_point()

ggplot(interval, aes(n, lower)) + geom_point()

ggplot(interval, aes(n, ((upper - lower)/mn) * 100)) + geom_point()

