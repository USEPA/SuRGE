# Stratification Index 
# 10/20/2024
# Going to try with RLakeAnalyzer

sites <- depth_profiles_all %>%
  distinct(lake_id, visit)

buoyf <- NULL

for (i in 1:nrow(sites)) {
  prol <- filter(depth_profiles_all, lake_id == sites$lake_id[i], visit == sites$visit[i])
  wtr <- as.vector(prol$temp)
  depths <- as.vector(prol$sample_depth)
  buoyf[i] <- center.buoyancy(wtr, depths)
}

thermdep <- NULL

for (i in 1:nrow(sites)) {
  prol <- filter(depth_profiles_all, lake_id == sites$lake_id[i], visit == sites$visit[i])
  temp <- c(prol$temp)
  dep <- c(prol$sample_depth)
  thermdep[i] <- thermo.depth(temp, dep, Smin = 0.1, )
}

#Now make thermocline depth zero for NA values 

thermdep2<-ifelse(is.na(thermdep),0,thermdep)

#Make a link for stratification indices
#I didn't include a depth to hypoxia since we have oxygen data at
#each site where we have methane but we can revisit this depending
#on the modeling approach we land on

strat_link <- data.frame(sites$lake_id, sites$visit, buoyf, thermdep)
colnames(strat_link)<-c("lake_id","visit","buoyf","thermdep2")

# For the 2020 JGR paper, I used the following script to calculate stratification
# indices. My notes indicate that I started by working with rLakeAnalyzer but
# pivoted to these custom functions.

# # CALCULATE DEPTH TO THERMOCLINE--------------
# # Calculate water temp rate of change
# tempDo <- 
#   group_by(tempDo, lake_name) %>%
#   mutate(wtrChange = (wtr - lag(wtr, 1)) / (depth.m - lag(depth.m, 1)))
# 
# 
# # Function to calculate thermocline depth.
# # Interpolates between measurement depths
# meanThermoDepth <- function(df) {
#   minCriteria <-  min(df$wtrChange, na.rm = TRUE)  # rate of change must be >= 1C/m
#   if (minCriteria > -1) {maxDepth <- NA}
#   if (minCriteria > -1) {minDepth <- NA}  
#   if (minCriteria <= -1) {maxDepth <- df[which.min(df$wtrChange), "depth.m"]}
#   if (minCriteria <= -1) {minDepth <- df[which.min(df$wtrChange) - 1, "depth.m"]}
#   (minDepth + maxDepth) / 2
# }
# 
# # Calculate thermocline depth
# thermoDepth <- ddply(tempDo, .(lake_name), function(df) meanThermoDepth(df)) %>%
#   rename(thermoDepth.m = V1)
# 
# # Print depth profile
# pdf(file = 'ohio2016/output/figures/tempProfile.pdf')
# 
# for(i in 1:length(unique(tempDo$lake_name))) {
#   reservoir.i = unique(tempDo$lake_name)[i]
#   
#   try(print(
#     ggplot(tempDo[tempDo$lake_name == reservoir.i, ], aes(x=wtr, y=depth.m)) +   #aes_string helpful in for loop (http://stackoverflow.com/questions/13260626/selecting-data-frame-columns-to-plot-in-ggplot2)
#       ggtitle(label = reservoir.i) +
#       geom_point() +
#       geom_hline(yintercept = thermoDepth[thermoDepth$lake_name == reservoir.i, "thermoDepth.m"]) +
#       scale_y_reverse(), 
#     silent=TRUE))
# }
# dev.off()
# 
# 
# # CALCULATE DEPTH TO HYPOXIA (<10% DO)------------
# hypoxicDepth <- filter(tempDo, row_number() == min(which(doobs < 10))) %>%
#   select(lake_name, depth.m, doobs) %>%
#   rename(hypoxicDepth.m = depth.m)
# 
# # Print do profile
# pdf(file = 'ohio2016/output/figures/doProfile.pdf')
# 
# for(i in 1:length(unique(tempDo$lake_name))) {
#   reservoir.i = unique(tempDo$lake_name)[i]
#   
#   try(print(
#     ggplot(tempDo[tempDo$lake_name == reservoir.i, ], aes(x=doobs, y=depth.m)) +   #aes_string helpful in for loop (http://stackoverflow.com/questions/13260626/selecting-data-frame-columns-to-plot-in-ggplot2)
#       ggtitle(label = reservoir.i) +
#       geom_point() +
#       geom_hline(yintercept = hypoxicDepth[hypoxicDepth$lake_name == reservoir.i, "hypoxicDepth.m"]$hypoxicDepth.m) +
#       scale_y_reverse(), 
#     silent=TRUE))
# }
# dev.off()
# 
# # MERGE THERMOCLINE AND HYPOXIA DEPTHS
# # Merge depth to hypoxia and thermocline
# hypoxicThermoDepths <- 
#   merge(select(hypoxicDepth, -doobs), thermoDepth, all = TRUE)
# 
# write.table(x = hypoxicThermoDepths, 
#             file = "ohio2016/output/hypoxicAnoxicDepths.txt", 
#             row.names = FALSE)