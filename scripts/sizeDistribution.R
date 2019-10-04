# IS THERE A DIFFERENCE IN THE SIZE DISTRIBUTION OF LAKES
# VS RESERVOIRS?  ONLY 2007 NLA DATA AVAILABLE ON LINE,
# CAN PROBABLY GET MORE INFO FROM CORVALLIS IF NECESSARY.

info <- read.csv("inputData/nla2007/NLA2007_SampledLakeInformation_20091113.csv",
                 stringsAsFactors = FALSE)

ggplot(info, aes(x = log(LAKEAREA), color = LAKE_ORIGIN)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity") +
  ggtitle("Size distribution of man-made and natural waterbodies in NLA 2007")


# It looks like large waterbodies are more likely to be reservoirs.
# I'm not clear on implications of this.  If we subset this population
# of reservoirs, will our sample have an artifically high number
# of large reservoirs?