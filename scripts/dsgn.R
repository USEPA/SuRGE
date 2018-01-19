# SCRIPT USED TO DESIGN INTENSIVE SAMPLING OF A SUBSET OF 
# NLA 2012 MAN MADE WATER BODIES.


################################################################
# AN OUTSTANDING ISSUE IS OVERLAP IN COLUMN NAMES BETWEEN      #
# NLA2012 DATA AND FIELD CREATED BY grts BELOW.  NEW FIELDS    #
# ARE LOWERCASE WHEREAS ORIGINAL FIELD ARE UPPERCASE.  THIS    #
# WILL SUFFICE FOR NOW, BUT NEED TO RECONCILE MOVING FORWARD   #
################################################################

# STRATIFIED, EQUAL PROBABILITY, OVERSAMPLE, GRTS SURVEY DESIGN--------------
# EXTRACT ATTRIBUTE TABLE -----------  
# nla2012Alb is a spatialPointsDataFrame created in mapNla2012.R
# This has non-unique records, because some sites were visited twice.
nla2012AlbUnique <- subset(nla2012Alb, !duplicated(SITE_ID))

#attRes <- as.data.frame(nla2012Alb@data)



# Create the design list
stratDsgn <- list(CPL = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #50
                  NAP = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #30
                  NPL = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #25
                  SAP = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #50
                  SPL = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #50
                  TPL = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #50
                  UMW = list(panel=c(PanelOne=7), seltype="Equal", over = 5), #5
                  WMT = list(panel=c(PanelOne=7), seltype="Equal", over = 10), #50
                  XER = list(panel=c(PanelOne=7), seltype="Equal", over = 10)) #50

# Select the sample
set.seed(4447864) # allows design to be replicated

nrsSites <- grts(design=stratDsgn,
                 DesignID="STRATIFIED",
                 type.frame="finite",
                 src.frame="sp.object", # if df is used, could use shp
                 sp.object = nla2012AlbUnique,
                 att.frame=NULL,
                 stratum="FW_ECO9",
                 out.shape = "output/nrs") 

# Print the initial six lines of the survey design
head(nrsSites@data)

# Print the survey design summary
summary(nrsSites)

plotIt <- TRUE # Can use FALSE if having problem w/ plotting.
               # Plotting not working on Will's machine...
if(plotIt){
  # Plot survey design
  ggplot() + 
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Coastal Plains"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Northern Appalachians"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Northern Plains"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Southern Appalachians"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Southern Plains"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Upper Midwest"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Xeric"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Western Mountains"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Temperate Plains"),
                 aes(long,lat,group=group,fill=WSA9_NAME)) +
    geom_polygon(data=statesAlb.df, aes(x=long, y=lat, group = group),
                 colour="cornsilk3", fill=NA, size = 0.1 ) + 
    scale_fill_manual("Ecoregion", values = cols) +
    geom_point(data = filter(data.frame(nrsSites), panel != "OverSamp"),
               aes(xcoord, ycoord), # columns created by grts
               size = 1)  +
    ggtitle("NRS Main Sites") +
    coord_equal()
  
  # Optional
  ggsave(filename="output/figures/nrsMainSites.tiff",
         width=8,height=5.5, units="in",
         dpi=800,compression="lzw")
}

