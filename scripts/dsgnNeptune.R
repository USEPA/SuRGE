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
# nla2012 is a spatialPointsDataFrame created in readSpatial.R
# This has non-unique records, because some sites were visited twice.
nla2012Unique <- subset(nla2012, !duplicated(SITE_ID))

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
                 src.frame="sf.object", # if df is used, could use shp
                 sf.object = nla2012Unique,
                 att.frame=NULL,
                 stratum="FW_ECO9"
                 #column names get chopped off in written shapefile
                 #better to create sf object from nrsSites (below)
                 #out.shape = "output/nrs" 
                 ) 

# Print the initial six lines of the survey design
head(nrsSites@data)

# Print the survey design summary
summary(nrsSites)

# Conver to sf object for subsequent processing/mapping
# nrs <- st_read(dsn = "C:/Users/JBEAULIE/GitRepository/NRS/output",
#                       layer = "nrs", crs = 5070)

nrs <- st_as_sf(as.data.frame(nrsSites), 
                coords = c("xcoord", "ycoord"), 
                crs = 5070)  # inherited CONUS Albers from nla2012Unique

st_crs(nrs) # 5070


