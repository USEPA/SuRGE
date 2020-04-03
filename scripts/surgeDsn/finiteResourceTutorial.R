# GRTS Survey Designs for a Finite Resource
# https://cran.r-project.org/web/packages/spsurvey/vignettes/Finite_Design.pdf

# PRELIMINARIES------------------------
# Load the sp object in the data directory
 data(NE_lakes)

# Create a shapefile
 sp2shape(sp.obj=NE_lakes, shpfilename="NE_lakes")

# Read the attribute table from the shapefile
 att <- read.dbf("NE_lakes")
 
# Display the initial six lines in the attribute data frame
 head(att) 

# Display number of lakes cross-classified by strata and multidensity
# category
 addmargins(table("State"=att$State, "Lake Area Category"=att$Area_Cat))

 
 
# UNSTRATIFIED, EQUAL PROBABILITY, GRTS SURVEY DESIGN---------------------
# Call the set.seed function so that the survey designs can be replicate
  set.seed(4447864)

# Create the design list
  Equaldsgn <- list(None=list(panel=c(PanelOne=300), 
                              seltype="Equal"))

  Equalsites <- grts(design=Equaldsgn,
                         DesignID="EQUAL",
                         type.frame="finite",
                         src.frame="shapefile",
                         in.shape="NE_lakes",
                         att.frame=att,
                         shapefile=FALSE)


# Print the initial six lines of the survey design
  head(Equalsites@data)

# Print the survey design summary
  summary(Equalsites)
  
  

# STRATIFIED, EQUAL PROBABILITY, GRTS SURVEY DESIGN--------------
  # Create the design list
  Stratdsgn <- list(CT=list(panel=c(PanelOne=125), seltype="Equal"),
                    MA=list(panel=c(PanelOne=125), seltype="Equal"),
                    RI=list(panel=c(PanelOne=50), seltype="Equal"))
  
  # Select the sample
  Stratsites <- grts(design=Stratdsgn,
                     DesignID="STRATIFIED",
                     type.frame="finite",
                     src.frame="att.frame", # if df is used, could use shp
                     att.frame=att, # specify df
                     xcoord="xcoord", # specify x if df is used
                     ycoord="ycoord", # specify y is df is used
                     stratum="State",
                     shapefile=FALSE) 
  
  # Print the initial six lines of the survey design
  head(Stratsites@data)
  
  # Print the survey design summary
  summary(Stratsites)
  
# UNSTRATIFIED, UNEQUAL PROBABILITY, GRTS SURVEY DESIGN WITH AN OVERSAMPLE----
# Read the shapefile
shp <- read.shape("NE_lakes")
  
# Create the design list
Unequaldsgn <- list(None=list(panel=c(PanelOne=300),
                                seltype="Unequal",
                                caty.n=c("(0,1]"=50, "(1,5]"=120, "(5,10]"=50,
                                         "(10,50]"=50, "(50,500]"=25,
                                         "(500,1e+04]"=5),
                                over=120))
# Select the sample
  Unequalsites <- grts(design=Unequaldsgn,
                       DesignID="UNEQUAL",
                       type.frame="finite",
                       src.frame="sp.object", # could also be df or shp
                       sp.object=shp,
                       att.frame=att,
                       mdcaty="Area_Cat",
                       shapefile=FALSE)  
  
# Print the initial six lines of the survey design
  head(Unequalsites@data)
  
# Print the survey design summary
  summary(Unequalsites)

# UNSTRATIFIED, UNEQUAL PROBABILITY, GRTS SURVEY DESIGN WITH AN OVERSAMPLE----- 
# AND A PANEL STRUCTURE FOR SURVEY OVER TIME

# Create the design list  
  Paneldsgn <- list(None=list(panel=c(Annual=50, Year1=50, Year2=50, Year3=50,
                                       Year4=50, Year5=50),
                               seltype="Unequal",
                               caty.n=c("(0,1]"=50, "(1,5]"=120, "(5,10]"=50,
                                          "(10,50]"=50, "(50,500]"=25,
                                          "(500,1e+04]"=5),
                               over=100))  
  
# Select the sample
 Panelsites <- grts(design=Paneldsgn,
                        DesignID="UNEQUAL",
                        type.frame="finite",
                        src.frame="shapefile",
                        in.shape="NE_lakes",
                        att.frame=att,
                        mdcaty="Area_Cat",
                        shapefile=FALSE)  
  
 
# Print the survey design summary
  summary(Panelsites)
  
  
  
  
  
  