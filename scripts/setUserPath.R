# Identify local path to synced SharePoint site for each user.  This path
# is above the path for the Rproject and allows for reading of data higher
# in the directory tree.

userPath <- paste0(Sys.getenv("USERPROFILE"), 
                   "/Environmental Protection Agency (EPA)/SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/")

# Switch to this definition after all data have been moved to github repo
# userPath <- ""
