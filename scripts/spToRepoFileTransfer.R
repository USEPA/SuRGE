# COPY GEODATABASES------------------------
# ventor of .gbd to copy from SP to repo
# see lines 9-57 of writeSuRGElakesToGpkg.R for how this object was created
# userPath was set to SharePoint when code was run to generate this object
needed.gdb 


# 1. MIRROR DIRECTORY STRUCTURE  
# Bridget already recreated most of the directory structure, but we need to add
# the folders that hold the collection of files that constitue each .gdb. The 
# .gdb "file" referenced in needed.gdb is actually just a folder. Must create
# this folder in repo before we copy files to it.

# vector of old directory paths
old.dir.path <- needed.gdb

# The paths for the new folders are
new.dir.path <- gsub(
  # replace the sharepoint directory
  pattern = paste0("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/",
                   "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/"), 
  # with the repo directory
  replacement = userPath, 
  x = old.dir.path, # target
  fixed = TRUE) # needed to accomodate () in path

# create the .gdb folders in repo
dir_create(new.dir.path)

# 2. GEODATABASE FILES
# Need vector of files that constitue each .gdb to be copied to repo
old.file.path <- dir_ls(path = needed.gdb, type = "file") # get files that constitute the .gdb

# file path for destination
new.file.path <- gsub(
  # replace the sharepoint directory
  pattern = paste0("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/",
                   "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/"), 
  # with the repo directory
  replacement = userPath, 
  x = old.file.path, # target
  fixed = TRUE) # needed to accomodate () in path

file_copy(old.file.path, new.file.path)

# 3. CHECK FILE SIZES FOR COMMIT TO REPO
# none exceed 50MB
file_info(new.file.path) %>%
  select(path, size) %>% 
  filter(size > fs_bytes("50MB"))

# COPY 2016 SHAPEFILES-------------

# 1. CREATE VECTOR OF ALL FILES THAT CONSTITUTE A SHAPEFILE-------------------------
# Files are currently present in SharePoint
paths <- paste0(paste0("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/", #temp to collect files
                       "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/"),
                "lakeDsn/", "2016_survey")

old.shp.paths <- fs::dir_ls(path = paths, 
                            # all files that constitute shapefile
                            regexp = c(".shp|.sbx|.dbf|.prj|.sbn|.shx"), # file names containing this pattern
                            recurse = 1, # one level into subdirectories (avoid bathymetry files)
                            type = "file") %>% # only retain file names, not directory names  
  .[!(grepl("xml|lock|basin", .))] %>% # omit .shp, .xml, .lock, and basin shapefiles
  # I couldn't dissolve intra-reservoir boundaries for these lakes in R. dissolved in Pro. Ignore original .shp
  # and read in dissolved polygons. Specified lake name below.
  .[!(grepl("fallsLakeSitesEqArea|fallsLakeEqArea.shp|miltonEqArea.shp|senecavilleEqArea.shp|caesarCreekEqArea.shp", .))]


# 2. MIRROR SHAREPOINT DIRECTORY STRUCTURE IN REPO
new.shp.dir.paths <- old.shp.paths %>%
  sub('\\/[^\\/]*$', '',.) %>% # extract characters before final /
  gsub(
    # replace the sharepoint directory
    pattern = paste0("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/",
                     "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/"), 
    # with the repo directory
    replacement = userPath, 
    x = ., # target
    fixed = TRUE) # needed to accomodate () in path

dir_create(new.shp.dir.paths) # create folder structure in repo


# 3. CREATE VECTOR OF PATHS FOR SHAPEFILES TO BE COPIED TO
new.shp.paths <- gsub(
  # replace the sharepoint directory
  pattern = paste0("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/",
                   "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/"), 
  # with the repo directory
  replacement = userPath, 
  x = old.shp.paths, # target
  fixed = TRUE) # needed to accomodate () in path

# 4. COPY FILES TO REPO
file_copy(old.shp.paths, new.shp.paths, overwrite = TRUE)


# 5. CHECK FILE SIZES FOR COMMIT TO REPO
# none exceed 50MB
file_info(new.shp.paths) %>%
  select(path, size) %>% 
  filter(size > fs_bytes("50MB"))


# FS DEMO CODE BELOW----------------

# create some files to copy over
dir_create("fs-example", c("CIN", "ADA")) # also subdirectories
# files with structure to match directory
files <- c("ADA/RAW-DATA.csv",
           "CIN/Clean Data.csv")

# add files to new directories
file_create("fs-example", files)
dir_tree("fs-example")
old.path.ex <- dir_ls(path = "fs-example", recurse = TRUE, type = "file")

# create new directory to copy files to
dir_create("new_dir")
dir_create("new_dir/ADA")

# modify old file path to new file path
new.path.ex <- gsub(pattern = "fs-example", replacement = "new_dir", fixed = TRUE, x = old.path.ex)
file_copy(old.path.ex, new.path.ex)




files_old <- dir_ls("fs-example")
files_old

files_new <- files_old |>
  str_to_lower() |>
  str_replace_all(" ", "-") |>
  str_replace_all("_", "-")

file_move(files_old, files_new)
dir_ls("fs-example")


subdirs <- c("R", "data", "reports")
dir_create("fs-example", subdirs)
dir_tree("fs-example")

r_files <- dir_ls("fs-example", glob = "*.r|*.R")
data_files <- dir_ls("fs-example", glob = "*.csv")
reports <- dir_ls("fs-example", glob = "*.qmd|*.pdf")

file_move(r_files, "fs-example/R")
file_move(data_files, "fs-example/data")
file_move(reports, "fs-example/reports")

dir_ls("fs-example")
dir_tree("fs-example")
dir_delete("fs-example")
