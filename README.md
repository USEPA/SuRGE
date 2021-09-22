# Survey of Reservoir Greenhouse gas Emissions (SuRGE)

## R Project Directory Structure
### SuRGE SharePoint Shared Documents Library
SuRGE data are maintained in the shared documents library at the private SuRGE SharePoint site (https://usepa.sharepoint.com/sites/SuRGE).  Scripts in this project assume users have synced the documents library to their computer.  Contact Jake Beaulieu if you do not have access the site or need assistance syncing to your computer.

### Abolute and Relative File Paths
R scripts in this repository read data from the SuRGE SharePoint site.  Scripts used to create lake specific survey designs are contained in scripts/lakeDsn/... and use relative file paths to read data from SharePoint.  If you wish to reproduce the survey designs, you must create your local clone within the rProjects folder at SharePoint.  The remainining scripts use absolute file paths based on the values returned from `Sys.getenv("USERPROFILE")`.  These scipts can be run regardless of where the R project is cloned on your computer.

## Library Management

This project uses the `renv` library to manage library versions.  After creating a local clone, a message will appear in the console directing users to sync the project library  by running `renv::restore()`.  This will download the packages and versions used in this project as specified in the renv.lock file.  If you need to add libraries to the project, be sure to use `renv::snapshot` to update the lock file.  When collaborators sync their local clone with the master, they can then run `renv::restore()` to have the new library installed on their local project directory.  Please see here (https://rstudio.github.io/renv/index.html) for a primer on `renv` and here (https://rstudio.github.io/renv/articles/collaborating.html) for using `renv` in a collaborative workflow.

Please use caution when updating libraries already captured in the lock file.  The code runs succesfully with the suite of packages captured in the current lock file; updating package versions could cause problems with the existing code.  The whole point of `renv` is to  minimize the potential for code breakage due to differences among package versions.

The R project was initiated by J Beaulieu using R.3.6.2 and `renv` will produce a warning message if a different version of R is used.  In theory, `renv` shouldn't be sensitive to the version of R, but in practice I have found that `renv::restore()` can take a very long time or even fail if a different version of R is used.  For example, `renv::restore()` took 1.5 hours when tested with R.4.1.0, then `readxl::read_excel()` threw an error related to the `Rcpp` package.  I suggest using R.3.6.2.  

Please note that the .Rprofile file is under version control in the repository.  This file contains an autoloader which automatically downloads and installs the appropriate version of `renv` into the project library.  It is best that users do not use the .Rprofile file to further customize the environment. 

Finally, `renv::restore()` may require Rtools.  Rtools can be found here (https://cran.r-project.org/bin/windows/Rtools/history.html).  Be sure to install a version that is compatible with your R version.

## Conflicted
This project uses `conflicted` to manage conflicts between the `dplyr` functions `select()` and `filter()` and functions with the same names, but in different packages (i.e.`MASS::select()`).  The `conflicted` settings will always give preference to the dplyr library when `select()` or `filter()` are called.

## Running Scripts
Scripts must be run in the sequence documented in scripts/masterScript.R.  Please see status update at the top of script for current status of the scripts.

## Version Control
This project uses git version control.  git software is independent from the github website and must be installed on each users computer.  Users will be given push/pull rights to the repo and can therefore push directly to the repo without the need to submit a pull request.  To minimize conflicts among separate commits, it is best practice to start each session by pulling from the repo to get the most up to date code.  Similarly, users should commit and push their changes to the repo at the conclusion of each working session.  
