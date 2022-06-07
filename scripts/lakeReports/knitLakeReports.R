

files <- fs::dir_ls(path = "scripts/lakeReports", regexp = "[.]Rmd$")
for (f in files) rmarkdown::render(f)
