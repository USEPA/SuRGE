# This script is for reading nutrient data from Region 10 (R10) sampling
# in 2018.  Data are at: …data/chemistry/nutrients/2018_ESF-EFWS_NutrientData_Updated03012019_SS_CTNUpdate04012019.xlsx

# Data unique identifiers (e.g., site_id field in Excel file) follows the QAPP
# for the 2018 sampling ("...\projectDocuments\QAPP, SOPs, manuals, HASP\QAPP for an assessment of methane emissions from Region 10 reservoirs.docx")

# All samples were unfiltered, hence all analytes are reported with a "T" in the 
# name (e.g. TNH4, rather than NH4).  Just ignore this and adopt the analyte names
# defined in github issue #10.

# Samples were collected at an open-water and tributary site.  We only want the
# open_water site data.