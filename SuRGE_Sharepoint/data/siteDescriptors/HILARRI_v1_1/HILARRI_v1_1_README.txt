---------------------------------------------------------------------
DATASET OVERVIEW

Dataset Title: Hydropower Infrastructure - LAkes, Reservoirs, and Rivers Version 1.1 (HILARRIv1.1)

Investigators: Carly Hansen, hansench@ornl.gov, ORCID: 0000-0001-9328-0838; Paul Matson, matsonpg@ornl.gov, ORCID: 0000-0003-2105-7308

Point of Contact: Carly Hansen, hansench@ornl.gov, Oak Ridge National Laboratory

Summary: HILARRI is a database of links between major datasets of operational hydropower dams and powerplants (National Inventory of Dams (2019), Global Reservoir and Dam Database (GRanD v1.3), Existing Hydropower Assets (EHA 2021), and inland water bodies (NHDPlusV2 river network, NHD water bodies, NHD Watershed Boundary Dataset, HydroLAKES). 

Keywords:Existing Assets, Hydrography

Acknowledgments: The dataset was produced with funding from the US Department of Energy Water Power Technology Office.

Related Publication: N/A

Related Datasets: Existing Hydropower Assets (DOI: 10.21951/EHA_FY2021/1782791)
---------------------------------------------------------------------
DATASET CHARACTERISTICS

Spatial Resolution: point locations

Projection Information: EPSG 4269

Temporal Resolution: N/A

Temporal Coverage: up to May 2021 (last published Existing Hydropower Assets operational plants dataset)

File Format: .csv

File Naming Convention: 
HILARRI_vX.X_Public (where X.X is the major release and minor update versioning) is the full dataset
HILARRI_vX.X_Public_SubsetHydropowerDams is a table limited to just the unique hydropower dams. No additional waterbody or plant information is included
HILARRI_vX.X_Public_SubsetHydropowerDams_Plants is a table limited to just unique pairs between hydropower dams and power plants. No additional waterbody information is included

File Descriptions: See HILARRI_Field_Descriptions.txt and HILARRI_v1_1_UserNotes.docx

Data Dictionary: See HILARRI_Field_Descriptions.txt 
---------------------------------------------------------------------
APPLICATION & DERIVATION 

Connections between major hydropower infrastructure and hydrographic data are critical for conducting large-scale 
analysis of hydropower infrastructure and their associated natural and engineered water systems.
---------------------------------------------------------------------
QUALITY ASSESSMENT 

Estimate of Uncertainty: The HILARRI dataset also incorporates information from additional datasets to facilitate 
more effective and accurate analysis. For example, dams were checked against the most recent American Rivers Dam 
Removal Database to identify and remove facilities that no longer exist. Additionally, dams that are listed multiple 
times in the NID are identified and flagged to avoid double-counting when analyzing and summarizing information. 
Other quality flags include certainty of operational hydropower (i.e., if one or more datasets indicates hydropower 
at a particular location), and whether an associated water body is accurate or composed of multiple polygons. These 
additional data flags will increase confidence in data usage for individual to large-scale analyses.
---------------------------------------------------------------------
DATA ACQUISITION, MATERIALS & METHODS

Data were processed and compiled using QGISv3.10.10
---------------------------------------------------------------------
CHANGE LOG
V1.0 of this dataset was published on May 5th, 2021.

V1.1 was published Aug 23rd, 2021 and includes the following updates:
- revised matching between dams and plants based on most current EHA operational plants dataset (published in May 2021)
- performed additional QA to match between dam inventories and water body datasets
- removed duplicates in the dams (i.e., cases where dams are listed by multiple states)
- added unique tables with just unique dams and dam-plant pairs to facilitate more convenient analysis
- added a user guide describing how the dataset works and detailing some of the complexities
---------------------------------------------------------------------
REFERENCES
- American Rivers Dam Removal Database v.8 (https://figshare.com/articles/dataset/American_Rivers_Dam_Removal_Database/5234068) 
American Rivers (2019): American Rivers Dam Removal Database. figshare. Dataset. https://doi.org/10.6084/m9.figshare.5234068 
- Existing Hydropower Assets Plant Dataset, 2021 (https://hydrosource.ornl.gov/dataset/existing-hydropower-assets-plant-dataset-2021)
M.M. Johnson, S.-C. Kao, N.M. Samu, and R. Uria-Martinez, Existing Hydropower Assets, 2021. HydroSource. Oak Ridge National Laboratory, Oak Ridge, TN. DOI: 10.21951/EHA_FY2021/1782791
- Global Reservoirs and Dams (GRanD v1.3) (http://globaldamwatch.org/grand/) Lehner, B., C. Reidy Liermann, C. Revenga, C. Vorosmarty, 
B. Fekete, P. Crouzet, P. Doll, M. Endejan, K. Frenken, J. Magome, C. Nilsson, J.C. Robertson, R. Rodel, N. Sindorf, and D. Wisser. 2011. Global Reservoir and Dam Database, Version 1 (GRanDv1): Reservoirs, Revision 01. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4HH6H08.
- HydroLAKES (https://www.hydrosheds.org/page/hydrolakes) Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. (2016): Estimating the volume and age of water stored in global lakes using a geo-statistical approach. Nature Communications: 13603. doi: 10.1038/ncomms13603 (open access)
- National Hydrography Dataset Plus (NHD) v.2 (https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data) 
McKay, L., Bondelid, T., Dewald, T., Johnston, J., Moore, R., and Rea, A., “NHDPlus Version 2: User Guide”, 2012"
- National Inventory of Dams. Published in 2019 (https://nid.sec.usace.army.mil/)
- National Watershed Boundary Dataset (WBD) (https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con)
- US Hydropower Potential from Existing Non-powered Dams (greater than 1MW) (https://hydrosource.ornl.gov/dataset/us-hydropower-potential-existing-non-powered-dams-greater-1mw) 
Hadjerioua, B., Wei, Y. and Kao, S.C., 2012. An Assessment of Energy Potential at Non-Powered Dams in the United States. GPO DOE/EE-0711, Wind and Water Power Program, Department of Energy, DC.
- U.S. Hydropower Retired Facilities Data and Metadata, 2020 (https://hydrosource.ornl.gov/dataset/us-hydropower-retired-facilities-data-and-metadata-2020)
M.M. Johnson, S.-C. Kao, N.M. Samu, and Uría-Martinez, R., (2020). U.S. Hydropower Retired Facilities, 2020. HydroSource. Oak Ridge National Laboratory, Oak Ridge, TN.
- U.S. Hydropower Development Pipeline Data and Metadata, 2020 (https://hydrosource.ornl.gov/dataset/US-Hydropower-Pipeline-Data) 
Johnson, M.M., and Uría-Martínez, R., (2020). U.S. Hydropower Development Pipeline Data, 2020. DOI: 10.21951/HMR_PipelineFY20/1756447
---------------------------------------------------------------------
SUPPLEMENTAL FILES

HILARRI_v1_1_Public.csv
HILARRI_v1_1_Public.Rdata
HILARRI_v1_1_Public.gpkg
HILARRI_v1_1_Shapefile

HILARRI_v1_1_Public_SubsetHydropowerDams.csv
HILARRI_v1_1_Public_SubsetHydropowerDams_Plants.csv

HILARRI_Field_Descriptions_v1_1.csv
HILARRI_v1_1_UserNotes.docx
HILARRI_Acronyms_v1_1.csv
HILARRI_Workflow_v1_1.png
HILARRI_Overview_v1_1.tif

---------------------------------------------------------------------