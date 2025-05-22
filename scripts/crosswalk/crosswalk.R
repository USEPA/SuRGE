library(sf)
library(readr)
library(dplyr)
library(stringr)
library(here)
library(tidyr)

#' Function to correct wonkiness in longitude and latitude readings
#' @param df data frame with lon and lat in it
#' @param lon the column name for longitude
#' @param lat the column name for latitude

fix_lon_lat <- function(df, lon, lat, site = "site_id", dropna = TRUE) {

  if(dropna){df <- drop_na(df)}
  fixed_df <- df |>
    mutate({{lon}} := case_when(str_detect(.data[[lon]], "^w") ~
                                  str_replace(.data[[lon]], "^w", "-"),
                                str_detect(.data[[lon]], "$?��") ~
                                  str_replace(.data[[lon]], "$?��", ""),
                                str_detect(.data[[lon]], " '") ~
                                  str_replace(.data[[lon]], " '", ""),
                                str_detect(.data[[lon]], "'") ~
                                  str_replace(.data[[lon]], "'", ""),
                                str_detect(.data[[lon]], "/3$") ~
                                  str_replace(.data[[lon]], "/3$", ""),
                                str_detect(.data[[lon]], "\\?\\?$") ~
                                  str_replace(.data[[lon]], "\\?\\?$", ""),
                                TRUE ~ .data[[lon]])) |>
    mutate({{lat}} := case_when(str_detect(.data[[lat]], "^w") ~
                                  str_replace(.data[[lat]], "^w", "-"),
                                str_detect(.data[[lat]], "$?��") ~
                                  str_replace(.data[[lat]], "$?��", ""),
                                str_detect(.data[[lat]], " '") ~
                                  str_replace(.data[[lat]], " '", ""),
                                str_detect(.data[[lat]], "'") ~
                                  str_replace(.data[[lat]], "'", ""),
                                str_detect(.data[[lat]], "/3$") ~
                                  str_replace(.data[[lat]], "/3$", ""),
                                str_detect(.data[[lat]], "\\?\\?$") ~
                                  str_replace(.data[[lat]], "\\?\\?$", ""),
                                TRUE ~ .data[[lat]])) |>
    mutate({{lon}} := case_when(str_detect(.data[[lon]], "\\?$") ~
                                  str_replace(.data[[lon]], "\\?$", ""),
                                str_detect(.data[[lon]], "\\?\\?") ~ str_replace(paste(
                                  str_split(.data[[lon]], "\\?\\?", simplify = TRUE)[,1],
                                  round(as.numeric(str_split(.data[[lon]], "\\?\\?",
                                                             simplify = TRUE)[,2])/60, 5),
                                  sep = "."), ".0.","."),
                                str_detect(.data[[lon]], "(\\..*?)\\.") ~
                                  str_replace(.data[[lon]], "(\\..*?)\\.", "."),
                                TRUE ~ .data[[lon]])) |>
    mutate({{lon}} := case_when(!str_detect(.data[[lon]], "^-") ~
                                  paste0("-", .data[[lon]]),
                                TRUE ~ .data[[lon]])) |>

    mutate({{lat}} := case_when(str_detect(.data[[lat]], "\\?$") ~
                                  str_replace(.data[[lat]], "\\?$", ""),
                                str_detect(.data[[lat]], "\\?\\?") ~ str_replace(paste(
                                  str_split(.data[[lat]], "\\?\\?", simplify = TRUE)[,1],
                                  round(as.numeric(str_split(.data[[lat]], "\\?\\?",
                                                             simplify = TRUE)[,2])/60, 5),
                                  sep = "."), ".0.","."),
                                .data[[lat]] == "." ~
                                  NA_character_,
                                str_detect(.data[[lat]], " ") ~
                                  str_replace(.data[[lat]], " ",""),
                                TRUE ~ .data[[lat]])) |>
    filter(!is.na(.data[[lon]])) |>
    filter(!is.na(.data[[lat]])) |>
    filter(!is.na(.data[[site]])) |>
    mutate({{lon}} := str_replace(str_remove(str_replace(.data[[lon]],"\\.", ";"), "\\."),
                                  ";", "."),
           {{lat}} := str_replace(str_remove(str_replace(.data[[lat]],"\\.", ";"), "\\."),
                                  ";", ".")) |>
    mutate({{lon}} := as.numeric(.data[[lon]]),
           {{lat}} := as.numeric(.data[[lat]]))
  fixed_df
}


surge_reservoirs <- st_read(here("SuRGE_Sharepoint/lakeDsn/all_lakes_2025-04-24.gpkg"), "all_lakes") |>
  st_transform(4326)

# NLA
nla22 <- read_csv(here("inputData/nla2022/nla22_siteinfo.csv"), guess_max = 35000) |>
  select(nla22_site_id = SITE_ID, lon = INDEX_LON_DD, lat = INDEX_LAT_DD) |>
  drop_na() |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla22 <- st_join(surge_reservoirs, nla22)
st_geometry(surge_nla22) <- NULL
surge_nla22 <- unique(surge_nla22) |>
  select(-lake_name)

nla17 <- read_csv(here("inputData/nla2017/nla_2017_profile-data.csv")) |>
  select(nla17_site_id = SITE_ID, lon = INDEX_LON_DD,
         lat = INDEX_LAT_DD) |>
  fix_lon_lat("lon", "lat", "nla17_site_id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla17 <- st_join(surge_reservoirs, nla17)
st_geometry(surge_nla17) <- NULL
surge_nla17 <- unique(surge_nla17) |>
  select(-lake_name)

# Column 19, 54, and 21 have some characters in them.  If important for
# crosswalk, clean up.
# Col 19 - COND_STD1_VALUE - not important
# Col 54 - TEMP_SENSOR - not important, but converts N/A to NA anyway
# Col 21 - COND_STD2_VALUE - not importnat, but converts N/A to NA anyway
nla12 <- read_csv(here("inputData/nla2012/nla2012_wide_profile_08232016.csv")) |>
  select(nla12_site_id = SITE_ID, lon = INDEX_LON_DD,
         lat = INDEX_LAT_DD) |>
  filter(!is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla12 <- st_join(surge_reservoirs, nla12)
st_geometry(surge_nla12) <- NULL
surge_nla12 <- unique(surge_nla12)  |>
  select(-lake_name)

nla07 <- read_csv(here("inputData/nla2007/nla2007_sampledlakeinformation_20091113.csv")) |>
  select(nla07_site_id = SITE_ID, lon = LON_DD, lat = LAT_DD) |>
  filter(!is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla07 <- st_join(surge_reservoirs, nla07)
st_geometry(surge_nla07) <- NULL
surge_nla07 <- unique(surge_nla07)  |>
  select(-lake_name)

#surge_nla <- full_join(surge_nla17, surge_nla12) |>
#  full_join(surge_nla07)

#lakemorpho from NHDPlus V2
lmorpho <- st_read(here("inputData/national_lmorpho/national_lake_morphometry.gpkg")) |>
  select(lmorpho_comid = COMID, lmorpho_nla07 = nlaSITE_ID) |>
  st_transform(st_crs(surge_reservoirs)) |>
  st_make_valid()
surge_lmorpho <- st_join(surge_reservoirs, lmorpho) |>
  select(-lake_name)
st_geometry(surge_lmorpho) <- NULL

#nhdplus v21 - check on lakemorpho
nhdplus <- st_read(here("inputData/nhd/nhd_plus_waterbodies.gpkg")) |>
  select(nhdplus_comid = COMID, gnis_id = GNIS_ID) |>
  st_transform(st_crs(surge_reservoirs))
sf_use_s2(FALSE)
nhdplus <- st_make_valid(nhdplus)
surge_nhdplus <- st_join(surge_reservoirs, nhdplus) |>
  select(-lake_name)
sf_use_s2(TRUE)
st_geometry(surge_nhdplus) <- NULL

#lagos depth
lagos_depth <- read_csv(here("inputData/lagos/lagos_depth.csv")) |>
  select(lon = lake_lon_decdeg, lat = lake_lat_decdeg, poly_intersect_lagoslakeid = lagoslakeid) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_lagos <- st_join(surge_reservoirs, lagos_depth) |>
  select(-lake_name)
st_geometry(surge_lagos) <- NULL

# Add in LAGOS ids from COMID match
#lagos_ids_old <- read_csv("https://github.com/user-attachments/files/20179527/lagos_ids.csv") |>
#  select(lake_id, comid_match_lagoslakeid = lagoslakeid)

lagos_ids <- read_csv("https://github.com/user-attachments/files/20193399/lagos_ids.csv") |>
  select(lake_id, comid_match_lagoslakeid = lagoslakeid) |>
  bind_rows(tibble(lake_id = c(231, 69),
                   comid_match_lagoslakeid = c(351297, 337264)))

surge_lagos <- left_join(lagos_ids, surge_lagos, relationship = "many-to-many")

# Existing Crosswalks: hylak
surge_hylak <- readr::read_csv(here::here("inputData/hylak/SuRGE_design_hylakID.csv")) |>
  select(lake_id = siteID, hylak_comid = COMID) |>
  mutate(lake_id = str_replace(lake_id, "CH4-", "")) |>
  mutate(lake_id = str_replace(lake_id, "^0+", ""))  |>
  mutate(lake_id = as.numeric(lake_id)) |>
  left_join(readr::read_csv(here("SuRGE_Sharepoint/data/siteDescriptors/hylak_link.csv"))) |>
  select(lake_id, hylak_comid, hylak_id, grand_id)

# Existing Crosswalks: nid
surge_nid <- readr::read_csv(here::here("inputData/nid/NID_data_for_Surge_and_hand_sites.csv")) |>
  select(lake_id = siteID, nid_id = NID.ID) |>
  mutate(lake_id = str_replace(lake_id, "CH4-", "")) |>
  mutate(lake_id = str_replace(lake_id, "^0+", ""))  |>
  mutate(lake_id = as.numeric(lake_id))

# Existing Crosswalks: nhdhr
surge_nhdhr <- readr::read_csv(here::here("inputData/nhd/Surge_nhdhr.csv")) |>
  mutate(lake_id = str_replace(siteID, "CH4-", "")) |>
  mutate(lake_id = str_replace(lake_id, "^0+", ""))  |>
  mutate(lake_id = as.numeric(lake_id))

# Combine in Master
surge_master_crosswalk <- surge_reservoirs
st_geometry(surge_master_crosswalk) <- NULL

surge_master_crosswalk <- left_join(surge_master_crosswalk, surge_nla17,
                                    by = "lake_id",
                                    relationship = "many-to-many") |>
  left_join(surge_nla22, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nla12, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nla07, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_lmorpho, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nhdplus, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_lagos, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_hylak, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nid, by = "lake_id", relationship = "many-to-many") |>
  mutate(across(c(lmorpho_comid, nhdplus_comid, comid_match_lagoslakeid,
                  poly_intersect_lagoslakeid, hylak_comid, hylak_id, grand_id), as.character))



surge_master_crosswalk_long <- pivot_longer(surge_master_crosswalk, 3:15,
                                            names_to = "join_id_name",
                                            values_to = "join_id") |>
  filter(!is.na(join_id)) |>
  filter(join_id != " ") |>
  filter(join_id != "NA") |>
  unique()

write_csv(surge_master_crosswalk_long, "output/crosswalk_long.csv")
