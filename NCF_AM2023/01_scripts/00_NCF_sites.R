library(googlesheets4)
library(tidyverse)
library(sf)
library(here)
i_am("NCF_AM2023/index.Rmd")
talk_folder <- "NCF_AM2023/"

load(here(talk_folder, "00_data/maps_sf.RData"))


# sites info from G Sheet
gs4_auth("karthik.t@ncf-india.org")

sheet_url <- "https://docs.google.com/spreadsheets/d/1C-eIQY9n3Bb_JUBLXna_wLcLG4nJsO8G-ve3elCw39U/edit#gid=0"

ncf_sites <- read_sheet(sheet_url, sheet = "Final list") %>% 
  # process
  dplyr::select(-1) %>% # s no column
  dplyr::select(c(everything(), - "GPS Coordinates of the field station")) %>% 
  magrittr::set_colnames(c("SITE.NAME", "SITE.COORD", "STATE.NAME", 
                           "PROGRAMME", "LABEL", "INFO.SOURCE")) %>% 
  # nannaj not quite a programme
  filter(!SITE.NAME %in% c("Maharashtra - Nannaj", "Nannaj Long-term Monitoring")) %>% 
  mutate(LONGITUDE = str_split(SITE.COORD, ", ", simplify = TRUE)[,2],
         LATITUDE = str_split(SITE.COORD, ", ", simplify = TRUE)[,1],
         SITE.COORD = NULL) %>% 
  # we need to merge very close points
  mutate(SITE.NAME = case_when(str_detect(SITE.NAME, "Valparai") ~ "Valparai Office & Field Station",
                               str_detect(SITE.NAME, "Pakke") ~ "Pakke Base Camps",
                               TRUE ~ SITE.NAME)) %>% 
  # converting to sf object
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  # set crs
  st_set_crs("OGC:CRS84") %>% 
  # merging overlapping points
  group_by(SITE.NAME, PROGRAMME, LABEL) %>% 
  dplyr::summarise()


# buffers 
ncf_sites_buff <- st_buffer(ncf_sites, dist = 15000) # 15 km (only LD lower)


# linking sites to districts
ncf_dists <- ncf_sites_buff %>% 
  st_intersection(dists_sf) %>% 
  dplyr::select(SITE.NAME, PROGRAMME, LABEL, DISTRICT.NAME, STATE.NAME) %>% 
  # remove point geometries
  st_drop_geometry() %>% 
  left_join(dists_sf) %>% 
  mutate(DISTRICT.NAME = case_when(
    DISTRICT.NAME == "Lahul and Spiti" ~ "Lahaul and Spiti",
    DISTRICT.NAME == "Bandipore" ~ "Bandipora",
    TRUE ~ DISTRICT.NAME
  ))



# renaming buffered and non-
ncf_sites_points <- ncf_sites
ncf_sites <- ncf_sites_buff


# save

save(ncf_sites, ncf_sites_points, ncf_dists, 
     file = here(talk_folder, "00_data/NCF_sites.RData"))
