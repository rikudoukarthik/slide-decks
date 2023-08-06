library(tidyverse)
library(sf)
library(here)
i_am("NCF_AM2023/index.Rmd")
talk_folder <- "NCF_AM2023/"

load(here(talk_folder, "00_data/NCF_sites.RData"))


# already filtered: observations from 2015
data_inat <- read_csv(here(talk_folder, "00_data/inat_NCFproject.csv")) %>% 
  magrittr::set_colnames(colnames(.) %>% str_to_upper() %>% str_replace_all("_", ".")) %>% 
  # year
  mutate(OBSERVED.ON = as_date(OBSERVED.ON),
         CREATED.AT = as_date(CREATED.AT)) %>% 
  rename(OBSERVATION.DATE = OBSERVED.ON,
         UPLOAD.DATE = CREATED.AT,
         STATE = PLACE.STATE.NAME) %>% 
  mutate(YEAR = year(OBSERVATION.DATE),
         UPLOAD.YEAR = year(UPLOAD.DATE),
         STATE = case_when(STATE == "Andaman and Nicobar" ~ "Andaman and Nicobar Islands",
                           TRUE ~ STATE)) %>% 
  # columns of use
  dplyr::select(c(ID, OBSERVATION.DATE, TIME.OBSERVED.AT, USER.NAME, UPLOAD.DATE, OBSERVATION.DATE,
                  STATE, YEAR, UPLOAD.YEAR, QUALITY.GRADE, LICENSE, LATITUDE, LONGITUDE,
                  PLACE.COUNTY.NAME, PLACE.ADMIN1.NAME, PLACE.ADMIN2.NAME, SPECIES.GUESS, 
                  SCIENTIFIC.NAME, COMMON.NAME, contains("TAXON.")))

# filtering to match eBird but for iNat will use unfiltered also
data_inat_filt <- data_inat %>% 
  # Ladakh probably not present
  filter(STATE %in% c("Andaman and Nicobar Islands", "Karnataka", "Lakshadweep",
                      "Himachal Pradesh", "Arunachal Pradesh", "Tamil Nadu",
                      "Ladakh", "Maharashtra", "Nagaland", "West Bengal",
                      "Jammu and Kashmir"))

# for 15 km site buffer
data_inat_sites <- data_inat_filt %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(ncf_sites))

data_inat_sites <- data_inat_sites[unlist(st_intersects(ncf_sites, data_inat_sites)),] %>% 
  st_join(ncf_sites)



# renaming
data_inat_full <- data_inat

# saving objects
save(data_inat, data_inat_sites, 
     file = here(talk_folder, "00_data/inat_NCF_AM2023.RData"))
