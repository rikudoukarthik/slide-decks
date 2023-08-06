library(tidyverse)
library(sf)
library(here)
i_am("NCF_AM2023/index.Rmd")
talk_folder <- "NCF_AM2023/"

load(here(talk_folder, "00_data/NCF_sites.RData"))
load("../BCI/ebird-datasets/EBD/ebd_IN_relMay-2023.RData")


# first filter
data_filt <- data %>% 
  filter(YEAR >= 2015,
         STATE %in% c("Andaman and Nicobar Islands", "Karnataka", "Lakshadweep",
                      "Himachal Pradesh", "Arunachal Pradesh", "Tamil Nadu",
                      "Ladakh", "Maharashtra", "Nagaland", "West Bengal",
                      "Jammu and Kashmir")) %>% 
  dplyr::select(-c(M.YEAR, M.MONTH, LAST.EDITED.DATE, BREEDING.CODE, HAS.MEDIA))


# filtering for districts of interest
data_ebd_dists <- data_filt %>% 
  filter(COUNTY %in% ncf_dists$DISTRICT.NAME)

# for 15 km site buffer
data_ebd_sites <- data_ebd_dists %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(ncf_sites))

data_ebd_sites <- data_ebd_sites[unlist(st_intersects(ncf_sites, data_ebd_sites)),] %>% 
  st_join(ncf_sites)


# saving objects
save(data_ebd_dists, data_ebd_sites, file = "ebd_NCF_AM2023.RData")
