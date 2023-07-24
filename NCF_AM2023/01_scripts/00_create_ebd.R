load("../ebird-datasets/EBD/ebd_IN_relMay-2023.RData")

data_filt <- data %>% 
  filter(YEAR >= 2015,
         STATE %in% c("Andaman and Nicobar Islands", "Karnataka", "Lakshadweep",
                      "Himachal Pradesh", "Arunachal Pradesh", "Tamil Nadu",
                      "Ladakh", "Maharashtra", "Nagaland", "West Bengal",
                      "Jammu and Kashmir")) %>% 
  dplyr::select(-c(M.YEAR, M.MONTH, LAST.EDITED.DATE, BREEDING.CODE, HAS.MEDIA))

save(data_filt, file = "ebd_NCF_AM2023.RData")
