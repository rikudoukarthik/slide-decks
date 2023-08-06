require(tidyverse)
require(lubridate)
require(patchwork)
require(here)
require(sf)
require(ggrepel)

# setup -----------------------------------------------------------------------------

# setting the talk folder as root 
i_am("NCF_AM2023/index.Rmd")
talk_folder <- "NCF_AM2023/"

# plot defaults
theme_set(theme_classic() +
            theme(axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_text(size = 19),
                  axis.title = element_text(size = 21),
                  # no need for "Year"
                  axis.title.x = element_blank(),
                  panel.grid.major.y = element_line(linetype = 2, colour = "grey70")))

palette_point <- "#FCFA53"

palette <- c("#1B9E77", "#E89005", "#EF4050", "#9678B6")

# import data -----------------------------------------------------------------------

# data processed for current purpose (refer 01_scripts/00_create_ebd.R)
load(here(talk_folder, "00_data/NCF_sites.RData"))
load(here(talk_folder, "00_data/ebd_NCF_AM2023.RData"))
load(here(talk_folder, "00_data/inat_NCF_AM2023.RData"))

load(here(talk_folder, "00_data/maps_sf.RData"))

data_inat_sites <- data_inat_sites %>% 
  select(-PROGRAMME) %>% 
  left_join(ncf_sites %>% distinct(SITE.NAME, PROGRAMME, LABEL)) %>% 
  # filtering: nannaj not quite a programme
  filter(!SITE.NAME %in% c("Maharashtra - Nannaj", "Nannaj Long-term Monitoring")) %>% 
  mutate(PROGRAMME = factor(PROGRAMME, 
                            levels = c("Western Ghats Program", "CEROS Program",
                                       "Eastern Himalayan Program", "Oceans and Coasts Program",
                                       "High Altitudes Program", "Dibang Valley Team",
                                       "Thanamir Community Conservation Project")),
         LABEL = factor(LABEL,
                        levels = c("Western Ghats", "CEROS", "Eastern Himalaya", "Oceans and Coasts",
                                   "High Altitudes", "Dibang Valley", "Thanamir"))) 

data_ebd_sites <- data_ebd_sites %>% 
  select(-PROGRAMME) %>% 
  left_join(ncf_sites %>% distinct(SITE.NAME, PROGRAMME, LABEL)) %>% 
  # filtering: nannaj not quite a programme
  filter(!SITE.NAME %in% c("Maharashtra - Nannaj", "Nannaj Long-term Monitoring")) %>% 
  mutate(PROGRAMME = factor(PROGRAMME, 
                            levels = c("Western Ghats Program", "CEROS Program",
                                       "Eastern Himalayan Program", "Oceans and Coasts Program",
                                       "High Altitudes Program", "Dibang Valley Team",
                                       "Thanamir Community Conservation Project")),
         LABEL = factor(LABEL,
                        levels = c("Western Ghats", "CEROS", "Eastern Himalaya", "Oceans and Coasts",
                                   "High Altitudes", "Dibang Valley", "Thanamir"))) 

data_ebd_dists <- data_ebd_dists %>% 
  # filtering: nannaj not quite a programme
  filter(!COUNTY %in% c("Solapur", "Osmanabad"))
         
# map of sites -------------------------------------------------------------------

site_map <- ggplot(ncf_sites_points) +
  geom_sf(data = india_sf, colour = "grey40", fill = "black", linewidth = 0.7) +
  geom_sf(colour = palette_point, fill = NA, size = 7) +
  coord_sf(clip = "off") +
  theme_void()

ggsave(plot = site_map, here(talk_folder, "03_outputs/01_site_map.png"),
       units = "in", width = 20, height = 12, dpi = 300, bg = "black")


# summary over NCF programmes iNat over time ----------------------------------------

inat_summary <- data_inat_sites %>% 
  st_drop_geometry() %>% 
  group_by(YEAR, PROGRAMME, LABEL) %>% 
  summarise(NO.OBS = n_distinct(ID),
            NO.USERS = n_distinct(USER.NAME)) %>% 
  ungroup() %>% 
  complete(nesting(PROGRAMME = levels(data_inat_sites$PROGRAMME), 
                   LABEL = levels(data_inat_sites$LABEL)),
           YEAR = 2015:2023, 
           fill = list(NO.OBS = 0, NO.USERS = 0))

inat_prog_overtime <- inat_summary %>% 
  group_by(PROGRAMME) %>%
  mutate(F.YEAR = max(YEAR),
         LABEL = case_when(YEAR == F.YEAR ~ LABEL,
                           TRUE ~ "")) %>% 
  ungroup() %>% 
  # since we are using log scale, adding 1 to all
  mutate(NO.OBS = NO.OBS + 1) %>% 
  # wrapping labels
  mutate(LABEL = str_wrap(LABEL, width = 20)) %>% 
  arrange(PROGRAMME, YEAR) %>% 
  ggplot(aes(x = YEAR, y = NO.OBS, 
             colour = fct_inorder(PROGRAMME), label = fct_inorder(LABEL))) +
  geom_point(size = 3.5, position = position_dodge(0.3)) +
  geom_line(linewidth = 2, position = position_dodge(0.3)) +
  geom_text_repel(min.segment.length = Inf, hjust = 0, direction = "y",
                  fontface = 2, size = 8, xlim = c(2023.1, NA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_log10(n.breaks = 5) +
  scale_x_continuous(breaks = 2015:2023, limits = c(2014.7, 2025)) +
  guides(colour = "none") +
  labs(x = "Year", y = "No. of observations")

ggsave(plot = inat_prog_overtime, here(talk_folder, "03_outputs/02_inat_prog.png"),
       units = "in", width = 16, height = 9, dpi = 300, bg = "black")

# summary over NCF programmes eBird over time ---------------------------------------------------------

ebird_summary <- data_ebd_sites %>% 
  st_drop_geometry() %>% 
  group_by(YEAR, PROGRAMME, LABEL) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            NO.USERS = n_distinct(OBSERVER.ID)) %>% 
  ungroup() %>% 
  complete(nesting(PROGRAMME = levels(data_ebd_sites$PROGRAMME), 
                   LABEL = levels(data_ebd_sites$LABEL)),
           YEAR = 2015:2023, 
           fill = list(NO.LISTS = 0, NO.USERS = 0))

ebird_prog_overtime_df <- ebird_summary %>% 
  group_by(PROGRAMME) %>%
  mutate(F.YEAR = max(YEAR),
         LABEL = case_when(YEAR == F.YEAR ~ LABEL,
                           TRUE ~ "")) %>% 
  ungroup() %>% 
  # since we are using log scale, adding 1 to all
  mutate(NO.LISTS = NO.LISTS + 1) %>% 
  # wrapping labels
  mutate(LABEL = str_wrap(LABEL, width = 20)) %>% 
  arrange(PROGRAMME, YEAR) 

ebird_prog_overtime <- ebird_prog_overtime_df %>% 
  ggplot(aes(x = YEAR, y = NO.LISTS, 
             colour = fct_inorder(PROGRAMME), label = fct_inorder(LABEL))) +
  geom_point(size = 3.5, position = position_dodge(0.3)) +
  geom_line(linewidth = 2, position = position_dodge(0.3)) +
  geom_text_repel(min.segment.length = Inf, hjust = 0, direction = "y",
                  fontface = 2, size = 8, xlim = c(2023.1, NA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_log10(breaks = c(2, 10, 100, 1000, 3000)) +
  scale_x_continuous(breaks = 2015:2023, limits = c(2014.7, 2025)) +
  guides(colour = "none") +
  labs(x = "Year", y = "No. of unique lists")

ggsave(plot = ebird_prog_overtime, here(talk_folder, "03_outputs/03_ebird_prog.png"),
       units = "in", width = 16, height = 9, dpi = 300, bg = "black")


# summary of eBird (sites vs dists) -------------------------------------------------------

ebird_sites_dists0 <- data_ebd_dists %>% 
  group_by(YEAR, COUNTY) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  ungroup() %>% 
  left_join(ncf_dists %>% distinct(DISTRICT.NAME, PROGRAMME, LABEL), 
            by = c("COUNTY" = "DISTRICT.NAME")) %>% 
  group_by(YEAR, PROGRAMME, LABEL) %>% 
  summarise(DIST.LISTS = sum(NO.LISTS)) %>% 
  ungroup()

ebird_sites_dists1 <- data_ebd_sites %>% 
  st_drop_geometry() %>% 
  group_by(YEAR, SITE.NAME, PROGRAMME, LABEL) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(YEAR, PROGRAMME, LABEL) %>% 
  summarise(SITE.LISTS = sum(NO.LISTS)) %>% 
  ungroup()

ebird_sites_dists <- full_join(ebird_sites_dists0, ebird_sites_dists1) %>% 
  mutate(PROP.LISTS = 100*SITE.LISTS/DIST.LISTS) %>% 
  group_by(PROGRAMME) %>% 
  mutate(F.YEAR = max(YEAR),
         LABEL = case_when(YEAR == F.YEAR ~ LABEL,
                           TRUE ~ "")) %>% 
  ungroup() %>% 
  # wrapping labels
  mutate(LABEL = str_wrap(LABEL, width = 20)) %>% 
  arrange(PROGRAMME, YEAR) 


ebird_sites_dists_plot <- ebird_sites_dists %>% 
  ggplot(aes(x = YEAR, y = PROP.LISTS, 
             colour = fct_inorder(PROGRAMME), label = fct_inorder(LABEL))) +
  geom_point(size = 3.5, position = position_dodge(0.3)) +
  geom_line(linewidth = 2, position = position_dodge(0.3)) +
  geom_text_repel(min.segment.length = Inf, hjust = 0, direction = "y",
                  fontface = 2, size = 8, xlim = c(2023.1, NA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = 2015:2023, limits = c(2014.7, 2025)) +
  guides(colour = "none") +
  labs(x = "Year", y = "Prop. contribution to district lists")

ggsave(plot = ebird_sites_dists_plot, here(talk_folder, "03_outputs/04_ebird_sitesvsdists.png"),
       units = "in", width = 16, height = 9, dpi = 300, bg = "black")


# iNat histogram (supp.) --------------------------------------------------------------------

inat_histo <- data_inat %>% 
  group_by(USER.NAME) %>% 
  reframe(NO.OBS = n_distinct(ID)) %>% 
  ggplot() +
  geom_histogram(aes(NO.OBS), bins = 80, fill = "black") +
  labs(x = "No. of observations", y = "No. of users", title = "NCF India iNat Project") +
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 21))

ggsave(plot = inat_histo, here(talk_folder, "03_outputs/05_inat_histo.png"),
       units = "in", width = 16, height = 9, dpi = 300, bg = "black")


# leaderboards of programmes (supp.) ---------------------------------------------------------

temp1 <- inat_summary %>% 
  select(-PROGRAMME, -NO.OBS) %>% 
  rename(PROGRAMME = LABEL) %>% 
  group_by(PROGRAMME) %>% 
  mutate(TOTAL = sum(NO.USERS)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = YEAR, values_from = NO.USERS) %>% 
  relocate(TOTAL, .after = last_col()) %>% 
  mutate(PLATFORM = "iNat") %>% 
  arrange(desc(TOTAL), desc(2023), desc(2022), desc(2021), desc(2020), desc(2019), 
          desc(2018), desc(2017), desc(2016))

temp2 <- ebird_summary %>% 
  select(-PROGRAMME, -NO.LISTS) %>% 
  rename(PROGRAMME = LABEL) %>% 
  group_by(PROGRAMME) %>% 
  mutate(TOTAL = sum(NO.USERS)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = YEAR, values_from = NO.USERS) %>% 
  relocate(TOTAL, .after = last_col()) %>% 
  mutate(PLATFORM = "eBird") %>% 
  arrange(desc(TOTAL), desc(2023), desc(2022), desc(2021), desc(2020), desc(2019), 
          desc(2018), desc(2017), desc(2016))

table1 <- bind_rows(temp1, temp2)


temp1 <- inat_summary %>% 
  select(-PROGRAMME, -NO.USERS) %>% 
  rename(PROGRAMME = LABEL) %>% 
  group_by(PROGRAMME) %>% 
  mutate(TOTAL = sum(NO.OBS)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = YEAR, values_from = NO.OBS) %>% 
  relocate(TOTAL, .after = last_col()) %>% 
  mutate(PLATFORM = "iNat") %>% 
  arrange(desc(TOTAL), desc(2023), desc(2022), desc(2021), desc(2020), desc(2019), 
          desc(2018), desc(2017), desc(2016))

temp2 <- ebird_summary %>% 
  select(-PROGRAMME, -NO.USERS) %>% 
  rename(PROGRAMME = LABEL) %>% 
  group_by(PROGRAMME) %>% 
  mutate(TOTAL = sum(NO.LISTS)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = YEAR, values_from = NO.LISTS) %>% 
  relocate(TOTAL, .after = last_col()) %>% 
  mutate(PLATFORM = "eBird") %>% 
  arrange(desc(TOTAL), desc(2023), desc(2022), desc(2021), desc(2020), desc(2019), 
          desc(2018), desc(2017), desc(2016))

table2 <- bind_rows(temp1, temp2)


write_csv(table1, here(talk_folder, "03_outputs/table1.csv"))
write_csv(table2, here(talk_folder, "03_outputs/table2.csv"))
