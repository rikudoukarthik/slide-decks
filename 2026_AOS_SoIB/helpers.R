# auth --------------------------------------------------------------------

auth_gdrive <- function() {
  
  require(googledrive)
  require(googlesheets4)
  
  drive_auth(email = "rikudoukarthik@gmail.com")
  gs4_auth(email = "rikudoukarthik@gmail.com")
  
}

# taxonomy lists and species data -----------------------------------------

get_avilist <- function(which = "raw") {

  if (!which %in% c("raw", "processed")) stop("Select 'raw'/'processed'")
  
  avilist <- read_xlsx("data/AviList-v2025-11Jun-extended.xlsx", sheet = 1)
  
  if (which == "processed") {
    avilist <- avilist |> 
      filter(Taxon_rank %in% c("species")) |> 
      select(Family, Scientific_name, starts_with("English_name")) |> 
      rename(FAMILY = Family,
             SCI.NAME.AVILIST = Scientific_name,
             ENG.NAME.AVILIST = English_name_AviList,
             ENG.NAME.EBIRD = English_name_Clements_v2024,
             ENG.NAME.BLI = English_name_BirdLife_v9)
  }
  
  return(avilist)
  
}

get_bli_list <- function() {
  
  read_xlsx("data/BLI_species_maps/BLI_v10.xlsx", sheet = 1, range = "A4:P33291") |> 
    # only species rows by default
    filter(!is.na(`Common name`))
  
}

get_chaco_list <- function(which = "raw") {
  
  if (!which %in% c("raw", "proved")) stop("Select 'raw'/'proved'")
  
  require(googledrive)
  require(googlesheets4)
  require(magrittr)
  
  auth_gdrive()
  
  chaco_spec <- read_sheet(ss = "126dQabZUktXD7PTFnOaaq2xj86njtIeasXAIsrk9Rys") %>%
    {if (which == "proved") {
      filter(., Proved_to_Occur == TRUE)
    } else {
      .
    }} %>%
    select(eBird_Family, eBird_Name_Sci, eBird_Name_Eng) |> 
    magrittr::set_colnames(c("FAMILY.EBIRD", "SCI.NAME.EBIRD", "ENG.NAME.EBIRD"))
  
  return(chaco_spec)
  
}


# functional traits datasets ----------------------------------------------

get_avonet <- function(which = "summary", 
                       tax = if (which == "summary") "eBird" else NULL) {
  
  which_list <- c("raw", "summary")
  tax_list <- c("BirdLife", "eBird", "BirdTree")
  file_name <- "AVONET Supplementary dataset 1.xlsx" # file name as on Figshare
  file_path <- paste0("data/traits/", file_name)
  
  # checks ------------------------------------------------------------------
  
  if (!which %in% which_list) stop(
    "Invalid data/sheet specification (raw/summary)"
  )
  
  if (which == "raw" & !is.null(tax)) stop(
    "Raw data cannot have taxonomy specification"
  )
  
  if (which == "summary") {
    
    if (is.null(tax)) {
      stop("Summary data require taxonomy specification")
    } else if (!(tax %in% tax_list)) {
      stop("Invalid taxonomy (BirdLife/eBird/BirdTree)") 
    }
    
  } 
  
  # get data ----------------------------------------------------------------
  
  sheet <- if (which == "raw") "AVONET_Raw_Data" else {
    paste0("AVONET", which(tax == tax_list), "_", tax)
  }
  
  require(readxl)
  avonet <- read_xlsx(path = file_path, sheet = sheet,
                      na = c("", "NA", " "))
  
  # tidy up, arrange --------------------------------------------------------
  
  # modify data according to specific sheet
  
  if (which != "raw") {
    
    avonet <- avonet |> 
      # gets read as character because col has "NA" along with numbers, so fix
      mutate(Migration = as.double(Migration)) |> 
      # rename species name cols to explicitly state tax
      rename_with(.cols = starts_with("Species"),
                  ~ str_c(.x, tax, sep = "_"))
    
  } else if (which == "raw") {
    
    avonet <- avonet |> 
      select(-c(
        Locality, Country # completely empty
      ))
    
  }

  # rename cols to consistent style
  avonet <- avonet |> 
    # my format
    rename_with(toupper) %>% 
    rename_with(~ str_replace_all(string = .x, pattern = "_", replacement = ".")) |> 
    rename_with(~ str_replace_all(string = .x, pattern = "-", replacement = "."))
  
    
  # focus on AVONET ---------------------------------------------------------
  
  # remove traits from this dataset that aren't forte of AVONET
  # e.g., those that are better used from AVONICHE and BIRDBASE
  
  # these are only in the summary sheets, but tidyselect means I don't need 
  # a conditional here
  cols_to_remove <- c(
    "HABITAT", "HABITAT.DENSITY", "MIGRATION", "TROPHIC.LEVEL", "TROPHIC.NICHE",
    "PRIMARY.LIFESTYLE", "MIN.LATITUDE", "MAX.LATITUDE", "CENTROID.LATITUDE",
    "CENTROID.LONGITUDE", "SPECIES.STATUS.BIRDTREE"
  )
  
  avonet <- avonet |> select(-any_of(cols_to_remove))
  
  # return ------------------------------------------------------------------
  
  return(avonet)
  
}


# frequent data imports ---------------------------------------------------

# aridity index

get_arid_index <- function(which = "raw") {
  
  if (!which %in% c("raw", "reclass")) stop("Select valid AI object")
  
  require(terra)
  
  if (which == "raw") {
    
    ai <- terra::rast("data/trabucco_zomer_2022/ai_v31_yr.tif") * 0.0001
    return(ai)
    
  } else if (which == "reclass") {
    
    ai <- terra::rast("data/processed/drylands_rast_reclass.tif")
    return(ai)
    
  }
  
}


# crs values for quick transformation

# EPSG 6933 vs 4326: projected, equal-area; for env, raster data (like satellite imagery)
# 4326 lat-long based, for GPS and vector data, area distortion at high lats
# 6933 better for area calc

get_crs <- function(which = "ea") {
  
  if (!which %in% c("ea", "ll")) stop("Select valid CRS type")
  
  if (which == "ea") {
    return(6933)
  } else if (which == "ll") {
    return(4326)
  }
  
}


# get timeline from GSheets -----------------------------------------------

# import from GSheets, transform, save locally and return object

get_phd_timeline <- function() {
  
  require(googledrive)
  require(googlesheets4)
  require(tidyr)
  require(dplyr)
  require(lubridate)
  require(readr)
  require(forcats)
  require(here)
  
  # vars
  phd_start <- as_date("2025-01-01")
  
  drive_auth(email = "rikudoukarthik@gmail.com")
  gs4_auth(email = "rikudoukarthik@gmail.com")
  timeline_sheet <- read_sheet(ss = "1VU5w0SHfU4YC1WdE-3V09NTXcGFlyge1GbhVOrAxLkI")
  
  
  # data wrangling
  
  timeline <- timeline_sheet |>
    # make longer (tidy)
    separate_longer_delim(cols = CHAPTER, delim = ", ") |>
    separate_longer_delim(cols = SEMESTER, delim = ", ") |>
    # add cols
    mutate(
      SEMESTER.NO = as.numeric(SEMESTER), # rename
      SEMESTER = case_when(
        SEMESTER.NO %in% seq(1, length.out = 5, by = 2) ~ "Spring",
        SEMESTER.NO %in% seq(2, length.out = 5, by = 2) ~ "Fall",
        SEMESTER.NO %in% seq(1.5, length.out = 5, by = 2) ~ "Summer",
      ),
      SEMESTER = factor(SEMESTER, levels = c("Spring", "Summer", "Fall")),
      
      YEAR.NO = ceiling(SEMESTER.NO/2),
      
      DATE.FROM = case_when(
        SEMESTER == "Spring" ~ phd_start + years(x = YEAR.NO - 1) + months(0),
        SEMESTER == "Summer" ~ phd_start + years(x = YEAR.NO - 1) + months(5),
        SEMESTER == "Fall" ~ phd_start + years(x = YEAR.NO - 1) + months(8),
      ),
      DATE.TO = case_when(
        SEMESTER == "Spring" ~ phd_start + years(x = YEAR.NO - 1) + months(5) - days(1),
        SEMESTER == "Summer" ~ phd_start + years(x = YEAR.NO - 1) + months(8) - days(1),
        SEMESTER == "Fall" ~ phd_start + years(x = YEAR.NO - 1) + months(12) - days(1),
      )
    ) %>% 
    mutate(CATEGORY = factor(CATEGORY, 
                             levels = c("Research", "Dissemination", "Graduate program")),
           ITEM = fct_inorder(ITEM)) %>% 
    arrange(CATEGORY, ITEM)
  
  # save locally
  write_csv(timeline, file = here("data/processed/phd_timeline.csv"))
  
  # return object
  return(timeline)
  
}


# ggplots for presentations -------------------------------------------------

# PhD timeline

gg_timeline <- function() {
  
  require(ggplot2)
  require(glue)
  require(forcats)
  
  timeline <- get_phd_timeline()
  
  x_breaks <- timeline %>% 
    distinct(SEMESTER, DATE.FROM) |> 
    filter(SEMESTER != "Summer") %>% 
    arrange(DATE.FROM) %>% 
    mutate(LABEL = glue("{SEMESTER}\n{year(DATE.FROM)}"))
  
  x_breaks_years <- timeline %>% 
    distinct(YEAR.NO, DATE.FROM) |> 
    arrange(DATE.FROM) |>
    group_by(YEAR.NO) |>
    slice(1) |>
    mutate(LABEL = glue("Year {YEAR.NO}"))
  
  y_breaks_cats <- timeline %>% 
    group_by(CATEGORY) %>% 
    reframe(MIN = first(ITEM),
            MAX = last(ITEM))
  
  timeline %>% 
    left_join(y_breaks_cats, by = "CATEGORY") %>% 
    
    ggplot(mapping = aes(y = fct_rev(ITEM))) +
    
    # # colours for categories
    # geom_rect(mapping = aes(xmin = first(x_breaks_years$DATE.FROM), 
    #                         xmax = last(x_breaks_years$DATE.FROM) + years(1) - days(1),
    #                         ymin = MIN - 0.5, ymax = MAX + 0.5,
    #                         fill = CATEGORY),
    #              alpha = 0.02, colour = NA) +
    
    # year breaks
    geom_vline(mapping = aes(xintercept = x_breaks_years$DATE.FROM[1]),
               linetype = "dotted", col = "grey45") +
    geom_vline(mapping = aes(xintercept = x_breaks_years$DATE.FROM[2]),
               linetype = "dotted", col = "grey45") +
    geom_vline(mapping = aes(xintercept = x_breaks_years$DATE.FROM[3]),
               linetype = "dotted", col = "grey45") +
    geom_vline(mapping = aes(xintercept = x_breaks_years$DATE.FROM[4]),
               linetype = "dotted", col = "grey45") +
    geom_vline(mapping = aes(xintercept = x_breaks_years$DATE.FROM[5]),
               linetype = "dotted", col = "grey45") +
    
    geom_segment(mapping = aes(x = DATE.FROM, xend = DATE.TO, colour = CHAPTER), 
                 linewidth = 3, lineend = "round", 
                 position = position_dodge(width = 1)) +
    
    # show current point on timeline
    geom_vline(mapping = aes(xintercept = today()),
               col = "darkred") +
    annotate("text", 
             x = today(), y = n_distinct(timeline$ITEM), 
             label = "bold(TODAY)", parse = TRUE,
             col = "darkred", vjust = -2) +
    coord_cartesian(clip = "off") + # don't clip the vjusted text
    
    # geom_errorbarh(mapping = aes(xmin = DATE.FROM, xmax = DATE.TO),
    #                position = "dodge") +
    scale_x_date(limits = c(as_date("2025-01-01"), as_date("2030-01-01")),
                 breaks = x_breaks_years$DATE.FROM,
                 labels = x_breaks_years$LABEL,
                 expand = expansion(0, 0)) +
    scale_color_brewer(name = "Chapter", palette = "Set2", na.value = "grey25",
                       labels = c(1, 2, 3, "All")) +
    labs(y = "", x = "") +
    
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey60"),
          axis.text.x = element_text(hjust = -1, face = "bold", size = 10),
          plot.margin = margin(t = 20, r = 20),
          
          legend.position = "inside",
          legend.position.inside = c(0.85, 0.7),
          legend.direction = "horizontal",
          legend.title.position = "top",
          legend.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  # returns ggplot
  
}


# predictions

gg_pred <- function(x_lab, y_lab,
                    facet = FALSE,
                    relation = "direct") {
  
  gg_slope <- if (relation == "direct") 1 else
    if (relation == "indirect") -1 else
      stop("Enter valid relationship for trend")
  gg_intercept <- if (relation == "direct") 0 else
    if (relation == "indirect") 1 else
      stop("Enter valid relationship for trend")
  
  # accent colours
  accent <- "#6f170d"
  accent2 <- "#8b6131"
  
  df <- if (facet == TRUE) {
    data.frame(x = c(0, 0.5, 1),
               y = c(0, 0.5, 1),
               facet_var = y_lab) 
  } else {
    data.frame(x = c(0, 0.5, 1),
               y = c(0, 0.5, 1))
  }
  
  ggplot(data = df, 
         mapping = aes(x = x, y = y)) +
    # Draws a line starting at 0 with a 1-to-1 slope
    geom_abline(intercept = gg_intercept, slope = gg_slope, 
                color = accent, linewidth = 2) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    {if (facet == TRUE) {
      list(
      facet_wrap(~ facet_var, nrow = 1),
      labs(x = x_lab, y = "")
      )
    } else {
      labs(x = x_lab, y = y_lab)
    }} +
    theme_classic() +
    theme(axis.title = element_text(size = if (facet == TRUE) 18 else 16),
          strip.text = element_text(size = 16),
          strip.background = element_rect(colour = NA, fill = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
}

gg_pred_continents <- function() {
  
  # define base position for each dryland type
  dryland_centers <- data.frame(
    dryland = factor(paste("Dryland", 1:4)),
    base_x = c(2, 8, 2, 8),
    base_y = c(2, 2, 8, 8)
  )
  
  set.seed(123) # For reproducibility
  
  ggplot(
    data = expand.grid(dryland = factor(paste("Dryland", 1:4)),
                       continent = factor(paste("Continent", 1:3))) |>
      left_join(dryland_centers, by = "dryland") |>
      # slight variance for continents (tight clusters)
      mutate(PC1 = base_x + rnorm(n(), sd = 1),
             PC2 = base_y + rnorm(n(), sd = 1)),
    
    mapping = aes(x = PC1, y = PC2, shape = dryland, color = continent)
  ) +
    geom_point(size = 5, stroke = 1.5) +
    labs(x = "", y = "", shape = "", colour = "") +
    scale_color_manual(values = c("#8b6131", "#443a31", "#6f170d")) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
  
}