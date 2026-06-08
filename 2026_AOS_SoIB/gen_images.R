library(tidyverse)
library(magick)
library(here)

# create base plot --------------------------------------------------------

# set seed for random value per species/year
set.seed(1)

# repfreq values
y_values <- c(
  0, 6, sample(seq(6, 8, by = 1), 3, replace = TRUE), sample(seq(8, 10, by = 1), 4, replace = TRUE), 10,
  0, 0, sample(seq(-2, 2, by = 1), 3, replace = TRUE), sample(seq(-1, 1, by = 1), 4, replace = TRUE), 0,
  0, -5, sample(seq(-7, -10, by = -1), 7, replace = TRUE), -10
)

trends_plot <- data.frame(species = c("a", "b", "c")) |> 
  group_by(species) |> 
  reframe(year = 1:10) |> 
  bind_cols(value = y_values) |> 
  ggplot(mapping = aes(x = year, y = value, group = species)) +
  geom_point(col = "#716252", size = 3) +
  geom_line(col = "#716252", linewidth = 1, linetype = "dotdash", alpha = 0.8) + 
  theme_void() +
  theme(legend.position = "none")

ggsave(trends_plot, filename = here("2026_AOS_SoIB/pics2/title_bg_trends.png"),
       width = 16, height = 9, units = "in", bg = "transparent")

# add images --------------------------------------------------------------

# read plot using magick
trends_magick <- image_read(here("2026_AOS_SoIB/pics2/title_bg_trends.png"))

# logos
aos_logo <- image_read(here("2026_AOS_SoIB/pics2/aos2026_logo.png")) |> 
  image_resize(700)
soib_logo <- image_read(here("2026_AOS_SoIB/pics2/soib_logo.png")) |> 
  image_resize(500)

# # woodpecker
# gswo <- image_read(here("2026_AOS_SoIB/pics2/gswo_saumitra.png")) |> 
#   image_resize("x800") |> 
#   image_flop() 
# # image_rotate(-20)

# # kestrel
# euke <- image_read(here("2026_AOS_SoIB/pics2/euke_saumitra.png")) |> 
#   image_resize(800) |> 
#   image_flop()

# koel
asko <- image_read(here("2026_AOS_SoIB/pics2/asko_saumitra.png")) |> 
  image_resize(1200)


# put everything together and overwrite image
trends_magick |> 
  image_composite(soib_logo, operator = "over", offset = "+2000+1600") |> 
  image_composite(aos_logo, operator = "over", offset = "+1200+1500") |> 
  # image_composite(gswo, operator = "over", offset = "+0+100") |> 
  # image_composite(euke, operator = "over", offset = "+0+100") |>
  image_composite(asko, operator = "over", offset = "+50+30") |>
  image_write(here("2026_AOS_SoIB/pics2/title_bg_trends.png"))


# The Good ----------------------------------------------------------------

# set seed for random value per species/year
set.seed(2)

# repfreq values
y_values <- c(
  0, 6, sample(seq(6, 8, by = 1), 3, replace = TRUE), sample(seq(8, 10, by = 1), 4, replace = TRUE), 10,
  0, 2, 2, 3, 4, 5, 4, 11, 26, 30,
  0, 8, 11, 10, 17, 14, 18, 14, 17, 18
)

trends_plot <- data.frame(species = c("a", "b", "c")) |> 
  group_by(species) |> 
  reframe(year = 1:10) |> 
  bind_cols(value = y_values) |> 
  ggplot(mapping = aes(x = year, y = value, group = species)) +
  geom_point(col = "#716252", size = 3) +
  geom_line(col = "#716252", linewidth = 1, linetype = "dotdash", alpha = 0.8) + 
  theme_void() +
  theme(legend.position = "none")

ggsave(trends_plot, filename = here("2026_AOS_SoIB/pics2/section_the_good.png"),
       width = 16, height = 9, units = "in", bg = "transparent")


# The Bad ----------------------------------------------------------------

# repfreq values
y_values <- c(
  0, -2, -5, -3, -7, -9, -12, -17, -20, -25,
  0, -25, -27, -28, -27, -29, -28, -30, -29, -30
)

trends_plot <- data.frame(species = c("a", "b")) |> 
  group_by(species) |> 
  reframe(year = 1:10) |> 
  bind_cols(value = y_values) |> 
  ggplot(mapping = aes(x = year, y = value, group = species)) +
  geom_point(col = "#716252", size = 3) +
  geom_line(col = "#716252", linewidth = 1, linetype = "dotdash", alpha = 0.8) + 
  theme_void() +
  theme(legend.position = "none")

ggsave(trends_plot, filename = here("2026_AOS_SoIB/pics2/section_the_bad.png"),
       width = 16, height = 9, units = "in", bg = "transparent")

