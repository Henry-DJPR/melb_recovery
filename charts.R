
# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, svglite, rvest, lubridate, ggrepel)
pacman::p_load_gh("djpr-data/djprtheme")




# Download/unzip google mobility data
google_url   <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip" 
google_dest  <- tempfile(fileext = ".zip")
google_files <- paste0(2020:2022, "_AU_Region_Mobility_Report.csv")


if(!file.exists(google_dest)){
  download.file(google_url, google_dest, mode = "wb")
}

google_paths <- unzip(
  zipfile = google_dest, 
  files   = google_files, 
  exdir   = dirname(google_dest)
  )




# Scrape PCA occupancy data
pca_url <- "https://www.propertycouncil.com.au/Web/Content/Media_Release/National/2022/Encouraging_signs_in_return_to_office_as_occupancy_rates_in_most_cities_rise-significantly.aspx"
pca_page <- read_html(pca_url)





# Parse all data
google_mobility <- map_df(google_paths, read_csv)
pca_occupancy <- html_table(pca_page)[[2]]




# Clean mobility data 
google_mobility <- google_mobility %>% 
  filter(str_detect(sub_region_2, "Melbourne")) %>% 
  rename_with(~str_remove(., "_percent_change_from_baseline")) %>% 
  select(-matches("region|code|id|area")) %>% 
  mutate(date = ceiling_date(date, "month")) %>%
  group_by(date) %>%
  filter(n() >= 28) %>%
  summarise(across(everything(), mean)) %>%
  ungroup()




# Extract pca occupancy column names from the first row 
names(pca_occupancy) <- unlist(pca_occupancy[1,])
names(pca_occupancy)[1] <- "city"

pca_occupancy <- pca_occupancy %>%
  filter(city != "")




# Clean PCA occupancy data
pca_occupancy <- pca_occupancy %>% 
  mutate(city = str_remove_all(city, "[:blank:]|[:space:]|CBD")) %>% 
  pivot_longer(!city, names_to = "date", values_to = "occupancy") %>% 
  mutate(
    date = my(date),
    city = recode(city, "Canberra" = "Canb"),
    occupancy = occupancy %>% 
      str_remove("%") %>% 
      as.numeric() %>% 
      `/`(100)
  )




# plot defaults
ppt_width <- 33.867
ppt_height <- 19.05
ppt_units <- "cm"

svg_width <- ppt_width / 2
svg_height <- ppt_height /2




# Charts
chart_mobility <- ggplot(google_mobility, aes(date, workplaces)) +
  geom_area() +
  labs(
    x = NULL, 
    title = "Melbourne workplace visits", 
    subtitle = "% differnce in visits since pre-covid" 
  ) +
  djprtheme::theme_djpr()

# ggplot(pca_occupancy, aes(date, occupancy, colour = city)) +
#   geom_line() +
#   theme_djpr() +
#   djpr_colour_manual(
#     length(unique(pca_occupancy$city))
#   ) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   geom_label_repel(
#     data = pca_occupancy %>% filter(date == max(date)),
#     aes(label = city),
#     hjust = 0,
#     nudge_x = 13,
#     label.padding = 0.01,
#     label.size = NA,
#     lineheight = 0.9,
#     point.padding = unit(0, "lines"),
#     direction = "y",
#     seed = 123,
#     show.legend = FALSE,
#     min.segment.length = unit(5, "lines"),
#     size = 14 / .pt
#   ) +
#   scale_x_date(
#     expand = expansion(
#       mult = 0.2
#     ),
#     date_labels = "%b\n%Y"
#   )

chart_occupancy <- pca_occupancy %>% 
  filter(city == "Melb") %>% 
  ggplot(aes(date, occupancy)) +
  # geom_area() +
  geom_area(stat = "smooth", span = 0.3) +
  theme_djpr() +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0, 0.05, 0)) +
  labs(
    title = "Melbourne CBD office occupancy rate", 
    subtitle = "% offices with tennants",
    x = NULL
    )


ggsave(
  filename = "output/workplace vists.svg", 
  plot = chart_mobility,
  width = svg_width, 
  height = svg_height, 
  units = ppt_units
  )

ggsave(
  filename = "output/CBD occupancy.svg", 
  plot = chart_occupancy,
  width = svg_width, 
  height = svg_height, 
  units = ppt_units
)
