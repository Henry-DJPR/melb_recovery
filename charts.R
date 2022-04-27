
# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggseas)
pacman::p_load_gh("djpr-data/djprtheme")




# Download/unzip data
google_url   <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip" 
google_dest  <- tempfile(fileext = ".zip")
google_files <- paste0(2020:2022, "_AU_Region_Mobility_Report.csv")

download.file(google_url, google_dest, mode = "wb")

google_paths <- unzip(
  zipfile = google_dest, 
  files   = google_files, 
  exdir   = dirname(google_dest)
  )



# Parse data
google_mobility <- map_df(google_paths, read_csv)




# Clean data
google_mobility <- google_mobility %>% 
  filter(str_detect(sub_region_2, "Melbourne")) %>% 
  rename_with(~str_remove(., "_percent_change_from_baseline")) %>% 
  select(-matches("region|code|id|area")) #%>% 
  # mutate(date = ceiling_date(date, "week")) %>% 
  # group_by(date) %>% 
  # filter(n() == 7) %>% 
  # summarise(across(everything(), mean)) %>% 
  # ungroup()

google_ts <- as.ts(google_mobility$workplaces, start= c(2020,))



# reindex google data
google_mobility <- google_mobility %>%
  filter(date >= date[which.min(workplaces)]) %>%
  mutate(across(-date, ~. / first(.) * -100 + 100))



# Plot
ggplot(google_mobility, aes(date, workplaces)) +
  geom_area()
