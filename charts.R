
# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, svglite, rvest, lubridate, ggrepel, readabs)
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




# Download industry employment (not in ts directory for read_abs for some reason)
abs_ind_url <- "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/mar-2022/6291004.xlsx"
abs_ind_dest <- tempfile(fileext = ".xlsx")

if(!file.exists(abs_ind_dest)){
  download.file(abs_ind_url, abs_ind_dest, mode = "wb")
}




# Parse all data
google_mobility <- map_df(google_paths, read_csv)
pca_occupancy <- html_table(pca_page)[[2]]
abs_industry_vac <- read_abs("6354.0", 4)
clue_melb_ind <- read_csv(
  "https://data.melbourne.vic.gov.au/resource/nw38-8y7g.csv"
  )
abs_industry_n <- read_abs_local(
  path = dirname(abs_ind_dest), 
  filenames = basename(abs_ind_dest)
  )




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




# Clean CLUE data
industry_recode <-c(
  "accommodation_and_food" = "Accommodation and Food Services",
  "administrative_and_support" = "Administrative and Support Services",
  "agriculture_forestry_and" = "Agriculture, Forestry and Fishing",
  "arts_and_recreation_services" = "Arts and Recreation Services",
  "construction" = "Construction",
  "education_and_training" = "Education and Training",
  "electricity_gas_water_and" = "Electricity, Gas, Water and Waste Services",
  "financial_and_insurance" = "Financial and Insurance Services",
  "health_care_and_social" = "Health Care and Social Assistance",
  "information_media_and" = "Information Media and Telecommunications",
  "manufacturing" = "Manufacturing",
  "mining" = "Mining",
  "other_services" = "Other Services",
  "professional_scientific_and" = "Professional, Scientific and Technical Services",
  "public_administration_and" = "Public Administration and Safety",
  "rental_hiring_and_real_estate" = "Rental, Hiring and Real Estate Services",
  "retail_trade" = "Retail Trade",
  "transport_postal_and" = "Transport, Postal and Warehousing",
  "wholesale_trade" = "Wholesale Trade"
)

clue_melb_ind <- clue_melb_ind %>% 
  filter(clue_small_area == "Melbourne (CBD)") %>% 
  select(
    -census_year, 
    -block_id, 
    -clue_small_area, 
    -total_employment_in_block
    ) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) %>% 
  pivot_longer(everything(), names_to = "industry", values_to = "melb_emp") %>% 
  mutate(industry = recode(industry, !!!industry_recode))




# Clean ABS job vacancy data
abs_industry_vac <- abs_industry_vac %>% 
  filter(str_detect(series, "^Job Vacancies")) %>% 
  filter(date == max(date)) %>% 
  mutate(
    vacancies = value * 1000,
    industry = series %>% 
      str_remove_all("Job Vacancies|;") %>% 
      str_squish()
  ) %>% 
  select(industry, vacancies) %>% 
  filter(!str_detect(industry, "Total"))




# Clean ABS industry employment
composition_ref_date <- as.Date("2020-12-01")
closest_date <- abs_industry_vac$date[
  which.min(abs(abs_industry_vac$date - composition_ref_date))
]

closest_date <- abs_industry_n$date[
  which.min(abs(abs_industry_n$date - composition_ref_date))
]
abs_industry_n <- abs_industry_n %>% 
  filter(series_type == "Original", date == closest_date) %>% 
  select(industry = series, tot_emp = value) %>% 
  mutate(
    tot_emp = tot_emp * 1000,
    industry = industry %>% 
      str_remove_all("Employed total|;") %>% 
      str_squish()
  ) %>% 
  filter(industry != "")




# Join MELB empty jobs data
empty_jobs <- abs_industry_n %>% 
  full_join(abs_industry_vac) %>% 
  full_join(clue_melb_ind) %>% 
  mutate(
    empty_jobs = melb_emp / tot_emp * vacancies,
    empty_jobs = ifelse(is.na(empty_jobs), 0, empty_jobs),
    industry_label = recode(
      industry,
      "Agriculture, Forestry and Fishing" = "Agriculture",
      "Mining" = "Mining",
      "Manufacturing" = "Manufacturing",
      "Electricity, Gas, Water and Waste Services" = "Utilities",
      "Construction" = "Construction",
      "Wholesale Trade" = "Wholesale",
      "Retail Trade" = "Retail",
      "Accommodation and Food Services" = "Food & accom.",
      "Transport, Postal and Warehousing" = "Logistics",
      "Information Media and Telecommunications" = "IT",
      "Financial and Insurance Services" = "Finance",
      "Rental, Hiring and Real Estate Services" = "Real estate",
      "Professional, Scientific and Technical Services" = "Prof. services",
      "Administrative and Support Services" = "Admin",
      "Public Administration and Safety" = "Government",
      "Education and Training" = "Education",
      "Health Care and Social Assistance" = "Health",
      "Arts and Recreation Services" = "Arts & rec.",
      "Other Services" = "Other Services"
    ) 
    ) %>% 
  arrange(empty_jobs) %>% 
  mutate(industry_label = factor(industry_label, levels = industry_label))
  





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

chart_empty_jobs <- empty_jobs %>% 
  ggplot(aes(industry_label, empty_jobs, label = round(empty_jobs))) +
  geom_col()+
  geom_text(hjust = 0) +
  scale_y_discrete(expand = c(0, 0, 0.5, 0))+
  theme_djpr() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_blank()
  )+
  labs(
    title = "Estimate unfilled positions in CBD",
    subtitle = "Number of job vacancies by industry",
    x = NULL,
    y = NULL
  )+
  coord_flip()

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

ggsave(
  filename = "output/empty jobs.svg", 
  plot = chart_empty_jobs,
  width = svg_width, 
  height = svg_height, 
  units = ppt_units
)
