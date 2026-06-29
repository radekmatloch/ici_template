# Do Experimental Schools Drive Housing Prices?
# Big Data and Social Analysis — NCCU, Group 10
# Radek Matloch (113266016) | Prof. Chung-pei Pien

library(tidyverse)
library(lubridate)
library(tidygeocoder)
library(geosphere)

Sys.setenv(GOOGLEGEOCODE_API_KEY = "....")

data_folder <- "C:/Users/owner/Downloads/Big Data/week 10/final/big data project"

files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
files <- files[!str_detect(basename(files), "e1_new|high\\.csv")]

raw_data <- map_dfr(files, function(f) {
  df <- read_csv(f,
                 locale = locale(encoding = "UTF-8"),
                 skip = 1,
                 col_names = c(
                   "district", "transaction_type", "address",
                   "land_area", "urban_zone", "non_urban_zone", "non_urban_use",
                   "transaction_date", "transaction_count", "floor",
                   "total_floors", "building_type", "main_use",
                   "main_material", "construction_date", "building_area",
                   "rooms", "halls", "bathrooms", "partitions",
                   "has_management", "total_price", "unit_price",
                   "parking_type", "parking_area", "parking_price",
                   "notes", "serial", "main_building_area",
                   "attached_area", "balcony_area", "elevator", "transfer_id"
                 ),
                 col_types = cols(.default = "c"),
                 show_col_types = FALSE)
  df %>%
    mutate(source_file = basename(f)) %>%
    filter(district != "The villages and towns urban district")
})

cat("Rows loaded:", nrow(raw_data), "| Files:", length(files), "\n")

clean_data <- raw_data %>%
  mutate(
    total_price        = as.numeric(total_price),
    unit_price         = as.numeric(unit_price),
    building_area      = as.numeric(building_area),
    rooms              = as.numeric(rooms),
    halls              = as.numeric(halls),
    bathrooms          = as.numeric(bathrooms),
    main_building_area = as.numeric(main_building_area)
  ) %>%
  mutate(
    date_str     = as.character(transaction_date),
    roc_year     = as.numeric(substr(date_str, 1, nchar(date_str) - 4)),
    month        = as.numeric(substr(date_str, nchar(date_str) - 3, nchar(date_str) - 2)),
    day          = as.numeric(substr(date_str, nchar(date_str) - 1, nchar(date_str))),
    western_year = roc_year + 1911,
    date         = make_date(western_year, month, day)
  ) %>%
  mutate(
    construction_str      = as.character(construction_date),
    construction_roc_year = as.numeric(substr(construction_str, 1, nchar(construction_str) - 4)),
    construction_year     = construction_roc_year + 1911,
    building_age          = western_year - construction_year
  ) %>%
  mutate(
    year    = western_year,
    quarter = paste0(year, "Q", quarter(date))
  ) %>%
  filter(
    !is.na(unit_price),
    unit_price > 0,
    !is.na(date),
    year >= 2015 & year <= 2024,
    building_area > 0
  )

cat("Rows after cleaning:", nrow(clean_data), "\n")
cat("Year range:", min(clean_data$year), "-", max(clean_data$year), "\n")

exp_schools <- tibble(
  school_name = c(
    "博嘉實驗國小", "泉源實驗國小", "芳和實驗中學",
    "溪山實驗國小", "湖田實驗國小", "民族實驗國中",
    "濱江實驗國中", "指南實驗國小", "西湖實驗國中"
  ),
  district = c(
    "文山區", "北投區", "大安區",
    "士林區", "士林區", "大安區",
    "中山區", "文山區", "內湖區"
  ),
  level = c(
    "elementary", "elementary", "junior_high",
    "elementary", "elementary", "junior_high",
    "junior_high", "elementary", "junior_high"
  ),
  lat = c(
    25.002350, 25.149457, 25.018639,
    25.119791, 25.168429, 25.011183,
    25.079550, 24.976793, 25.085836
  ),
  lng = c(
    121.575728, 121.524397, 121.550250,
    121.579610, 121.538771, 121.538830,
    121.560832, 121.583568, 121.565670
  ),
  school_type     = "experimental",
  conversion_year = c(2017, 2018, 2018, 2019, 2019, 2019, 2020, 2022, 2022)
)

exp_districts   <- unique(exp_schools$district)
exp_school_names <- exp_schools$school_name

elem_raw   <- read_csv(file.path(data_folder, "e1_new.csv"),
                       locale = locale(encoding = "UTF-8"),
                       show_col_types = FALSE)
junior_raw <- read_csv(file.path(data_folder, "high.csv"),
                       locale = locale(encoding = "UTF-8"),
                       show_col_types = FALSE)

clean_address <- function(addr) {
  addr %>%
    str_replace("^\\[\\d+\\]", "") %>%
    str_replace("(?<=區)[\\p{Han}]{1,4}里", "") %>%
    str_trim()
}

regular_elem <- elem_raw %>%
  filter(str_detect(縣市名稱, "臺北|台北"), `公/私立` == "公立") %>%
  mutate(
    address     = clean_address(地址),
    district    = str_extract(address, "(?<=臺北市|台北市)\\S+區"),
    school_name = 學校名稱,
    school_type = "regular"
  ) %>%
  filter(district %in% exp_districts, !school_name %in% exp_school_names) %>%
  select(school_name, district, address, school_type)

regular_junior <- junior_raw %>%
  filter(str_detect(縣市名稱, "臺北|台北"), `公/私立` == "公立") %>%
  mutate(
    address     = clean_address(地址),
    district    = str_extract(address, "(?<=臺北市|台北市)\\S+區"),
    school_name = 學校名稱,
    school_type = "regular"
  ) %>%
  filter(district %in% exp_districts, !school_name %in% exp_school_names) %>%
  select(school_name, district, address, school_type)

regular_schools <- bind_rows(regular_elem, regular_junior)
cat("Regular schools:", nrow(regular_schools), "\n")

geocode_cache_file <- "C:/Users/owner/Downloads/Big Data/week 10/final/school_coordinates.csv"

if (file.exists(geocode_cache_file)) {
  reg_schools_geo <- read_csv(geocode_cache_file, show_col_types = FALSE)
  cat("Loaded school coordinates from cache:", nrow(reg_schools_geo), "schools\n")

  # retry any that previously failed (NA lat), plus any new schools not in cache
  already_failed <- reg_schools_geo %>% filter(is.na(lat)) %>% pull(school_name)
  reg_schools_geo <- reg_schools_geo %>% filter(!school_name %in% already_failed)

  missing <- regular_schools %>%
    filter(!school_name %in% reg_schools_geo$school_name | school_name %in% already_failed)

  if (nrow(missing) > 0) {
    cat("Geocoding", nrow(missing), "schools via Google Maps...\n")
    new_geo <- missing %>%
      geocode(address = address, method = "google",
              lat = "lat", long = "lng",
              full_results = FALSE) %>%
      select(school_name, district, address, school_type, lat, lng)
    reg_schools_geo <- bind_rows(reg_schools_geo, new_geo)
    write_csv(reg_schools_geo, geocode_cache_file)
  }

} else {
  cat("Geocoding", nrow(regular_schools), "regular schools via Google Maps...\n")
  reg_schools_geo <- regular_schools %>%
    geocode(address = address, method = "google",
            lat = "lat", long = "lng",
            full_results = FALSE) %>%
    select(school_name, district, address, school_type, lat, lng)
  write_csv(reg_schools_geo, geocode_cache_file)
}

n_geocoded <- sum(!is.na(reg_schools_geo$lat))
cat(sprintf("Geocoded: %d / %d regular schools\n", n_geocoded, nrow(reg_schools_geo)))

failed <- reg_schools_geo %>% filter(is.na(lat))
if (nrow(failed) > 0) {
  cat("Failed:\n")
  print(failed %>% select(school_name, district, address))
}

all_schools <- bind_rows(
  exp_schools %>% select(school_name, district, lat, lng, school_type, conversion_year),
  reg_schools_geo %>%
    filter(!is.na(lat)) %>%
    mutate(conversion_year = NA_real_) %>%
    select(school_name, district, lat, lng, school_type, conversion_year)
)

cat("Schools — experimental:", sum(all_schools$school_type == "experimental"),
    "| regular:", sum(all_schools$school_type == "regular"), "\n")

road_section_cache <- "C:/Users/owner/Downloads/Big Data/week 10/final/road_section_coordinates.csv"

props_geo <- clean_data %>%
  filter(district %in% exp_districts) %>%
  mutate(
    road         = str_extract(address, "[\\p{Han}]+[路街道]"),
    section      = str_extract(address, "[\\p{Han}]+[路街道][\\d一二三四五六七八九十]+段"),
    road_section = if_else(!is.na(section), section, road)
  )

unique_segments <- props_geo %>%
  filter(!is.na(road_section)) %>%
  distinct(district, road_section) %>%
  mutate(query = paste0("臺北市", district, road_section))

cat("Unique road sections:", nrow(unique_segments), "\n")

if (file.exists(road_section_cache)) {
  seg_coords <- read_csv(road_section_cache, show_col_types = FALSE)
  cat("Loaded road-section cache:", nrow(seg_coords), "segments\n")

  missing_segs <- unique_segments %>%
    filter(!paste(district, road_section) %in% paste(seg_coords$district, seg_coords$road_section))

  if (nrow(missing_segs) > 0) {
    cat("Geocoding", nrow(missing_segs), "new segments via ArcGIS...\n")
    new_segs <- missing_segs %>%
      geocode(address = query, method = "arcgis", lat = "seg_lat", long = "seg_lng") %>%
      select(district, road_section, seg_lat, seg_lng)
    seg_coords <- bind_rows(seg_coords, new_segs)
    write_csv(seg_coords, road_section_cache)
  }
} else {
  cat("Geocoding", nrow(unique_segments), "road sections via ArcGIS...\n")
  seg_coords <- unique_segments %>%
    geocode(address = query, method = "arcgis", lat = "seg_lat", long = "seg_lng") %>%
    select(district, road_section, seg_lat, seg_lng)
  write_csv(seg_coords, road_section_cache)
}

cat(sprintf("Road sections geocoded: %d / %d\n",
            sum(!is.na(seg_coords$seg_lat)), nrow(seg_coords)))

analysis_geo <- props_geo %>%
  left_join(seg_coords, by = c("district", "road_section")) %>%
  filter(!is.na(seg_lat))

cat("Properties with coordinates:", nrow(analysis_geo), "\n")

cat("Calculating distances...\n")

exp_by_district <- all_schools %>% filter(school_type == "experimental") %>% split(.$district)
reg_by_district <- all_schools %>% filter(school_type == "regular") %>% split(.$district)

calc_min_dist <- function(plat, plng, school_df) {
  if (is.null(school_df) || nrow(school_df) == 0) return(NA_real_)
  min(distHaversine(c(plng, plat), cbind(school_df$lng, school_df$lat)))
}

calc_nearest_name <- function(plat, plng, school_df) {
  if (is.null(school_df) || nrow(school_df) == 0) return(NA_character_)
  dists <- distHaversine(c(plng, plat), cbind(school_df$lng, school_df$lat))
  school_df$school_name[which.min(dists)]
}

results <- list()
for (d in exp_districts) {
  cat(" ", d, "...")
  d_data <- analysis_geo %>% filter(district == d)
  d_exp  <- exp_by_district[[d]]
  d_reg  <- reg_by_district[[d]]

  if (is.null(d_exp) || is.null(d_reg)) { cat(" skipped\n"); next }

  d_data <- d_data %>%
    rowwise() %>%
    mutate(
      dist_to_nearest_exp = calc_min_dist(seg_lat, seg_lng, d_exp),
      dist_to_nearest_reg = calc_min_dist(seg_lat, seg_lng, d_reg),
      nearest_exp_school  = calc_nearest_name(seg_lat, seg_lng, d_exp)
    ) %>%
    ungroup()

  results[[d]] <- d_data
  cat(" done (", nrow(d_data), ")\n")
}

analysis_geo <- bind_rows(results)
cat("Distance calculation done:", nrow(analysis_geo), "properties\n")

for (r in c(500, 1000, 1500, 2000, 3000)) {
  near_exp <- sum(analysis_geo$dist_to_nearest_exp < r, na.rm = TRUE)
  near_reg <- sum(analysis_geo$dist_to_nearest_reg < r &
                    analysis_geo$dist_to_nearest_exp >= r, na.rm = TRUE)
  cat(sprintf("Radius %4dm: %5d near experimental | %5d near regular\n", r, near_exp, near_reg))
}

RADIUS <- 1000

district_conversion <- exp_schools %>%
  group_by(district) %>%
  summarise(conversion_year = min(conversion_year), .groups = "drop")

comparison_data <- analysis_geo %>%
  mutate(
    near_experimental = dist_to_nearest_exp < RADIUS,
    near_regular      = dist_to_nearest_reg < RADIUS,
    nearest_school_type = case_when(
      near_experimental                  ~ "experimental",
      near_regular & !near_experimental  ~ "regular",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(nearest_school_type)) %>%
  left_join(district_conversion, by = "district") %>%
  mutate(
    period = factor(
      case_when(
        year < conversion_year  ~ "before",
        year >= conversion_year ~ "after"
      ),
      levels = c("before", "after")
    )
  )

comparison_data %>% count(nearest_school_type) %>% print()

# district-level baseline (plots 1-6)
analysis_data <- clean_data %>%
  mutate(has_experimental = district %in% exp_districts) %>%
  left_join(district_conversion, by = "district") %>%
  mutate(
    period = case_when(
      !has_experimental       ~ "control",
      year < conversion_year  ~ "before",
      year >= conversion_year ~ "after",
      TRUE                    ~ "control"
    )
  )

yearly_trends <- analysis_data %>%
  group_by(has_experimental, year) %>%
  summarise(n = n(), median_price = median(unit_price), .groups = "drop")

growth <- yearly_trends %>%
  arrange(has_experimental, year) %>%
  group_by(has_experimental) %>%
  mutate(yoy_growth = (median_price - lag(median_price)) / lag(median_price) * 100) %>%
  filter(!is.na(yoy_growth))

matched_district <- analysis_data %>%
  filter(str_detect(building_type, "住宅大樓|華廈"),
         rooms >= 2, rooms <= 4,
         building_age >= 0, building_age <= 40) %>%
  group_by(has_experimental, year) %>%
  summarise(n = n(), median_price = median(unit_price), .groups = "drop")

p1 <- ggplot(yearly_trends,
             aes(x = year, y = median_price, color = has_experimental)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_color_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
    labels = c("TRUE" = "Experimental School Districts",
               "FALSE" = "Non-Experimental Districts")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Median Housing Price: Experimental vs. Non-Experimental Districts",
       subtitle = "Taipei City, 2015-2023",
       x = "Year", y = "Median Unit Price (NTD/sqm)", color = "District Type") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot1_price_trends.png", p1, width = 10, height = 6, dpi = 200)

district_year <- analysis_data %>%
  group_by(district, year) %>%
  summarise(median_price = median(unit_price), .groups = "drop")

p2 <- ggplot(district_year,
             aes(x = year, y = reorder(district, median_price), fill = median_price)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276", labels = scales::comma) +
  labs(title = "Median Housing Price by District Over Time",
       subtitle = "Taipei City, 2015-2023",
       x = "Year", y = "District", fill = "Median Price\n(NTD/sqm)") +
  theme_minimal()
ggsave("plot2_district_heatmap.png", p2, width = 10, height = 6, dpi = 200)

p3 <- ggplot(growth, aes(x = year, y = yoy_growth, fill = has_experimental)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
    labels = c("TRUE" = "Experimental Districts", "FALSE" = "Non-Experimental Districts")
  ) +
  labs(title = "Year-over-Year Price Growth: Experimental vs. Non-Experimental Districts",
       x = "Year", y = "YoY Growth (%)", fill = "District Type") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot3_growth_rates.png", p3, width = 10, height = 6, dpi = 200)

p4 <- analysis_data %>%
  filter(str_detect(building_type, "住宅大樓|華廈")) %>%
  ggplot(aes(x = has_experimental, y = unit_price, fill = has_experimental)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
    labels = c("TRUE" = "Experimental", "FALSE" = "Non-Experimental")
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 500000)) +
  labs(title = "Price Distribution: Experimental vs. Non-Experimental Districts",
       subtitle = "Apartments only (住宅大樓 + 華廈)",
       x = "District Has Experimental School",
       y = "Unit Price (NTD/sqm)", fill = "District Type") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot4_boxplot.png", p4, width = 8, height = 6, dpi = 200)

district_before_after <- analysis_data %>%
  filter(has_experimental == TRUE, period %in% c("before", "after")) %>%
  group_by(district, period) %>%
  summarise(median_price = median(unit_price), n = n(), .groups = "drop")

p5 <- ggplot(district_before_after,
             aes(x = period, y = median_price, group = district, color = district)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Before vs. After School Conversion: Price Change by District",
       subtitle = "Experimental school districts only",
       x = "Period", y = "Median Unit Price (NTD/sqm)", color = "District") +
  theme_minimal()
ggsave("plot5_before_after_districts.png", p5, width = 10, height = 6, dpi = 200)

p6 <- ggplot(matched_district,
             aes(x = year, y = median_price, color = has_experimental)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_color_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
    labels = c("TRUE" = "Experimental Districts", "FALSE" = "Non-Experimental Districts")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Matched Comparison: 2-4 Room Apartments Only",
       subtitle = "Controlling for property type and size",
       x = "Year", y = "Median Unit Price (NTD/sqm)", color = "District Type") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot6_matched.png", p6, width = 10, height = 6, dpi = 200)

# proximity analysis (plots 7-10)
p7 <- comparison_data %>%
  group_by(nearest_school_type, year) %>%
  summarise(median_price = median(unit_price), .groups = "drop") %>%
  ggplot(aes(x = year, y = median_price, color = nearest_school_type)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_color_manual(
    values = c("experimental" = "#E74C3C", "regular" = "#3498DB"),
    labels = c("experimental" = "Near Experimental School",
               "regular" = "Near Regular School")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Within-District: Prices Near Experimental vs. Regular Schools",
       subtitle = paste0("Properties within ", RADIUS/1000, "km radius, same district"),
       x = "Year", y = "Median Unit Price (NTD/sqm)", color = "Nearest School Type") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot7_proximity_comparison.png", p7, width = 10, height = 6, dpi = 200)

by_district_premium <- comparison_data %>%
  group_by(district, nearest_school_type) %>%
  summarise(median_price = median(unit_price), .groups = "drop") %>%
  pivot_wider(names_from = nearest_school_type, values_from = median_price, values_fill = NA) %>%
  mutate(
    experimental = if ("experimental" %in% names(.)) experimental else NA_real_,
    regular      = if ("regular" %in% names(.)) regular else NA_real_,
    premium_pct  = (experimental - regular) / regular * 100
  )

p8 <- by_district_premium %>%
  filter(!is.na(premium_pct)) %>%
  ggplot(aes(x = reorder(district, premium_pct), y = premium_pct)) +
  geom_col(fill = "#2C3E50") + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Experimental School Price Premium by District",
       subtitle = paste0("% price difference within ", RADIUS/1000, "km radius"),
       x = "District", y = "Price Premium (%)") +
  theme_minimal()
ggsave("plot8_premium_by_district.png", p8, width = 8, height = 5, dpi = 200)

p9 <- comparison_data %>%
  filter(!is.na(period)) %>%
  group_by(district, nearest_school_type, period) %>%
  summarise(median_price = median(unit_price), .groups = "drop") %>%
  ggplot(aes(x = period, y = median_price,
             color = nearest_school_type, group = nearest_school_type)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  facet_wrap(~district, scales = "free_y") +
  scale_color_manual(values = c("experimental" = "#E74C3C", "regular" = "#3498DB")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Before/After School Conversion: By District",
       subtitle = "Comparing properties near experimental vs. regular schools",
       x = "Period", y = "Median Unit Price (NTD/sqm)", color = "Nearest School") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot9_before_after_proximity.png", p9, width = 12, height = 8, dpi = 200)

matched_prox <- comparison_data %>%
  filter(str_detect(building_type, "住宅大樓|華廈"), rooms >= 2, rooms <= 4) %>%
  group_by(nearest_school_type, year) %>%
  summarise(n = n(), median_price = median(unit_price), .groups = "drop")

p10 <- matched_prox %>%
  ggplot(aes(x = year, y = median_price, color = nearest_school_type)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_color_manual(
    values = c("experimental" = "#E74C3C", "regular" = "#3498DB"),
    labels = c("experimental" = "Near Experimental", "regular" = "Near Regular")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Matched Proximity Comparison: 2-4 Room Apartments",
       subtitle = paste0("Same district, within ", RADIUS/1000, "km, controlling for property type"),
       x = "Year", y = "Median Unit Price (NTD/sqm)", color = "Nearest School") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("plot10_matched_proximity.png", p10, width = 10, height = 6, dpi = 200)

# per-school analysis (plots 11-13)
comparison_matched <- comparison_data %>%
  filter(
    str_detect(building_type, "住宅大樓|華廈"),
    rooms >= 2, rooms <= 4,
    building_age >= 0, building_age <= 40
  )

cat("Matched properties:", nrow(comparison_matched), "\n")
comparison_matched %>% count(nearest_school_type) %>% print()

per_school_summary <- comparison_matched %>%
  group_by(nearest_exp_school, nearest_school_type) %>%
  summarise(n = n(), median_price = median(unit_price), .groups = "drop") %>%
  pivot_wider(names_from = nearest_school_type, values_from = c(n, median_price)) %>%
  mutate(premium_pct = (median_price_experimental - median_price_regular) /
           median_price_regular * 100)

print(per_school_summary)

# DiD table — (exp_after - exp_before) - (reg_after - reg_before)
MIN_EXP_PROPERTIES <- 100
excluded_schools <- per_school_summary %>%
  filter(is.na(n_experimental) | n_experimental < MIN_EXP_PROPERTIES) %>%
  pull(nearest_exp_school)

cat("Excluded (< 100 exp properties):", paste(excluded_schools, collapse = ", "), "\n")

did_table <- comparison_matched %>%
  left_join(exp_schools %>% select(school_name, conversion_year),
            by = c("nearest_exp_school" = "school_name")) %>%
  mutate(
    conversion_year = coalesce(conversion_year.x, conversion_year.y),
    period = case_when(
      year < conversion_year  ~ "before",
      year >= conversion_year ~ "after"
    )
  ) %>%
  filter(!is.na(period), !nearest_exp_school %in% excluded_schools) %>%
  group_by(nearest_exp_school, nearest_school_type, period) %>%
  summarise(median_price = median(unit_price), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(nearest_school_type, period),
              values_from = c(median_price, n)) %>%
  mutate(
    exp_growth_pct = (median_price_experimental_after - median_price_experimental_before) /
                      median_price_experimental_before * 100,
    reg_growth_pct = (median_price_regular_after - median_price_regular_before) /
                      median_price_regular_before * 100,
    did_pct        = exp_growth_pct - reg_growth_pct
  ) %>%
  select(
    nearest_exp_school,
    exp_before = median_price_experimental_before,
    exp_after  = median_price_experimental_after,
    exp_growth = exp_growth_pct,
    reg_before = median_price_regular_before,
    reg_after  = median_price_regular_after,
    reg_growth = reg_growth_pct,
    did        = did_pct
  ) %>%
  arrange(desc(did))

cat(sprintf("\n%-16s %8s %8s %8s | %8s %8s %8s | %8s\n",
            "School", "Exp.Bef", "Exp.Aft", "Exp.Gr%",
            "Reg.Bef", "Reg.Aft", "Reg.Gr%", "DiD%"))
cat(strrep("-", 85), "\n")
for (i in seq_len(nrow(did_table))) {
  r <- did_table[i, ]
  cat(sprintf("%-16s %8.0f %8.0f %7.1f%% | %8.0f %8.0f %7.1f%% | %7.1f%%\n",
              r$nearest_exp_school,
              r$exp_before, r$exp_after, r$exp_growth,
              r$reg_before, r$reg_after, r$reg_growth,
              r$did))
}
cat(strrep("-", 85), "\n")
cat(sprintf("%-16s %8s %8s %7.1f%% | %8s %8s %7.1f%% | %7.1f%%\n",
            "AVERAGE", "", "",
            mean(did_table$exp_growth, na.rm = TRUE),
            "", "",
            mean(did_table$reg_growth, na.rm = TRUE),
            mean(did_table$did, na.rm = TRUE)))

per_school_summary_plot <- per_school_summary %>%
  filter(!nearest_exp_school %in% excluded_schools, !is.na(premium_pct))

p11 <- per_school_summary_plot %>%
  ggplot(aes(x = reorder(nearest_exp_school, premium_pct), y = premium_pct)) +
  geom_col(fill = "#2C3E50") + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Price Premium by Individual Experimental School",
       subtitle = paste0("% price difference within ", RADIUS/1000, "km vs. nearest regular school"),
       x = "Experimental School", y = "Price Premium (%)") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10))
ggsave("plot11_per_school_premium.png", p11, width = 10, height = 6, dpi = 200)

per_school_ba <- comparison_matched %>%
  filter(!nearest_exp_school %in% excluded_schools) %>%
  left_join(exp_schools %>% select(school_name, conversion_year),
            by = c("nearest_exp_school" = "school_name")) %>%
  mutate(conversion_year = coalesce(conversion_year.x, conversion_year.y)) %>%
  mutate(period = factor(
    case_when(
      year < conversion_year  ~ "before",
      year >= conversion_year ~ "after"
    ), levels = c("before", "after")
  )) %>%
  filter(!is.na(period)) %>%
  group_by(nearest_exp_school, nearest_school_type, period) %>%
  summarise(n = n(), median_price = median(unit_price), .groups = "drop")

p12 <- per_school_ba %>%
  ggplot(aes(x = period, y = median_price,
             color = nearest_school_type, group = nearest_school_type)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  facet_wrap(~nearest_exp_school, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("experimental" = "#E74C3C", "regular" = "#3498DB"),
    labels = c("experimental" = "Near Experimental", "regular" = "Near Regular")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Before/After School Conversion: By Individual School",
       subtitle = "2-4 room apartments only — controlling for property type",
       x = "Period", y = "Median Unit Price (NTD/sqm)", color = "Nearest School") +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(size = 9))
ggsave("plot12_per_school_before_after.png", p12, width = 12, height = 8, dpi = 200)

per_school_yearly <- comparison_matched %>%
  filter(!nearest_exp_school %in% excluded_schools) %>%
  group_by(nearest_exp_school, nearest_school_type, year) %>%
  summarise(n = n(), median_price = median(unit_price), .groups = "drop")

p13 <- per_school_yearly %>%
  ggplot(aes(x = year, y = median_price, color = nearest_school_type)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~nearest_exp_school, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("experimental" = "#E74C3C", "regular" = "#3498DB"),
    labels = c("experimental" = "Near Experimental", "regular" = "Near Regular")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Yearly Price Trends: By Individual Experimental School",
       subtitle = paste0("Properties within ", RADIUS/1000, "km, same district"),
       x = "Year", y = "Median Unit Price (NTD/sqm)", color = "Nearest School") +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(size = 9))
ggsave("plot13_per_school_yearly.png", p13, width = 12, height = 8, dpi = 200)

cat("\nDone. Transactions loaded:", nrow(clean_data),
    "| With coordinates:", nrow(analysis_geo),
    "| In comparison:", nrow(comparison_data), "\n")
