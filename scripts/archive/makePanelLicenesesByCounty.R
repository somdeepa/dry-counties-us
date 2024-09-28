# make county-year panel of liquor establishments

# Last updated: 21-04-2024

# load tax data and parse dates

liquortax <- read.csv("data/raw/liquor license/Tax_Assessor-Collector_Licenses_Permits_Issued_20240417.csv") %>% 
  select(clp, clp_primary, county_name, begin_date, original_license_date, issue_date, expiration_date) %>% 
  mutate(across(ends_with("date"), ~ as.Date(., format = "%m/%d/%Y")),
         county_name = ifelse(county_name=="DeWitt", "De Witt", county_name))

# load countyfips codes

countyfips = read.csv("data/clean/texas_county_fips.csv")

# get active years of each business in the data

business_active = liquortax %>% 
  filter(year(original_license_date)<=2019) %>%  # filter out new businesses
  filter(clp != "" & clp_primary == "") %>% # remove lines where the same business has some additonal license
  group_by(clp) %>% 
  summarise(
    county = unique(county_name),
    start_year = max(year(min(original_license_date)), 2003),
    stop_year = min(year(max(expiration_date)), 2019)
  ) %>% 
  ungroup()

all_combinations <- expand.grid(
  county = unique(countyfips$county),
  year = seq(2003, 2019)
)

panel_data <- left_join(all_combinations, business_active, by = "county") %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), 2003, start_year),
         stop_year = ifelse(is.na(stop_year), 2019, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(county, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, year = seq(2003, 2019), fill = list(total_businesses = 0))

write.csv(panel_data, "data/clean/active-licenses-by-county.csv")
