
#####################################################################################
##  MAKE TIME-SERIES AND PANEL FOR LIQUOR LICENSES (BY CITY/COUNTY, QUARTER/YEAR)  ##
#####################################################################################

business_active = read.csv("data/clean/liquor-licenses/all-licenses-1990-2019.csv")

##---------------
##  TIME SERIES  
##---------------

## ALL CATEGORIES
count_active_businesses <- function(year, data) {
  data %>%
    filter(start_year <= year & stop_year >= year) %>%
    nrow()
}

businesses_by_year = data.frame(
  year = years,
  total_businesses = unlist(lapply(years, function(x) count_active_businesses(x, business_active)))
)

write.csv(businesses_by_year, "data/clean/liquor-licenses/total_businesses_by_year_1990_2019.csv", row.names = F)

## BY OFF/ON

all_combinations = expand.grid(
  category = unique(business_active$category),
  year = seq(startYear, endYear)
)

count_active_businesses_category = function(year, data) {
  data %>%
    filter(start_year <= year & stop_year >= year & category == category) %>%
    group_by(category) %>% 
    summarise(total_businesses = n()) %>% 
    mutate(year = year)
}

businesses_by_year_category = do.call(rbind, lapply(years, function(x) count_active_businesses_category(x, business_active)))

write.csv(businesses_by_year_category, "data/clean/liquor-licenses/total_businesses_by_year_category_1990_2019.csv", row.names = F)

## BY CATEGORY_DETAIL

all_combinations = expand.grid(
  license_type = unique(business_active$category_detail),
  year = seq(startYear, endYear)
)

count_active_businesses_category = function(year, data) {
  data %>%
    filter(start_year <= year & stop_year >= year & category_detail == category_detail) %>%
    group_by(category_detail) %>% 
    summarise(total_businesses = n()) %>% 
    mutate(year = year)
}

businesses_by_year_category_detail = do.call(rbind, lapply(years, function(x) count_active_businesses_category(x, business_active)))

write.csv(businesses_by_year_category_detail, "data/clean/liquor-licenses/total_businesses_by_year_category_detail_1990_2019.csv", row.names = F)

##--------------
##  BY QUARTER  
##--------------

## NOTE: including a line of code to filter out all non-retail licenses, as well
# as "private-club' licenses. Comment out to generate panels with all licenses

business_active_retail = business_active %>% 
  filter(!is.na(category) & category_detail!="Private Club")

# CITY LEVEL

## ALL CATEGORIES

all_combinations = expand.grid(
  city = unique(business_active_retail$city),
  quarter = quarters
) 

# make panel by city
city_panel_all_licenses = left_join(all_combinations, business_active_retail, by = "city") %>% 
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>% 
  # Filter active businesses
  filter(active) %>% 
  # Group by city and quarter and count businesses
  group_by(city, quarter) %>% 
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0) ) %>% 
  mutate(
    year = startYear + floor(quarter/4)
  )

write.csv(city_panel_all_licenses, "data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv", row.names = F)

## BY OFF/ON

business_active_retail = business_active %>% 
  filter(!is.na(category))

all_combinations = expand.grid(
  city = unique(business_active_retail$city),
  category = unique(business_active_retail$category),
  quarter = quarters
) 

city_panel_licenses_types = left_join(all_combinations, business_active_retail, by = c("city", "category")) %>% 
  # some NAs get created (why I do not fully understand), filter them out
  filter(!is.na(combined_number)) %>% 
  # Fill missing start_quarter and stop_quarter with 2003 and 2019 respectively
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>% 
  # Filter active businesses
  filter(active) %>% 
  # Group by county and quarter and count businesses
  group_by(city, category, quarter) %>% 
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, category, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0)) %>% 
  mutate(
    year = startYear + floor(quarter/4)
  )

write.csv(city_panel_licenses_types, "data/clean/liquor-licenses/quarterly_city_panel_off_on.csv", row.names = F)

## BY CATEGORY

all_combinations = expand.grid(
  city = unique(business_active$city),
  category_detail = unique(business_active$category_detail),
  quarter = quarters
)

city_panel_licenses_types = left_join(all_combinations, business_active, by = c("city", "category_detail")) %>%
  # Fill missing start_quarter and stop_quarter with 2003 and 2019 respectively
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and quarter and count businesses
  group_by(city, category_detail, quarter) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, category_detail, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0)) %>% 
  mutate(
    year = startYear + floor(quarter/4)
  )

write.csv(city_panel_licenses_types, "data/clean/liquor-licenses/quarterly_city_panel_categories.csv", row.names = F)

# NOTE : ALL CODE FROM HERE SHOULD BE UPDATED 

# COUNTY LEVEL


## ALL CATEGORIES

countynames = read.csv("data/clean/fips codes/texas_county_fips.csv")

all_combinations = expand.grid(
  county = countynames$county,
  quarter = quarters
)

county_panel_all_licenses = left_join(all_combinations, business_active, by = "county") %>%
  # Fill missing start_quarter and stop_quarter with 2003 and 2019 respectively
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and quarter and count businesses
  group_by(county, quarter) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0)) %>% 
  mutate(
    year = startYear + floor(quarter/4)
  )

write.csv(county_panel_all_licenses, "data/clean/liquor-licenses/quarterly_county_panel_all_licenses", row.names = F)

## BY OFF/ON

all_combinations = expand.grid(
  county = countynames$county,
  category = unique(business_active$category),
  quarter = quarters
)

county_panel_licenses_types = left_join(all_combinations, business_active, by = c("county", "category")) %>%
  # Fill missing start_quarter and stop_quarter with 2003 and 2019 respectively
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and quarter and count businesses
  group_by(county, license_type, quarter) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, category, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0)) %>% 
  mutate(
    year = startYear + floor(quarter/4)
  )

write.csv(county_panel_licenses_types, "data/clean/liquor-licenses/quarterly_county_panel_off_on", row.names = F)

## BY CATEGORy_DETAIL

all_combinations = expand.grid(
  county = countynames$county,
  license_type = unique(business_active$license_type),
  quarter = quarters
)

county_panel_licenses_types = left_join(all_combinations, business_active, by = c("county", "category_detail")) %>%
  # Fill missing start_quarter and stop_quarter with 2003 and 2019 respectively
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and quarter and count businesses
  group_by(county, category_detail, quarter) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, category_detail, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0)) %>% 
  mutate(
    year = startYear + floor(quarter/4)
  )

write.csv(county_panel_licenses_types, "data/clean/liquor-licenses/quarterly_county_panel_categories", row.names = F)