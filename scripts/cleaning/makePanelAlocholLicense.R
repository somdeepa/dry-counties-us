
##################################################################
##            MAKE PANEL OF ACTIVE ALOCOHOL LICENSES            ##
##################################################################

# Data clarifications from TABC
# 
# Status date is the date when the license status changed.  See below for different scenarios.
# 
# Expired date=Status date when License Status=Expired, otherwise, it was manually changed due to License Status was Vol Canceled/Vol Suspense... before License Status changed to Expired.
# 
# The logic is looking for license data not issued nor renewals after 2012 and some of these are (about less than 5000) license data with expiry/status year in 2013/2014 due to their applications pending until manually expired. 


##---------------------------------------------------------------
##              Combine pre 2012 and post 2012 data             -
##---------------------------------------------------------------


pre2012 = read_excel("data/raw/liquor license/Retailers_not_Issued_since_2012.xlsx") %>% 
  filter(ll_stateCode == "TX") %>% 
  select(rank, licNumber, Clp, licStatusDesc, ll_countyName, ll_cityName, ll_zip,
         ll_addressLine1, originalLicenseDate, expiryDate,
         tradeName, owner) %>% 
  rename(
    license_type = rank,
    license_number = licNumber,
    combined_number = Clp,
    license_status = licStatusDesc,
    county = ll_countyName,
    city = ll_cityName,
    zip = ll_zip,
    address = ll_addressLine1,
    trade_name = tradeName,
    original_issue_date = originalLicenseDate,
    expiration_date = expiryDate,
  ) %>% 
  mutate(
    across(ends_with("Date"), ~ as.Date(., format = "%m/%d/%Y")),
    county = ifelse(county=="DeWitt", "De Witt", county),
    zip = str_sub(zip, 1, 5)
  )

post2012 = read.csv("data/raw/liquor license/TABC_License_Information_20240730.csv") %>% 
  filter(State == "TX") %>% 
  select(License.Type, License.ID, License.Status, City, County, Zip,
         Original.Issue.Date, Expiration.Date,
         Trade.Name, Owner, Address, Legacy.CLP) %>% 
  rename(
    license_type = License.Type,
    license_number = License.ID,
    combined_number = Legacy.CLP,
    license_status = License.Status,
    county = County,
    city = City,
    zip = Zip,
    address = Address,
    trade_name = Trade.Name,
    owner = Owner,
    original_issue_date = Original.Issue.Date,
    expiration_date = Expiration.Date
  ) %>% 
  mutate(
    across(ends_with("date"), ~ as.Date(., format = "%m/%d/%Y")),
    county = ifelse(county=="DeWitt", "De Witt", county),
    zip = str_sub(zip, 1, 5))


# combine pre and post 2012 data, merge license categories

# merge license categories
license_information = read_excel("data/clean/merged/license-election-issues.xlsx", sheet = 2)

liquor_licenses = bind_rows(pre2012, post2012) %>% 
  mutate(
    city = tolower(city)
  ) %>% 
  left_join(., license_information, by = "license_type")

# Three pairs of municipalities share the same name: Lakeside, Oak Ridge, and Reno
# will modify city name to reflect this

duplicate_cities = c("lakeside", "oak ridge", "reno")

liquor_licenses = liquor_licenses %>%
  mutate(
    city = ifelse(city == "lake side", "lakeside", city),
    city = ifelse(city %in% duplicate_cities, str_c(city, " (", county, ")"), city),
  )


write.csv(liquor_licenses, "data/clean/liquor-licenses/all-liquor-licenses.csv", row.names = F)
liquor_licenses = read.csv("data/clean/liquor-licenses/all-liquor-licenses.csv")
# get active years of each license in the data
# Note: panel at the license level, not the business level


##---------------------------------------------------------------
##                        MAKE PANELS!!!!                       -
##---------------------------------------------------------------


startYear = 1990 # starting year of Panel
endYear = 2019 # Ending year of panel
years = seq(startYear, endYear)

startQuarter = 1 # 1st quarter of 1990
endQuarter = (endYear-startYear)*4
quarters = seq(startQuarter, endQuarter)

# function to convert a date to a quarter
convert_to_quarter <- function(date) {
  
  year = year(date)
  month = month(date)

  quarter = ceiling(month / 3)
  total_quarters = (max(year,startYear) - startYear) * 4 + quarter
  
  return(total_quarters)
}

business_active = liquor_licenses %>% 
  filter(year(original_issue_date)<=endYear) %>%  # filter out new businesses
  group_by(combined_number) %>% 
  summarise(
    city = unique(city),
    county = unique(county),
    license_type = unique(license_type),
    category = unique(category),
    category_detail = unique(category_detail),
    start_year = max(year(min(original_issue_date)), startYear),
    stop_year = min(year(max(expiration_date)), endYear),
    start_quarter = max(min(sapply(original_issue_date, convert_to_quarter)), startQuarter),
    stop_quarter = min(max(sapply(expiration_date, convert_to_quarter)), endQuarter)
  ) %>% 
  ungroup()

write.csv(business_active, "data/clean/liquor-licenses/all-licenses-1990-2019.csv", row.names = F)


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

## BY CATEGORy_DETAIL

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

# CITY LEVEL

## ALL CATEGORIES

all_combinations = expand.grid(
  city = unique(business_active$city),
  quarter = quarters
) 

# make panel by city
city_panel_all_licenses = left_join(all_combinations, business_active, by = "city") %>% 
  mutate(start_quarter = ifelse(is.na(start_quarter), startQuarter, start_quarter),
         stop_quarter = ifelse(is.na(stop_quarter), endQuarter, stop_quarter),
         # Create a flag for active businesses
         active = between(quarter, start_quarter, stop_quarter)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and quarter and count businesses
  group_by(city, quarter) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, quarter = seq(startQuarter, endQuarter), fill = list(total_businesses = 0) ) %>% 
  mutate(
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(city_panel_all_licenses, "data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv", row.names = F)

## BY OFF/ON

all_combinations = expand.grid(
  city = unique(business_active$city),
  category = unique(business_active$category),
  quarter = quarters
) %>% 
  mutate(
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

city_panel_licenses_types = left_join(all_combinations, business_active, by = c("city", "category")) %>%
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
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(city_panel_licenses_types, "data/clean/liquor-licenses/quarterly_city_panel_off_on.csv", row.names = F)

## BY CATEGORy_DETAIL
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
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(city_panel_licenses_types, "data/clean/liquor-licenses/quarterly_city_panel_categories.csv", row.names = F)



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
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
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
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
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
    year = startYear + floor(quarter/4),
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(county_panel_licenses_types, "data/clean/liquor-licenses/quarterly_county_panel_categories", row.names = F)


##-----------
##  BY YEAR  
##-----------

# CITY LEVEL

## ALL CATEGORIES

all_combinations = expand.grid(
  city = unique(business_active$city),
  year = years
)

# make panel by city
city_panel_all_licenses = left_join(all_combinations, business_active, by = "city") %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), startYear, start_year),
         stop_year = ifelse(is.na(stop_year), endYear, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(city, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, year = seq(startYear, endYear), fill = list(total_businesses = 0)) %>% 
  mutate(
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(city_panel_all_licenses, "data/clean/liquor-licenses/city_panel_all_licenses.csv", row.names = F)

## BY OFF/ON

all_combinations = expand.grid(
  city = unique(business_active$city),
  category = unique(business_active$category),
  year = years
)

city_panel_licenses_types = left_join(all_combinations, business_active, by = c("city", "category")) %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), startYear, start_year),
         stop_year = ifelse(is.na(stop_year), endYear, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(city, category, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, category, year = seq(startYear, endYear), fill = list(total_businesses = 0))  mutate(
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(city_panel_licenses_types, "data/clean/liquor-licenses/city_panel_off_on.csv", row.names = F)

## BY CATEGORy_DETAIL
all_combinations = expand.grid(
  city = unique(business_active$city),
  category_detail = unique(business_active$category_detail),
  year = years
)

city_panel_licenses_types = left_join(all_combinations, business_active, by = c("city", "category_detail")) %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), startYear, start_year),
         stop_year = ifelse(is.na(stop_year), endYear, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(city, category_detail, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(city, category_detail, year = seq(startYear, endYear), fill = list(total_businesses = 0))  mutate(
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(city_panel_licenses_types, "data/clean/liquor-licenses/city_panel_categories.csv", row.names = F)



# COUNTY LEVEL

 
## ALL CATEGORIES

countynames = read.csv("data/clean/fips codes/texas_county_fips.csv")

all_combinations = expand.grid(
  county = countynames$county,
  year = years
)

county_panel_all_licenses = left_join(all_combinations, business_active, by = "county") %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), startYear, start_year),
         stop_year = ifelse(is.na(stop_year), endYear, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(county, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, year = seq(startYear, endYear), fill = list(total_businesses = 0))  mutate(
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(county_panel_all_licenses, "data/clean/liquor-licenses/county_panel_all_licenses", row.names = F)

## BY OFF/ON

all_combinations = expand.grid(
  county = countynames$county,
  category = unique(business_active$category),
  year = years
)

county_panel_licenses_types = left_join(all_combinations, business_active, by = c("county", "category")) %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), startYear, start_year),
         stop_year = ifelse(is.na(stop_year), endYear, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(county, license_type, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, category, year = seq(startYear, endYear), fill = list(total_businesses = 0))  mutate(
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(county_panel_licenses_types, "data/clean/liquor-licenses/county_panel_off_on", row.names = F)

## BY CATEGORy_DETAIL

all_combinations = expand.grid(
  county = countynames$county,
  license_type = unique(business_active$license_type),
  year = years
)

county_panel_licenses_types = left_join(all_combinations, business_active, by = c("county", "category_detail")) %>%
  # Fill missing start_year and stop_year with 2003 and 2019 respectively
  mutate(start_year = ifelse(is.na(start_year), startYear, start_year),
         stop_year = ifelse(is.na(stop_year), endYear, stop_year),
         # Create a flag for active businesses
         active = between(year, start_year, stop_year)) %>%
  # Filter active businesses
  filter(active) %>%
  # Group by county and year and count businesses
  group_by(county, category_detail, year) %>%
  summarise(total_businesses = n(), .groups = 'drop') %>%
  # Fill in missing combinations with 0
  complete(county, category_detail, year = seq(startYear, endYear), fill = list(total_businesses = 0))  mutate(
    # make an adjusted year to merge with decennial population
    adjusted_year = round(year/10)*10
  )

write.csv(county_panel_licenses_types, "data/clean/liquor-licenses/county_panel_categories", row.names = F)
