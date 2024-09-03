
##################################################################
##                  CLEAN ALCOHOL LICENSE DATA                  ##
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
license_information = read_excel("data/clean/merged/license-categories.xlsx")

liquor_licenses = bind_rows(pre2012, post2012) %>% 
  mutate(
    city = tolower(city),
    city = str_remove_all(city, "village of | city| village| town"),
  ) %>% 
  left_join(., license_information, by = "license_type")

# Three pairs of municipalities share the same name: Lakeside, Oak Ridge, and Reno
# will modify city name to reflect this
# Also resolve duplicate cities with different spellings

duplicate_cities = c("lakeside", "oak ridge", "reno")

liquor_licenses = liquor_licenses %>%
  mutate(
    city = ifelse(city == "lake side", "lakeside", city),
    city = ifelse(city %in% duplicate_cities, str_c(city, " (", county, ")"), city),
  )

write.csv(liquor_licenses, "data/clean/liquor-licenses/all-liquor-licenses.csv", row.names = F)

# Aim: get active years of each license in the data
# Include flags for start and stop
# Note: panel at the license level, not the business level

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
